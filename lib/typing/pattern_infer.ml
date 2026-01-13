(** Pattern type inference.

    This module handles type inference for patterns, including:
    - Variable patterns
    - Wildcard patterns
    - Constant patterns
    - Tuple patterns
    - Constructor patterns
    - Record patterns (with punning support)
    - Alias patterns
    - Constraint patterns

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable]. *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Type of constant literals *)
let type_of_constant = Inference_utils.type_of_constant

(** Unify types with context-based alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** [infer_pattern ctx pattern] infers the type of a pattern.

    @param ctx The typing context (used for environment threading)
    @param pattern The pattern to infer
    @return A tuple [(typed_pattern, pattern_type, updated_ctx)] where
            [typed_pattern] is the typed pattern, [pattern_type] is its type,
            and [updated_ctx] includes any bindings introduced by the pattern *)
let rec infer_pattern ctx (pattern : pattern) =
  let loc = pattern.Location.location in
  match pattern.Location.value with
  | PatternVariable name ->
    let ty, ctx = Typing_context.new_type_variable ctx in
    let id = Identifier.create name in
    let env = Typing_context.environment ctx in
    let env = Environment.add_value name id (trivial_scheme ty) loc env in
    let ctx = Typing_context.with_environment env ctx in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternWildcard ->
    let ty, ctx = Typing_context.new_type_variable ctx in
    let typed_pattern = {
      pattern_desc = TypedPatternWildcard;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternConstant const ->
    let ty = type_of_constant const in
    let typed_pattern = {
      pattern_desc = TypedPatternConstant const;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternTuple patterns ->
    let typed_patterns, types, ctx =
      List.fold_left (fun (pats, tys, ctx) p ->
        let pat, ty, ctx = infer_pattern ctx p in
        (pat :: pats, ty :: tys, ctx)
      ) ([], [], ctx) patterns
    in
    let typed_patterns = List.rev typed_patterns in
    let types = List.rev types in
    let ty = TypeTuple types in
    let typed_pattern = {
      pattern_desc = TypedPatternTuple typed_patterns;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternConstructor (name, arg_pattern) ->
    let env = Typing_context.environment ctx in
    begin match Environment.find_constructor name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound constructor: %s" name)
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        Inference_utils.instantiate_constructor_with_ctx ctx constructor_info
      in
      let typed_arg, ctx = match arg_pattern, expected_arg_ty with
        | None, None -> (None, ctx)
        | Some p, Some expected_ty ->
          let typed_p, actual_ty, ctx = infer_pattern ctx p in
          unify ctx loc expected_ty actual_ty;
          (Some typed_p, ctx)
        | Some _, None ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s does not take an argument" name)
        | None, Some _ ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s requires an argument" name)
      in
      let typed_pattern = {
        pattern_desc = TypedPatternConstructor (constructor_info, typed_arg);
        pattern_type = result_ty;
        pattern_location = loc;
      } in
      (typed_pattern, result_ty, ctx)
    end

  | PatternAlias (inner_pattern, name) ->
    (* Note: typed_inner is discarded because TypedPatternAlias doesn't exist yet.
       The alias pattern binds to a variable, losing the inner pattern structure. *)
    let _typed_inner, ty, ctx = infer_pattern ctx inner_pattern in
    let id = Identifier.create name in
    let env = Typing_context.environment ctx in
    let env = Environment.add_value name id (trivial_scheme ty) loc env in
    let ctx = Typing_context.with_environment env ctx in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternConstraint (inner_pattern, type_expr) ->
    (* Infer type of inner pattern *)
    let typed_inner, inferred_ty, ctx = infer_pattern ctx inner_pattern in
    (* Convert the type annotation to a type *)
    let annotated_ty, ctx = Module_type_check.check_type_expression ctx type_expr in
    (* Unify the inferred type with the annotation *)
    unify ctx loc inferred_ty annotated_ty;
    (* Return the typed pattern with the annotated type *)
    let typed_pattern = {
      pattern_desc = typed_inner.pattern_desc;
      pattern_type = annotated_ty;
      pattern_location = loc;
    } in
    (typed_pattern, annotated_ty, ctx)

  | PatternRecord (pattern_fields, is_open) ->
    (* Infer types for each field pattern and collect bindings *)
    let typed_field_patterns, field_types, ctx =
      List.fold_left (fun (typed_fields_accumulator, field_types_accumulator, ctx) pattern_field ->
        let field_name = pattern_field.pattern_field_name.Location.value in
        let inner_pattern = match pattern_field.pattern_field_pattern with
          | Some pattern -> pattern
          | None ->
            (* Punning: { x } means { x = x } - create a variable pattern *)
            Location.{ value = PatternVariable field_name;
                       location = pattern_field.pattern_field_name.Location.location }
        in
        let typed_inner_pattern, inner_pattern_type, ctx = infer_pattern ctx inner_pattern in
        let typed_field = {
          Typed_tree.typed_pattern_field_name = field_name;
          typed_pattern_field_pattern = typed_inner_pattern;
        } in
        (typed_field :: typed_fields_accumulator,
         (field_name, RowFieldPresent inner_pattern_type) :: field_types_accumulator,
         ctx)
      ) ([], [], ctx) pattern_fields
    in
    let typed_field_patterns = List.rev typed_field_patterns in
    let field_types = List.rev field_types in
    let record_type, ctx =
      if is_open then
        let row_var, ctx = Typing_context.new_type_variable ctx in
        (Types.type_record_open field_types ~row_var, ctx)
      else
        (Types.type_record_closed field_types, ctx)
    in
    let typed_pattern = {
      pattern_desc = TypedPatternRecord (typed_field_patterns, is_open);
      pattern_type = record_type;
      pattern_location = loc;
    } in
    (typed_pattern, record_type, ctx)

  | PatternLocallyAbstract type_name ->
    (* Locally abstract type: (type a) introduces a scoped rigid type variable.
       The pattern matches nothing (has type unit), but adds a type alias
       to the environment that maps the name to a rigid type variable.

       Rigid type variables don't unify globally - instead, GADT pattern
       matching extracts equations on them that are applied locally within
       match branches. This enables the classic GADT eval function to work. *)
    let abstract_id = Identifier.create type_name in
    (* Create a rigid type variable for this locally abstract type *)
    let type_var, ctx = Typing_context.new_rigid_type_variable ctx in
    (* Create a type declaration that maps the name to this type variable *)
    let type_decl = {
      declaration_name = type_name;
      declaration_parameters = [];
      declaration_variances = [];
      declaration_manifest = Some type_var;  (* Alias to the type variable *)
      declaration_kind = DeclarationAbstract;
      declaration_private = false;
      declaration_constraints = [];
    } in
    (* Add the type alias to the environment *)
    let env = Typing_context.environment ctx in
    let env = Environment.add_type type_name type_decl env in
    let ctx = Typing_context.with_environment env ctx in
    (* The pattern has type unit - it doesn't match a value *)
    let typed_pattern = {
      pattern_desc = TypedPatternLocallyAbstract (abstract_id, type_decl);
      pattern_type = type_unit;
      pattern_location = loc;
    } in
    (typed_pattern, type_unit, ctx)

  | PatternPolyVariant (tag, arg_pattern) ->
    (* Polymorphic variant pattern: `Tag or `Tag pat
       Creates an open poly variant type that can be unified with the scrutinee.
       Pattern type is [> `Tag] or [> `Tag of ty] - same as expression type. *)
    let typed_arg, arg_ty_opt, ctx = match arg_pattern with
      | None -> (None, None, ctx)
      | Some p ->
        let typed_p, ty, ctx = infer_pattern ctx p in
        (Some typed_p, Some ty, ctx)
    in
    (* Create a row variable for the open poly variant type *)
    let row_var, ctx = Typing_context.new_type_variable ctx in
    let pv_field = Types.PVFieldPresent arg_ty_opt in
    let pv_type = Types.type_poly_variant_at_least [(tag, pv_field)] ~row_var in
    let typed_pattern = {
      pattern_desc = TypedPatternPolyVariant (tag, typed_arg);
      pattern_type = pv_type;
      pattern_location = loc;
    } in
    (typed_pattern, pv_type, ctx)

  | PatternError error_info ->
    (* Error patterns get a fresh type variable and are preserved in typed tree *)
    let error_ty, ctx = Typing_context.new_type_variable ctx in
    let typed_pattern = {
      pattern_desc = TypedPatternError error_info;
      pattern_type = error_ty;
      pattern_location = loc;
    } in
    (typed_pattern, error_ty, ctx)
