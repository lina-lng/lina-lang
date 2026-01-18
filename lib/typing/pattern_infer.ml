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

(** Collect all variable names bound in a pattern (for linearity checking).
    Returns a list of (name, location) pairs. *)
let rec collect_bound_names (pattern : pattern) : (string * Location.t) list =
  let loc = pattern.Location.location in
  match pattern.Location.value with
  | PatternVariable name -> [(name, loc)]
  | PatternWildcard -> []
  | PatternConstant _ -> []
  | PatternTuple patterns ->
      List.concat_map collect_bound_names patterns
  | PatternConstructor (_, Some arg) ->
      collect_bound_names arg
  | PatternConstructor (_, None) -> []
  | PatternAlias (inner, name) ->
      (name, loc) :: collect_bound_names inner
  | PatternOr (left, _right) ->
      (* For or-patterns, both sides must bind the same names.
         We only need to collect from one side since they must match. *)
      collect_bound_names left
  | PatternConstraint (inner, _) ->
      collect_bound_names inner
  | PatternRecord (fields, _) ->
      List.concat_map (fun field ->
        match field.pattern_field_pattern with
        | Some pat -> collect_bound_names pat
        | None ->
            (* Punning: { x } binds x *)
            let name = field.pattern_field_name.Location.value in
            [(name, field.pattern_field_name.Location.location)]
      ) fields
  | PatternLocallyAbstract _ -> []
  | PatternPolyVariant (_, Some arg) ->
      collect_bound_names arg
  | PatternPolyVariant (_, None) -> []
  | PatternError _ -> []

(** Check that a pattern is linear (no variable bound more than once).
    Raises a type error if any variable is bound multiple times. *)
let check_pattern_linearity (pattern : pattern) : unit =
  let bindings = collect_bound_names pattern in
  let rec check_duplicates seen = function
    | [] -> ()
    | (name, loc) :: rest ->
        if List.mem_assoc name seen then begin
          let first_loc = List.assoc name seen in
          Compiler_error.type_error loc
            (Printf.sprintf "The variable `%s` is bound more than once in this pattern.\n\n\
                             It was first bound at line %d. Each variable can only be bound once \
                             in a pattern to avoid ambiguity about which value to use."
               name first_loc.Location.start_pos.line)
        end else
          check_duplicates ((name, loc) :: seen) rest
  in
  check_duplicates [] bindings

(** Unify types with context-based alias expansion. *)
let unify = Inference_utils.unify

(** Substitute identifiers in a typed pattern based on a name -> identifier mapping.
    Used for or-patterns to ensure both branches use the same identifiers. *)
let rec substitute_pattern_identifiers (mapping : (string * Identifier.t) list) (pat : typed_pattern) : typed_pattern =
  let new_desc = match pat.pattern_desc with
    | TypedPatternVariable id ->
        let name = Identifier.name id in
        begin match List.assoc_opt name mapping with
        | Some new_id -> TypedPatternVariable new_id
        | None -> pat.pattern_desc
        end
    | TypedPatternWildcard -> pat.pattern_desc
    | TypedPatternConstant _ -> pat.pattern_desc
    | TypedPatternTuple pats ->
        TypedPatternTuple (List.map (substitute_pattern_identifiers mapping) pats)
    | TypedPatternConstructor (info, arg) ->
        TypedPatternConstructor (info, Option.map (substitute_pattern_identifiers mapping) arg)
    | TypedPatternRecord (fields, is_open) ->
        let new_fields = List.map (fun (field : typed_record_pattern_field) ->
          { field with typed_pattern_field_pattern = substitute_pattern_identifiers mapping field.typed_pattern_field_pattern }
        ) fields in
        TypedPatternRecord (new_fields, is_open)
    | TypedPatternAlias (inner, id) ->
        let name = Identifier.name id in
        let new_id = match List.assoc_opt name mapping with
          | Some mapped_id -> mapped_id
          | None -> id
        in
        TypedPatternAlias (substitute_pattern_identifiers mapping inner, new_id)
    | TypedPatternOr (left, right) ->
        TypedPatternOr (substitute_pattern_identifiers mapping left, substitute_pattern_identifiers mapping right)
    | TypedPatternLocallyAbstract _ -> pat.pattern_desc
    | TypedPatternPolyVariant (tag, arg) ->
        TypedPatternPolyVariant (tag, Option.map (substitute_pattern_identifiers mapping) arg)
    | TypedPatternError _ -> pat.pattern_desc
  in
  { pat with pattern_desc = new_desc }

(** [infer_pattern ctx pattern] infers the type of a pattern.

    @param ctx The typing context (used for environment threading)
    @param pattern The pattern to infer
    @return A tuple [(typed_pattern, pattern_type, updated_ctx)] where
            [typed_pattern] is the typed pattern, [pattern_type] is its type,
            and [updated_ctx] includes any bindings introduced by the pattern *)
let rec infer_pattern ctx (pattern : pattern) =
  (* Check pattern linearity before inference *)
  check_pattern_linearity pattern;
  infer_pattern_impl ctx pattern

(** Internal pattern inference - called after linearity check *)
and infer_pattern_impl ctx (pattern : pattern) =
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
    let typed_patterns_rev, types_rev, ctx =
      List.fold_left (fun (pats, tys, ctx) p ->
        let pat, ty, ctx = infer_pattern ctx p in
        (pat :: pats, ty :: tys, ctx)
      ) ([], [], ctx) patterns
    in

    let typed_patterns = List.rev typed_patterns_rev in
    let types = List.rev types_rev in
    let ty = TypeTuple types in

    let typed_pattern = {
      pattern_desc = TypedPatternTuple typed_patterns;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternConstructor (longident, arg_pattern) ->
    let result = Inference_utils.lookup_constructor_longident ctx loc longident in
    (* Note: No private type check for patterns - can match on private types *)
    let ctor_name = result.constructor_info.Types.constructor_name in
    Inference_utils.check_constructor_arity loc ctor_name
      ~has_arg:(Option.is_some arg_pattern)
      ~expects_arg:(Option.is_some result.expected_arg_type);
    let typed_arg, ctx = match arg_pattern, result.expected_arg_type with
      | None, None -> (None, ctx)
      | Some pat, Some expected_ty ->
        let typed_pat, actual_ty, ctx = infer_pattern ctx pat in
        unify ctx loc expected_ty actual_ty;
        (Some typed_pat, ctx)
      | Some _, None | None, Some _ ->
        (* Constructor arity was already checked by check_constructor_arity *)
        Compiler_error.internal_error
          "Constructor arity mismatch after arity check"
    in
    let typed_pattern = {
      pattern_desc = TypedPatternConstructor (result.constructor_info, typed_arg);
      pattern_type = result.result_type;
      pattern_location = loc;
    } in
    (typed_pattern, result.result_type, ctx)

  | PatternAlias (inner_pattern, name) ->
    (* Alias pattern: (p as x) matches p and also binds x to the whole value.
       The inner pattern is preserved for proper pattern matching. *)
    let typed_inner, ty, ctx = infer_pattern ctx inner_pattern in
    let id = Identifier.create name in
    let env = Typing_context.environment ctx in
    let env = Environment.add_value name id (trivial_scheme ty) loc env in
    let ctx = Typing_context.with_environment env ctx in
    let typed_pattern = {
      pattern_desc = TypedPatternAlias (typed_inner, id);
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, ctx)

  | PatternOr (pattern_left, pattern_right) ->
    (* Or-pattern: (p1 | p2) matches if either pattern matches.
       Both patterns must:
       1. Have the same type
       2. Bind the same variables with compatible types *)
    let original_env = Typing_context.environment ctx in

    (* Infer left pattern, collecting its bindings *)
    let typed_left, type_left, ctx_left = infer_pattern ctx pattern_left in
    let env_left = Typing_context.environment ctx_left in

    (* Reset environment and infer right pattern *)
    let ctx_for_right = Typing_context.with_environment original_env ctx_left in
    let typed_right, type_right, ctx_right = infer_pattern ctx_for_right pattern_right in
    let env_right = Typing_context.environment ctx_right in

    (* Unify the two pattern types *)
    unify ctx loc type_left type_right;

    (* Check that both patterns bind the same variables with compatible types *)
    let bindings_left = Environment.get_value_bindings env_left in
    let bindings_right = Environment.get_value_bindings env_right in
    let original_bindings = Environment.get_value_bindings original_env in

    (* Get only the NEW bindings from each branch *)
    let new_in_left = List.filter (fun (name, _, _) ->
      not (List.exists (fun (n, _, _) -> n = name) original_bindings)
    ) bindings_left in
    let new_in_right = List.filter (fun (name, _, _) ->
      not (List.exists (fun (n, _, _) -> n = name) original_bindings)
    ) bindings_right in

    (* Check that left has all variables that right has *)
    List.iter (fun (name, _, _) ->
      if not (List.exists (fun (n, _, _) -> n = name) new_in_left) then
        Compiler_error.type_error loc
          (Printf.sprintf "The variable `%s` is bound on the right side of this or-pattern \
                           but not on the left side.\n\n\
                           Both sides of an or-pattern (p1 | p2) must bind exactly the same \
                           variables so that the code after the match can use them safely."
             name)
    ) new_in_right;

    (* Check that right has all variables that left has, and unify their types.
       Also build a mapping from right identifiers to left identifiers for substitution. *)
    let id_mapping = List.filter_map (fun (name, id_left, scheme_left) ->
      match List.find_opt (fun (n, _, _) -> n = name) new_in_right with
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "The variable `%s` is bound on the left side of this or-pattern \
                           but not on the right side.\n\n\
                           Both sides of an or-pattern (p1 | p2) must bind exactly the same \
                           variables so that the code after the match can use them safely."
             name)
      | Some (_, id_right, scheme_right) ->
        (* Unify the types of the variable from both branches *)
        let ty_left, _ctx = Typing_context.instantiate ctx scheme_left in
        let ty_right, _ctx = Typing_context.instantiate ctx scheme_right in
        unify ctx loc ty_left ty_right;
        (* Map right identifier to left identifier *)
        Some (Identifier.name id_right, id_left)
    ) new_in_left in

    (* Substitute right branch identifiers with left branch identifiers *)
    let typed_right = substitute_pattern_identifiers id_mapping typed_right in

    (* Use the left branch's environment for the result (variables are the same) *)
    let ctx = Typing_context.with_environment env_left ctx_right in

    let typed_pattern = {
      pattern_desc = TypedPatternOr (typed_left, typed_right);
      pattern_type = type_left;
      pattern_location = loc;
    } in
    (typed_pattern, type_left, ctx)

  | PatternConstraint (inner_pattern, type_expr) ->
    (* Infer type of inner pattern *)
    let typed_inner, inferred_ty, ctx = infer_pattern ctx inner_pattern in
    (* Convert the type annotation to a type *)
    let annotated_ty, ctx = Type_expression_check.check_type_expression ctx type_expr in
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
      declaration_injectivities = [];
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
