(** Pattern type inference.

    This module handles type inference for patterns, including:
    - Variable patterns
    - Wildcard patterns
    - Constant patterns
    - Tuple patterns
    - Constructor patterns
    - Record patterns (with punning support)
    - Alias patterns
    - Constraint patterns *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Type of constant literals *)
let type_of_constant = Inference_utils.type_of_constant

(** Unify types with alias expansion support. *)
let unify = Inference_utils.unify_with_env

(** [infer_pattern env pattern] infers the type of a pattern.

    @param env The typing environment
    @param pattern The pattern to infer
    @return A triple [(typed_pattern, pattern_type, updated_env)] where
            [typed_pattern] is the typed pattern, [pattern_type] is its type,
            and [updated_env] includes any bindings introduced by the pattern *)
let rec infer_pattern env (pattern : pattern) =
  let loc = pattern.Location.location in
  match pattern.Location.value with
  | PatternVariable name ->
    let ty = new_type_variable () in
    let id = Identifier.create name in
    let env = Environment.add_value name id (trivial_scheme ty) env in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternWildcard ->
    let ty = new_type_variable () in
    let typed_pattern = {
      pattern_desc = TypedPatternWildcard;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstant const ->
    let ty = type_of_constant const in
    let typed_pattern = {
      pattern_desc = TypedPatternConstant const;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternTuple patterns ->
    let typed_patterns, types, env =
      List.fold_left (fun (pats, tys, env) p ->
        let pat, ty, env = infer_pattern env p in
        (pat :: pats, ty :: tys, env)
      ) ([], [], env) patterns
    in
    let typed_patterns = List.rev typed_patterns in
    let types = List.rev types in
    let ty = TypeTuple types in
    let typed_pattern = {
      pattern_desc = TypedPatternTuple typed_patterns;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstructor (name, arg_pattern) ->
    begin match Environment.find_constructor name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound constructor: %s" name)
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        Type_utils.instantiate_constructor constructor_info
      in
      let typed_arg, env = match arg_pattern, expected_arg_ty with
        | None, None -> (None, env)
        | Some p, Some expected_ty ->
          let typed_p, actual_ty, env = infer_pattern env p in
          unify env loc expected_ty actual_ty;
          (Some typed_p, env)
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
      (typed_pattern, result_ty, env)
    end

  | PatternAlias (inner_pattern, name) ->
    (* Note: typed_inner is discarded because TypedPatternAlias doesn't exist yet.
       The alias pattern binds to a variable, losing the inner pattern structure. *)
    let _typed_inner, ty, env = infer_pattern env inner_pattern in
    let id = Identifier.create name in
    let env = Environment.add_value name id (trivial_scheme ty) env in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstraint (inner_pattern, _type_expr) ->
    infer_pattern env inner_pattern

  | PatternRecord (pattern_fields, is_open) ->
    (* Infer types for each field pattern and collect bindings *)
    let typed_field_patterns, field_types, updated_environment =
      List.fold_left (fun (typed_fields_accumulator, field_types_accumulator, current_environment) pattern_field ->
        let field_name = pattern_field.pattern_field_name.Location.value in
        let inner_pattern = match pattern_field.pattern_field_pattern with
          | Some pattern -> pattern
          | None ->
            (* Punning: { x } means { x = x } - create a variable pattern *)
            Location.{ value = PatternVariable field_name;
                       location = pattern_field.pattern_field_name.Location.location }
        in
        let typed_inner_pattern, inner_pattern_type, updated_env = infer_pattern current_environment inner_pattern in
        let typed_field = {
          Typed_tree.typed_pattern_field_name = field_name;
          typed_pattern_field_pattern = typed_inner_pattern;
        } in
        (typed_field :: typed_fields_accumulator,
         (field_name, RowFieldPresent inner_pattern_type) :: field_types_accumulator,
         updated_env)
      ) ([], [], env) pattern_fields
    in
    let typed_field_patterns = List.rev typed_field_patterns in
    let field_types = List.sort compare (List.rev field_types) in
    let row_more = if is_open then new_type_variable () else TypeRowEmpty in
    let record_type = TypeRecord {
      row_fields = field_types;
      row_more;
    } in
    let typed_pattern = {
      pattern_desc = TypedPatternRecord (typed_field_patterns, is_open);
      pattern_type = record_type;
      pattern_location = loc;
    } in
    (typed_pattern, record_type, updated_environment)
