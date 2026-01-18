(** Record type inference.

    This module handles type inference for record expressions:
    - Record literals: [{ x = 1; y = 2 }]
    - Record field access: [r.x] (including module access disambiguation)
    - Record update: [{ r with x = 10 }]

    Extracted from expression_infer.ml to reduce complexity. *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn = Inference_utils.expression_infer_fn

(** {1 Helper Functions} *)

(** Register field locations for precise error reporting. *)
let register_field_locations fields =
  Field_locations.clear ();
  List.iter (fun field ->
    let name = field.field_name.Location.value in
    let loc = field.field_value.Location.location in
    Field_locations.add_field name loc
  ) fields

(** Unify types using context's environment for alias expansion. *)
let unify = Inference_utils.unify

(** Unify with field suggestion support.

    Catches record field errors and adds "Did you mean?" suggestions
    based on available field names in the record type. *)
let unify_with_field_suggestions ctx loc ~field_name ~record_type expected actual =
  try
    unify ctx loc expected actual
  with
  | Unification.Unification_error err ->
    let record_field_prefix = "This record doesn't have" in
    let prefix_len = String.length record_field_prefix in
    let is_record_field_error =
      String.length err.message >= prefix_len &&
      String.sub err.message 0 prefix_len = record_field_prefix
    in
    if is_record_field_error then begin
      let available_fields = Type_utils.extract_record_field_names record_type in
      let suggestion = Common.Suggestions.format_suggestions ~target:field_name ~candidates:available_fields () in
      let available_str =
        if available_fields = [] then ""
        else
          let fields_list = List.map (fun f -> Printf.sprintf "    .%s" f) available_fields in
          Printf.sprintf "\n\nIt has these accessible fields:\n%s" (String.concat "\n" fields_list)
      in
      let enhanced_msg =
        if suggestion = "" then
          Printf.sprintf "This record doesn't have a field named `%s`.%s"
            field_name available_str
        else
          Printf.sprintf "This record doesn't have a field named `%s`.\n\n%s%s"
            field_name suggestion available_str
      in
      Common.Compiler_error.type_error loc enhanced_msg
    end
    else raise (Unification.Unification_error err)

(** Infer types for a list of record fields, threading context through.
    Returns typed fields in source order and the updated context. *)
let infer_field_values ~infer_expr ctx fields =
  let typed_fields_rev, ctx =
    List.fold_left (fun (acc, ctx) field ->
      let field_name = field.field_name.Location.value in
      let typed_value, ctx = infer_expr ctx field.field_value in
      let typed_field = {
        Typed_tree.typed_field_name = field_name;
        typed_field_value = typed_value;
      } in
      (typed_field :: acc, ctx)
    ) ([], ctx) fields
  in
  (List.rev typed_fields_rev, ctx)

(** Extract a module path from nested record access expressions.

    Given [M.N.x], this extracts [["M"; "N"]] when [M] and [M.N] are modules.
    Returns [None] if the expression is not a module path.

    @param env The typing environment for module lookups
    @param expr The expression to extract from
    @return [Some path_components] if expr is a module path, [None] otherwise *)
let rec extract_module_path env expr =
  match expr.Location.value with
  | ExpressionConstructor (longident, None) ->
      (match longident.Location.value with
       | Lident name ->
           (match Environment.find_module name env with
            | Some _ -> Some [name]
            | None -> None)
       | Ldot _ -> None)
  | ExpressionRecordAccess (inner, component) ->
      (match extract_module_path env inner with
       | Some path -> Some (path @ [component])
       | None -> None)
  | _ -> None

(** {1 Record Inference Functions} *)

(** [infer_record ~infer_expr ctx loc record_fields] infers the type of a record literal.

    Checks for duplicate fields, infers types for each field value, and builds
    a closed row type from the inferred field types.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param record_fields The list of record fields
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_record ~(infer_expr : expression_infer_fn) ctx loc record_fields =
  (* Register field locations for precise error reporting *)
  register_field_locations record_fields;

  let field_names = List.map (fun rf -> rf.field_name.Location.value) record_fields in
  let rec check_duplicates seen = function
    | [] -> ()
    | name :: rest ->
        if List.mem name seen then
          Compiler_error.type_error loc
            (Printf.sprintf "The record field %s is defined several times" name)
        else
          check_duplicates (name :: seen) rest
  in
  check_duplicates [] field_names;

  let typed_record_fields, ctx = infer_field_values ~infer_expr ctx record_fields in

  let row_field_types = List.map (fun typed_field ->
    (typed_field.Typed_tree.typed_field_name,
     RowFieldPresent typed_field.typed_field_value.expression_type)
  ) typed_record_fields in
  let record_type = Types.type_record_closed row_field_types in

  ({ expression_desc = TypedExpressionRecord typed_record_fields;
     expression_type = record_type;
     expression_location = loc },
   ctx)

(** {2 Module Member Lookup Helpers} *)

let lookup_value_in_module ctx internal_path field_name sig_ loc =
  match Module_types.find_value_in_sig field_name sig_ with
  | Some val_desc ->
      let ty, ctx = Typing_context.instantiate ctx val_desc.value_type in
      Some ({
        expression_desc = TypedExpressionModuleAccess (internal_path, field_name);
        expression_type = ty;
        expression_location = loc;
      }, ctx)
  | None -> None

let lookup_constructor_in_module ctx field_name sig_ loc =
  match Module_types.find_constructor_in_sig field_name sig_ with
  | Some ctor_info ->
      let level = Typing_context.current_level ctx in
      let fresh_var () = Types.new_type_variable_at_level level in
      let arg_type_opt, result_type = Type_utils.instantiate_constructor ~fresh_var ctor_info in
      let expr_type = match arg_type_opt with
        | None -> result_type
        | Some arg_type -> Types.TypeArrow (Types.Nolabel, arg_type, result_type)
      in
      Some ({
        expression_desc = TypedExpressionConstructor (ctor_info, None);
        expression_type = expr_type;
        expression_location = loc;
      }, ctx)
  | None -> None

let lookup_member_in_module ctx internal_path field_name sig_ loc =
  match lookup_value_in_module ctx internal_path field_name sig_ loc with
  | Some value_expr -> Some value_expr
  | None -> lookup_constructor_in_module ctx field_name sig_ loc

let resolve_module_member ctx sig_ internal_path field_name path_components loc =
  match Module_types.find_module_in_sig field_name sig_ with
  | Some _ ->
      let extended_path = Types.PathDot (internal_path, field_name) in
      ({ expression_desc = TypedExpressionModuleAccess (extended_path, field_name);
         expression_type = TypeConstructor (PathBuiltin BuiltinUnit, []);
         expression_location = loc },
       ctx)
  | None ->
      match lookup_member_in_module ctx internal_path field_name sig_ loc with
      | Some member_expr -> member_expr
      | None ->
          let path_str = String.concat "." path_components in
          Compiler_error.type_error loc
            (Printf.sprintf "Value or constructor %s not found in module %s" field_name path_str)

(** {2 Record Access Inference} *)

(** [infer_record_access ~infer_expr ctx loc record_expr field_name] infers the type
    of a record field access expression.

    This handles both normal record access ([r.x]) and module access disambiguation
    when the expression looks like [M.N.x] where [M.N] is a module path. *)
let infer_record_access ~(infer_expr : expression_infer_fn) ctx loc record_expr field_name =
  let env = Typing_context.environment ctx in

  match extract_module_path env record_expr with
  | Some path_components ->
      let base_binding, mod_binding =
        Module_type_check.lookup_module_path env path_components loc in
      let internal_path =
        Module_type_check.module_path_to_internal_path base_binding.binding_id path_components in

      (match mod_binding.Module_types.binding_type with
       | Module_types.ModTypeSig sig_ ->
           resolve_module_member ctx sig_ internal_path field_name path_components loc
       | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
           Inference_utils.ensure_module_accessible loc mod_binding.Module_types.binding_type;
           Compiler_error.internal_error "Unreachable after module accessibility check")

  | None ->
      let typed_record_expr, ctx = infer_expr ctx record_expr in
      let field_type, ctx = Typing_context.new_type_variable ctx in
      let row_tail, ctx = Typing_context.new_type_variable ctx in
      let expected_record_type =
        Types.type_record_open [(field_name, RowFieldPresent field_type)] ~row_var:row_tail in

      unify_with_field_suggestions ctx loc
        ~field_name
        ~record_type:typed_record_expr.expression_type
        expected_record_type typed_record_expr.expression_type;

      ({ expression_desc = TypedExpressionRecordAccess (typed_record_expr, field_name);
         expression_type = field_type;
         expression_location = loc },
       ctx)

(** [infer_record_update ~infer_expr ctx loc base_expr update_fields] infers the type
    of a record update expression.

    Infers the type of the base record and each update field, then unifies the
    base record type with an open row type containing the update fields.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param base_expr The base record expression
    @param update_fields The list of fields to update
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_record_update ~(infer_expr : expression_infer_fn) ctx loc base_expr update_fields =
  (* Register field locations for precise error reporting *)
  register_field_locations update_fields;

  let typed_base_expr, ctx = infer_expr ctx base_expr in
  let typed_update_fields, ctx = infer_field_values ~infer_expr ctx update_fields in

  let update_field_types = List.map (fun typed_field ->
    (typed_field.Typed_tree.typed_field_name,
     RowFieldPresent typed_field.typed_field_value.expression_type)
  ) typed_update_fields in
  let row_tail, ctx = Typing_context.new_type_variable ctx in
  let expected_base_type = Types.type_record_open update_field_types ~row_var:row_tail in

  unify ctx loc expected_base_type typed_base_expr.expression_type;

  ({ expression_desc = TypedExpressionRecordUpdate (typed_base_expr, typed_update_fields);
     expression_type = typed_base_expr.expression_type;
     expression_location = loc },
   ctx)
