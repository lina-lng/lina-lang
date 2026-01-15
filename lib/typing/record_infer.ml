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
type expression_infer_fn =
  Typing_context.t -> expression -> typed_expression * Typing_context.t

(** {1 Helper Functions} *)

(** Unify types using context's environment for alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** Extract a module path from nested record access expressions.

    Given [M.N.x], this extracts [["M"; "N"]] when [M] and [M.N] are modules.
    Returns [None] if the expression is not a module path.

    @param env The typing environment for module lookups
    @param expr The expression to extract from
    @return [Some path_components] if expr is a module path, [None] otherwise *)
let rec extract_module_path env expr =
  match expr.Location.value with
  | ExpressionConstructor (longident, None) ->
    (* Check if this is a module name - for simple Lident only *)
    begin match longident.Location.value with
    | Lident name ->
      begin match Environment.find_module name env with
      | Some _ -> Some [name]
      | None -> None
      end
    | Ldot _ -> None (* Qualified constructors are not module paths *)
    end
  | ExpressionRecordAccess (inner, component) ->
    (* Recursively check if inner is a module path *)
    begin match extract_module_path env inner with
    | Some path -> Some (path @ [component])
    | None -> None
    end
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
  (* Check for duplicate fields *)
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
  (* Infer types for each field value *)
  let typed_record_fields, ctx = List.fold_left (fun (fields, ctx) record_field ->
    let field_name = record_field.field_name.Location.value in
    let typed_field_value, ctx = infer_expr ctx record_field.field_value in
    let typed_field = {
      Typed_tree.typed_field_name = field_name;
      typed_field_value;
    } in
    (typed_field :: fields, ctx)
  ) ([], ctx) record_fields in
  let typed_record_fields = List.rev typed_record_fields in
  (* Build row type from inferred field types *)
  let row_field_types = List.map (fun typed_field ->
    (typed_field.Typed_tree.typed_field_name,
     RowFieldPresent typed_field.typed_field_value.expression_type)
  ) typed_record_fields in
  let record_type = Types.type_record_closed row_field_types in
  ({
    expression_desc = TypedExpressionRecord typed_record_fields;
    expression_type = record_type;
    expression_location = loc;
  }, ctx)

(** [infer_record_access ~infer_expr ctx loc record_expr field_name] infers the type
    of a record field access expression.

    This handles both normal record access ([r.x]) and module access disambiguation
    when the expression looks like [M.N.x] where [M.N] is a module path.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param record_expr The record expression being accessed
    @param field_name The name of the field to access
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_record_access ~(infer_expr : expression_infer_fn) ctx loc record_expr field_name =
  let env = Typing_context.environment ctx in
  (* Check if record_expr is a module path *)
  match extract_module_path env record_expr with
  | Some path_components ->
    (* This is module access: look up field in the module at path *)
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_components loc in
    (* Build internal path from the ROOT module's name *)
    let internal_path = Module_type_check.module_path_to_internal_path base_binding.binding_id path_components in
    begin match mod_binding.Module_types.binding_type with
    | Module_types.ModTypeSig sig_ ->
      (* First check if field_name is a submodule *)
      begin match Module_types.find_module_in_sig field_name sig_ with
      | Some _submod_type ->
        (* field_name is a submodule - return a module access to it *)
        let extended_path = Types.PathDot (internal_path, field_name) in
        (* Return a module access expression *)
        (* The type will be the submodule type, but we represent it as unit for now *)
        (* since we don't have proper module values at the expression level *)
        ({
          expression_desc = TypedExpressionModuleAccess (extended_path, field_name);
          expression_type = TypeConstructor (PathBuiltin BuiltinUnit, []);
          expression_location = loc;
        }, ctx)
      | None ->
        (* Check if field_name is a value *)
        begin match Module_types.find_value_in_sig field_name sig_ with
        | Some val_desc ->
          let ty, ctx = Typing_context.instantiate ctx val_desc.value_type in
          ({
            expression_desc = TypedExpressionModuleAccess (internal_path, field_name);
            expression_type = ty;
            expression_location = loc;
          }, ctx)
        | None ->
          (* Not a value - check if it's a constructor (uppercase names) *)
          begin match Module_types.find_constructor_in_sig field_name sig_ with
          | Some ctor_info ->
            (* Found a constructor - instantiate and return *)
            let level = Typing_context.current_level ctx in
            let fresh_var () = Types.new_type_variable_at_level level in
            let arg_type_opt, result_type = Type_utils.instantiate_constructor ~fresh_var ctor_info in
            (* For constructors with arguments, return arrow type since we return
               the constructor without its argument applied *)
            let expr_type = match arg_type_opt with
              | None -> result_type
              | Some arg_type -> Types.TypeArrow (Types.Nolabel, arg_type, result_type)
            in
            ({
              expression_desc = TypedExpressionConstructor (ctor_info, None);
              expression_type = expr_type;
              expression_location = loc;
            }, ctx)
          | None ->
            let path_str = String.concat "." path_components in
            Compiler_error.type_error loc
              (Printf.sprintf "Value or constructor %s not found in module %s" field_name path_str)
          end
        end
      end
    | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
      (* ensure_module_accessible raises for these cases *)
      Inference_utils.ensure_module_accessible loc mod_binding.Module_types.binding_type;
      (* If we reach here, ensure_module_accessible failed to raise *)
      Compiler_error.internal_error
        "Unreachable code after module accessibility check"
    end
  | None ->
    (* Normal record access *)
    let typed_record_expr, ctx = infer_expr ctx record_expr in
    let field_type, ctx = Typing_context.new_type_variable ctx in
    let row_tail, ctx = Typing_context.new_type_variable ctx in
    let expected_record_type = Types.type_record_open
      [(field_name, RowFieldPresent field_type)] ~row_var:row_tail in
    unify ctx loc expected_record_type typed_record_expr.expression_type;
    ({
      expression_desc = TypedExpressionRecordAccess (typed_record_expr, field_name);
      expression_type = field_type;
      expression_location = loc;
    }, ctx)

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
  (* Infer the base record expression type *)
  let typed_base_expr, ctx = infer_expr ctx base_expr in
  (* Infer types for each update field *)
  let typed_update_fields, ctx = List.fold_left (fun (fields, ctx) update_field ->
    let field_name = update_field.field_name.Location.value in
    let typed_field_value, ctx = infer_expr ctx update_field.field_value in
    let typed_field = {
      Typed_tree.typed_field_name = field_name;
      typed_field_value;
    } in
    (typed_field :: fields, ctx)
  ) ([], ctx) update_fields in
  let typed_update_fields = List.rev typed_update_fields in
  (* Build expected row type from update fields *)
  let update_field_types = List.map (fun typed_field ->
    (typed_field.Typed_tree.typed_field_name,
     RowFieldPresent typed_field.typed_field_value.expression_type)
  ) typed_update_fields in
  let row_tail, ctx = Typing_context.new_type_variable ctx in
  let expected_base_type = Types.type_record_open update_field_types ~row_var:row_tail in
  unify ctx loc expected_base_type typed_base_expr.expression_type;
  (* Result type is same as base type *)
  ({
    expression_desc = TypedExpressionRecordUpdate (typed_base_expr, typed_update_fields);
    expression_type = typed_base_expr.expression_type;
    expression_location = loc;
  }, ctx)
