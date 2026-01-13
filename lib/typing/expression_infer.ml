(** Expression type inference.

    This module handles type inference for expressions, including:
    - Variables, constants, tuples
    - Constructors and pattern matching
    - Function abstraction and application
    - Let bindings (recursive and non-recursive)
    - Records and record access/update
    - Module access expressions

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable]. *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Unify types using context's environment for alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** Type of constant literals *)
let type_of_constant = Inference_utils.type_of_constant

(** Extract a module path from nested record access expressions.

    Given [M.N.x], this extracts [["M"; "N"]] when [M] and [M.N] are modules.
    Returns [None] if the expression is not a module path.

    @param env The typing environment for module lookups
    @param expr The expression to extract from
    @return [Some path_components] if expr is a module path, [None] otherwise *)
let rec extract_module_path env expr =
  match expr.Location.value with
  | ExpressionConstructor (name, None) ->
    (* Check if this is a module *)
    begin match Environment.find_module name env with
    | Some _ -> Some [name]
    | None -> None
    end
  | ExpressionRecordAccess (inner, component) ->
    (* Recursively check if inner is a module path *)
    begin match extract_module_path env inner with
    | Some path -> Some (path @ [component])
    | None -> None
    end
  | _ -> None

(** [infer_expression ctx expr] infers the type of an expression.

    @param ctx The typing context
    @param expr The expression to infer
    @return A pair [(typed_expr, updated_ctx)] *)
let rec infer_expression ctx (expr : expression) =
  let env = Typing_context.environment ctx in
  let loc = expr.Location.location in
  match expr.Location.value with
  | ExpressionVariable name ->
    begin match Environment.find_value name env with
    | None -> Inference_utils.error_unbound_variable loc name
    | Some binding ->
      let ty, ctx = Typing_context.instantiate ctx binding.Environment.binding_scheme in
      (* Store definition location for go-to-definition support *)
      let def_loc =
        if Common.Location.is_none binding.binding_location then None
        else Some binding.binding_location
      in
      ({
        expression_desc = TypedExpressionVariable (binding.binding_id, def_loc);
        expression_type = ty;
        expression_location = loc;
      }, ctx)
    end

  | ExpressionConstant const ->
    let ty = type_of_constant const in
    ({
      expression_desc = TypedExpressionConstant const;
      expression_type = ty;
      expression_location = loc;
    }, ctx)

  | ExpressionTuple exprs ->
    let typed_exprs, ctx = infer_expression_list ctx exprs in
    let types = List.map (fun e -> e.expression_type) typed_exprs in
    ({
      expression_desc = TypedExpressionTuple typed_exprs;
      expression_type = TypeTuple types;
      expression_location = loc;
    }, ctx)

  | ExpressionConstructor (name, arg_expr) ->
    begin match Environment.find_constructor name env with
    | None -> Inference_utils.error_unbound_constructor loc name
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        Inference_utils.instantiate_constructor_with_ctx ctx constructor_info
      in
      Inference_utils.check_constructor_arity loc name
        ~has_arg:(Option.is_some arg_expr)
        ~expects_arg:(Option.is_some expected_arg_ty);
      let typed_arg, ctx = match arg_expr, expected_arg_ty with
        | None, None -> (None, ctx)
        | Some e, Some expected_ty ->
          let typed_e, ctx = infer_expression ctx e in
          unify ctx loc expected_ty typed_e.expression_type;
          (Some typed_e, ctx)
        | Some _, None | None, Some _ ->
          (* Constructor arity was already checked by check_constructor_arity *)
          Compiler_error.internal_error
            "Constructor arity mismatch after arity check"
      in
      ({
        expression_desc = TypedExpressionConstructor (constructor_info, typed_arg);
        expression_type = result_ty;
        expression_location = loc;
      }, ctx)
    end

  | ExpressionApply (func_expr, arg_exprs) ->
    let typed_func, ctx = infer_expression ctx func_expr in
    let typed_args, ctx = infer_expression_list ctx arg_exprs in
    let result_ty, ctx = Typing_context.new_type_variable ctx in
    let expected_func_ty =
      List.fold_right (fun arg acc ->
        TypeArrow (arg.expression_type, acc)
      ) typed_args result_ty
    in
    unify ctx loc expected_func_ty typed_func.expression_type;
    ({
      expression_desc = TypedExpressionApply (typed_func, typed_args);
      expression_type = result_ty;
      expression_location = loc;
    }, ctx)

  | ExpressionFunction (param_patterns, body_expr) ->
    let ctx = Typing_context.enter_level ctx in
    let typed_params, param_types, inner_ctx =
      List.fold_left (fun (pats, tys, ctx) p ->
        let pat, ty, ctx = Pattern_infer.infer_pattern ctx p in
        (pat :: pats, ty :: tys, ctx)
      ) ([], [], ctx) param_patterns
    in
    let typed_params = List.rev typed_params in
    let param_types = List.rev param_types in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in
    let ctx = Typing_context.leave_level ctx in
    let func_ty =
      List.fold_right (fun param_ty acc ->
        TypeArrow (param_ty, acc)
      ) param_types typed_body.expression_type
    in
    ({
      expression_desc = TypedExpressionFunction (typed_params, typed_body);
      expression_type = func_ty;
      expression_location = loc;
    }, ctx)

  | ExpressionLet (rec_flag, bindings, body_expr) ->
    let typed_bindings, inner_ctx = infer_bindings ctx rec_flag bindings in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in
    ({
      expression_desc = TypedExpressionLet (rec_flag, typed_bindings, typed_body);
      expression_type = typed_body.expression_type;
      expression_location = loc;
    }, ctx)

  | ExpressionIf (cond_expr, then_expr, else_expr_opt) ->
    let typed_cond, ctx = infer_expression ctx cond_expr in
    unify ctx loc type_bool typed_cond.expression_type;
    let typed_then, ctx = infer_expression ctx then_expr in
    let typed_else, ctx = match else_expr_opt with
      | Some else_expr ->
        let typed_else, ctx = infer_expression ctx else_expr in
        unify ctx loc typed_then.expression_type typed_else.expression_type;
        (Some typed_else, ctx)
      | None ->
        unify ctx loc type_unit typed_then.expression_type;
        (None, ctx)
    in
    ({
      expression_desc = TypedExpressionIf (typed_cond, typed_then, typed_else);
      expression_type = typed_then.expression_type;
      expression_location = loc;
    }, ctx)

  | ExpressionSequence (first_expr, second_expr) ->
    let typed_first, ctx = infer_expression ctx first_expr in
    let typed_second, ctx = infer_expression ctx second_expr in
    ({
      expression_desc = TypedExpressionSequence (typed_first, typed_second);
      expression_type = typed_second.expression_type;
      expression_location = loc;
    }, ctx)

  | ExpressionConstraint (inner_expr, _type_expr) ->
    infer_expression ctx inner_expr

  | ExpressionRecord record_fields ->
    (* Infer types for each field value *)
    let typed_record_fields, ctx = List.fold_left (fun (fields, ctx) record_field ->
      let field_name = record_field.field_name.Location.value in
      let typed_field_value, ctx = infer_expression ctx record_field.field_value in
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

  | ExpressionRecordAccess (record_expression, field_name) ->
    (* Check if record_expression is a module path *)
    begin match extract_module_path env record_expression with
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
            let path_str = String.concat "." path_components in
            Compiler_error.type_error loc
              (Printf.sprintf "Value %s not found in module %s" field_name path_str)
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
      let typed_record_expression, ctx = infer_expression ctx record_expression in
      let field_type, ctx = Typing_context.new_type_variable ctx in
      let row_tail, ctx = Typing_context.new_type_variable ctx in
      let expected_record_type = Types.type_record_open
        [(field_name, RowFieldPresent field_type)] ~row_var:row_tail in
      unify ctx loc expected_record_type typed_record_expression.expression_type;
      ({
        expression_desc = TypedExpressionRecordAccess (typed_record_expression, field_name);
        expression_type = field_type;
        expression_location = loc;
      }, ctx)
    end

  | ExpressionRecordUpdate (base_expression, update_fields) ->
    (* Infer the base record expression type *)
    let typed_base_expression, ctx = infer_expression ctx base_expression in
    (* Infer types for each update field *)
    let typed_update_fields, ctx = List.fold_left (fun (fields, ctx) update_field ->
      let field_name = update_field.field_name.Location.value in
      let typed_field_value, ctx = infer_expression ctx update_field.field_value in
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
    unify ctx loc expected_base_type typed_base_expression.expression_type;
    (* Result type is same as base type *)
    ({
      expression_desc = TypedExpressionRecordUpdate (typed_base_expression, typed_update_fields);
      expression_type = typed_base_expression.expression_type;
      expression_location = loc;
    }, ctx)

  | ExpressionMatch (scrutinee_expression, match_arms) ->
    (* Infer type of the scrutinee *)
    let typed_scrutinee, ctx = infer_expression ctx scrutinee_expression in
    let scrutinee_type = typed_scrutinee.expression_type in
    (* Create fresh type variable for the result *)
    let result_type, ctx = Typing_context.new_type_variable ctx in
    (* Type-check each match arm *)
    let typed_match_arms, ctx = List.fold_left (fun (arms, ctx) match_arm ->
      (* Infer pattern type and get bindings *)
      let typed_pattern, pattern_type, arm_ctx = Pattern_infer.infer_pattern ctx match_arm.arm_pattern in
      (* Unify pattern type with scrutinee type *)
      unify ctx match_arm.arm_location scrutinee_type pattern_type;
      (* Type-check guard if present - must be bool *)
      let typed_guard, arm_ctx = match match_arm.arm_guard with
        | None -> (None, arm_ctx)
        | Some guard_expression ->
          let typed_guard_expression, arm_ctx = infer_expression arm_ctx guard_expression in
          unify arm_ctx guard_expression.Location.location type_bool typed_guard_expression.expression_type;
          (Some typed_guard_expression, arm_ctx)
      in
      (* Type-check arm body with pattern bindings *)
      let typed_arm_expression, _arm_ctx = infer_expression arm_ctx match_arm.arm_expression in
      (* Unify arm result with overall result type *)
      unify ctx match_arm.arm_location result_type typed_arm_expression.expression_type;
      let typed_arm = {
        Typed_tree.typed_arm_pattern = typed_pattern;
        typed_arm_guard = typed_guard;
        typed_arm_expression;
        typed_arm_location = match_arm.arm_location;
      } in
      (typed_arm :: arms, ctx)
    ) ([], ctx) match_arms in
    let typed_match_arms = List.rev typed_match_arms in
    (* Check exhaustiveness and redundancy *)
    Pattern_check.check_match env loc scrutinee_type typed_match_arms;
    ({
      expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
      expression_type = result_type;
      expression_location = loc;
    }, ctx)

  | ExpressionModuleAccess (module_path, value_name) ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules loc in
    Inference_utils.ensure_module_accessible loc mod_binding.Module_types.binding_type;
    (* Now we know it's a signature *)
    begin match mod_binding.Module_types.binding_type with
    | Module_types.ModTypeSig sig_ ->
      begin match Module_types.find_value_in_sig value_name sig_ with
      | Some val_desc ->
        let ty, ctx = Typing_context.instantiate ctx val_desc.value_type in
        let internal_path = Module_type_check.module_path_to_internal_path base_binding.binding_id path_modules in
        ({
          expression_desc = TypedExpressionModuleAccess (internal_path, value_name);
          expression_type = ty;
          expression_location = loc;
        }, ctx)
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "Value %s not found in module" value_name)
      end
    | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
      (* ensure_module_accessible should have raised for these cases *)
      Compiler_error.internal_error
        "Expected signature after module accessibility check"
    end

  (* === Reference operations === *)

  | ExpressionRef inner_expr ->
    (* ref e : ref 'a when e : 'a *)
    let typed_inner, ctx = infer_expression ctx inner_expr in
    let ref_type = Types.type_ref typed_inner.expression_type in
    ({
      expression_desc = TypedExpressionRef typed_inner;
      expression_type = ref_type;
      expression_location = loc;
    }, ctx)

  | ExpressionDeref ref_expr ->
    (* !e : 'a when e : ref 'a *)
    let typed_ref, ctx = infer_expression ctx ref_expr in
    let content_type, ctx = Typing_context.new_type_variable ctx in
    unify ctx loc typed_ref.expression_type (Types.type_ref content_type);
    ({
      expression_desc = TypedExpressionDeref typed_ref;
      expression_type = content_type;
      expression_location = loc;
    }, ctx)

  | ExpressionAssign (ref_expr, value_expr) ->
    (* e1 := e2 : unit when e1 : ref 'a and e2 : 'a *)
    let typed_ref, ctx = infer_expression ctx ref_expr in
    let typed_value, ctx = infer_expression ctx value_expr in
    let content_type, ctx = Typing_context.new_type_variable ctx in
    unify ctx loc typed_ref.expression_type (Types.type_ref content_type);
    unify ctx loc typed_value.expression_type content_type;
    ({
      expression_desc = TypedExpressionAssign (typed_ref, typed_value);
      expression_type = Types.type_unit;
      expression_location = loc;
    }, ctx)

  | ExpressionError error_info ->
    (* Error expressions get a fresh type variable and are preserved in typed tree *)
    let error_ty, ctx = Typing_context.new_type_variable ctx in
    ({
      expression_desc = TypedExpressionError error_info;
      expression_type = error_ty;
      expression_location = loc;
    }, ctx)

(** Helper to infer a list of expressions, threading context *)
and infer_expression_list ctx exprs =
  List.fold_left (fun (typed_exprs, ctx) expr ->
    let typed_expr, ctx = infer_expression ctx expr in
    (typed_exprs @ [typed_expr], ctx)
  ) ([], ctx) exprs

(** [infer_bindings ctx rec_flag bindings] infers types for a list of bindings.

    @param ctx The typing context
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_ctx)] *)
and infer_bindings ctx rec_flag bindings =
  match rec_flag with
  | Nonrecursive ->
    let ctx = Typing_context.enter_level ctx in
    let typed_bindings, ctx =
      List.fold_left (fun (bs, ctx) (binding : binding) ->
        let typed_expr, ctx = infer_expression ctx binding.binding_expression in
        let ctx = Typing_context.leave_level ctx in
        let level = Typing_context.current_level ctx in
        let scheme = Inference_utils.compute_binding_scheme ~level typed_expr typed_expr.expression_type in
        let ctx = Typing_context.enter_level ctx in
        let typed_pat, pat_ty, ctx = Pattern_infer.infer_pattern ctx binding.binding_pattern in
        unify ctx binding.binding_location pat_ty typed_expr.expression_type;
        let env = Typing_context.environment ctx in
        let env = match binding.binding_pattern.Location.value, typed_pat.pattern_desc with
          | PatternVariable name, TypedPatternVariable id ->
            Environment.add_value name id scheme binding.binding_location env
          | _ -> env
        in
        let ctx = Typing_context.with_environment env ctx in
        let typed_binding = {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        } in
        (typed_binding :: bs, ctx)
      ) ([], ctx) bindings
    in
    let ctx = Typing_context.leave_level ctx in
    (List.rev typed_bindings, ctx)

  | Recursive ->
    let ctx = Typing_context.enter_level ctx in
    let env = Typing_context.environment ctx in
    let env, temp_info, ctx =
      List.fold_left (fun (env, temps, ctx) (binding : binding) ->
        match binding.binding_pattern.Location.value with
        | PatternVariable name ->
          let ty, ctx = Typing_context.new_type_variable ctx in
          let id = Identifier.create name in
          let env = Environment.add_value name id (trivial_scheme ty) binding.binding_location env in
          (env, (name, id, ty, binding.binding_location) :: temps, ctx)
        | _ ->
          Compiler_error.type_error binding.binding_location
            "Recursive bindings must be simple variables"
      ) (env, [], ctx) bindings
    in
    let ctx = Typing_context.with_environment env ctx in
    let typed_bindings, ctx =
      List.fold_left2 (fun (typed_bindings, ctx) (binding : binding) (_name, id, expected_ty, _def_loc) ->
        let typed_expr, ctx = infer_expression ctx binding.binding_expression in
        unify ctx binding.binding_location expected_ty typed_expr.expression_type;
        let typed_pat = {
          pattern_desc = TypedPatternVariable id;
          pattern_type = expected_ty;
          pattern_location = binding.binding_pattern.Location.location;
        } in
        let typed_binding = {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        } in
        (typed_binding :: typed_bindings, ctx)
      ) ([], ctx) bindings (List.rev temp_info)
    in
    let typed_bindings = List.rev typed_bindings in
    let ctx = Typing_context.leave_level ctx in
    let level = Typing_context.current_level ctx in
    let env = Typing_context.environment ctx in
    let env =
      List.fold_left2 (fun env (binding : binding) (name, id, ty, def_loc) ->
        let typed_expr =
          List.find (fun tb -> tb.Typed_tree.binding_location = binding.binding_location) typed_bindings
        in
        let scheme = Inference_utils.compute_binding_scheme ~level typed_expr.binding_expression ty in
        Environment.add_value name id scheme def_loc env
      ) env bindings (List.rev temp_info)
    in
    let ctx = Typing_context.with_environment env ctx in
    (typed_bindings, ctx)
