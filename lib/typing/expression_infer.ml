(** Expression type inference.

    This module handles type inference for expressions, including:
    - Variables, constants, tuples
    - Constructors and pattern matching
    - Function abstraction and application
    - Let bindings (recursive and non-recursive)
    - Records and record access/update
    - Module access expressions *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Unify types with alias expansion support. *)
let unify = Inference_utils.unify_with_env

(** Type of constant literals *)
let type_of_constant = Inference_utils.type_of_constant

(** [infer_expression env expr] infers the type of an expression.

    @param env The typing environment
    @param expr The expression to infer
    @return The typed expression with its inferred type *)
let rec infer_expression env (expr : expression) =
  let loc = expr.Location.location in
  match expr.Location.value with
  | ExpressionVariable name ->
    begin match Environment.find_value name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound variable: %s" name)
    | Some (id, scheme) ->
      let ty = instantiate scheme in
      {
        expression_desc = TypedExpressionVariable id;
        expression_type = ty;
        expression_location = loc;
      }
    end

  | ExpressionConstant const ->
    let ty = type_of_constant const in
    {
      expression_desc = TypedExpressionConstant const;
      expression_type = ty;
      expression_location = loc;
    }

  | ExpressionTuple exprs ->
    let typed_exprs = List.map (infer_expression env) exprs in
    let types = List.map (fun e -> e.expression_type) typed_exprs in
    {
      expression_desc = TypedExpressionTuple typed_exprs;
      expression_type = TypeTuple types;
      expression_location = loc;
    }

  | ExpressionConstructor (name, arg_expr) ->
    begin match Environment.find_constructor name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound constructor: %s" name)
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        Type_utils.instantiate_constructor constructor_info
      in
      let typed_arg = match arg_expr, expected_arg_ty with
        | None, None -> None
        | Some e, Some expected_ty ->
          let typed_e = infer_expression env e in
          unify env loc expected_ty typed_e.expression_type;
          Some typed_e
        | Some _, None ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s does not take an argument" name)
        | None, Some _ ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s requires an argument" name)
      in
      {
        expression_desc = TypedExpressionConstructor (constructor_info, typed_arg);
        expression_type = result_ty;
        expression_location = loc;
      }
    end

  | ExpressionApply (func_expr, arg_exprs) ->
    let typed_func = infer_expression env func_expr in
    let typed_args = List.map (infer_expression env) arg_exprs in
    let result_ty = new_type_variable () in
    let expected_func_ty =
      List.fold_right (fun arg acc ->
        TypeArrow (arg.expression_type, acc)
      ) typed_args result_ty
    in
    unify env loc expected_func_ty typed_func.expression_type;
    {
      expression_desc = TypedExpressionApply (typed_func, typed_args);
      expression_type = result_ty;
      expression_location = loc;
    }

  | ExpressionFunction (param_patterns, body_expr) ->
    enter_level ();
    let typed_params, param_types, inner_env =
      List.fold_left (fun (pats, tys, env) p ->
        let pat, ty, env = Pattern_infer.infer_pattern env p in
        (pat :: pats, ty :: tys, env)
      ) ([], [], env) param_patterns
    in
    let typed_params = List.rev typed_params in
    let param_types = List.rev param_types in
    let typed_body = infer_expression inner_env body_expr in
    leave_level ();
    let func_ty =
      List.fold_right (fun param_ty acc ->
        TypeArrow (param_ty, acc)
      ) param_types typed_body.expression_type
    in
    {
      expression_desc = TypedExpressionFunction (typed_params, typed_body);
      expression_type = func_ty;
      expression_location = loc;
    }

  | ExpressionLet (rec_flag, bindings, body_expr) ->
    let typed_bindings, inner_env = infer_bindings env rec_flag bindings in
    let typed_body = infer_expression inner_env body_expr in
    {
      expression_desc = TypedExpressionLet (rec_flag, typed_bindings, typed_body);
      expression_type = typed_body.expression_type;
      expression_location = loc;
    }

  | ExpressionIf (cond_expr, then_expr, else_expr_opt) ->
    let typed_cond = infer_expression env cond_expr in
    unify env loc type_bool typed_cond.expression_type;
    let typed_then = infer_expression env then_expr in
    let typed_else = match else_expr_opt with
      | Some else_expr ->
        let typed_else = infer_expression env else_expr in
        unify env loc typed_then.expression_type typed_else.expression_type;
        Some typed_else
      | None ->
        unify env loc type_unit typed_then.expression_type;
        None
    in
    {
      expression_desc = TypedExpressionIf (typed_cond, typed_then, typed_else);
      expression_type = typed_then.expression_type;
      expression_location = loc;
    }

  | ExpressionSequence (first_expr, second_expr) ->
    let typed_first = infer_expression env first_expr in
    let typed_second = infer_expression env second_expr in
    {
      expression_desc = TypedExpressionSequence (typed_first, typed_second);
      expression_type = typed_second.expression_type;
      expression_location = loc;
    }

  | ExpressionConstraint (inner_expr, _type_expr) ->
    infer_expression env inner_expr

  | ExpressionRecord record_fields ->
    (* Infer types for each field value *)
    let typed_record_fields = List.map (fun record_field ->
      let field_name = record_field.field_name.Location.value in
      let typed_field_value = infer_expression env record_field.field_value in
      {
        Typed_tree.typed_field_name = field_name;
        typed_field_value;
      }
    ) record_fields in
    (* Build row type from inferred field types *)
    let row_field_types = List.map (fun typed_field ->
      (typed_field.Typed_tree.typed_field_name,
       RowFieldPresent typed_field.typed_field_value.expression_type)
    ) typed_record_fields in
    let record_type = TypeRecord {
      row_fields = List.sort compare row_field_types;
      row_more = TypeRowEmpty;  (* Record literals are closed *)
    } in
    {
      expression_desc = TypedExpressionRecord typed_record_fields;
      expression_type = record_type;
      expression_location = loc;
    }

  | ExpressionRecordAccess (record_expression, field_name) ->
    (* Try to extract a module path from nested record accesses *)
    let rec extract_module_path expr =
      match expr.Location.value with
      | ExpressionConstructor (name, None) ->
        (* Check if this is a module *)
        begin match Environment.find_module name env with
        | Some _ -> Some [name]
        | None -> None
        end
      | ExpressionRecordAccess (inner, component) ->
        (* Recursively check if inner is a module path *)
        begin match extract_module_path inner with
        | Some path -> Some (path @ [component])
        | None -> None
        end
      | _ -> None
    in
    (* Check if record_expression is a module path *)
    begin match extract_module_path record_expression with
    | Some path_components ->
      (* This is module access: look up field in the module at path *)
      let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_components loc in
      (* Build internal path from the ROOT module's name *)
      let internal_path = Module_type_check.module_path_to_internal_path base_binding.mod_id path_components in
      begin match mod_binding.Module_types.mod_type with
      | Module_types.ModTypeSig sig_ ->
        (* First check if field_name is a submodule *)
        begin match Module_types.find_module_in_sig field_name sig_ with
        | Some _submod_type ->
          (* field_name is a submodule - return a module access to it *)
          let extended_path = Types.PathDot (internal_path, field_name) in
          (* Return a module access expression *)
          (* The type will be the submodule type, but we represent it as unit for now *)
          (* since we don't have proper module values at the expression level *)
          {
            expression_desc = TypedExpressionModuleAccess (extended_path, field_name);
            expression_type = TypeConstructor (PathBuiltin BuiltinUnit, []);
            expression_location = loc;
          }
        | None ->
          (* Check if field_name is a value *)
          begin match Module_types.find_value_in_sig field_name sig_ with
          | Some val_desc ->
            let ty = Types.instantiate val_desc.val_type in
            {
              expression_desc = TypedExpressionModuleAccess (internal_path, field_name);
              expression_type = ty;
              expression_location = loc;
            }
          | None ->
            let path_str = String.concat "." path_components in
            Compiler_error.type_error loc
              (Printf.sprintf "Value %s not found in module %s" field_name path_str)
          end
        end
      | Module_types.ModTypeFunctor _ ->
        Compiler_error.type_error loc "Cannot access value in a functor"
      | Module_types.ModTypeIdent _ ->
        Compiler_error.type_error loc "Cannot access value in an abstract module type"
      end
    | None ->
      (* Normal record access *)
      let typed_record_expression = infer_expression env record_expression in
      let field_type = new_type_variable () in
      let row_tail = new_type_variable () in
      let expected_record_type = TypeRecord {
        row_fields = [(field_name, RowFieldPresent field_type)];
        row_more = row_tail;
      } in
      unify env loc expected_record_type typed_record_expression.expression_type;
      {
        expression_desc = TypedExpressionRecordAccess (typed_record_expression, field_name);
        expression_type = field_type;
        expression_location = loc;
      }
    end

  | ExpressionRecordUpdate (base_expression, update_fields) ->
    (* Infer the base record expression type *)
    let typed_base_expression = infer_expression env base_expression in
    (* Infer types for each update field *)
    let typed_update_fields = List.map (fun update_field ->
      let field_name = update_field.field_name.Location.value in
      let typed_field_value = infer_expression env update_field.field_value in
      {
        Typed_tree.typed_field_name = field_name;
        typed_field_value;
      }
    ) update_fields in
    (* Build expected row type from update fields *)
    let update_field_types = List.map (fun typed_field ->
      (typed_field.Typed_tree.typed_field_name,
       RowFieldPresent typed_field.typed_field_value.expression_type)
    ) typed_update_fields in
    let row_tail = new_type_variable () in
    let expected_base_type = TypeRecord {
      row_fields = List.sort compare update_field_types;
      row_more = row_tail;
    } in
    unify env loc expected_base_type typed_base_expression.expression_type;
    (* Result type is same as base type *)
    {
      expression_desc = TypedExpressionRecordUpdate (typed_base_expression, typed_update_fields);
      expression_type = typed_base_expression.expression_type;
      expression_location = loc;
    }

  | ExpressionMatch (scrutinee_expression, match_arms) ->
    (* Infer type of the scrutinee *)
    let typed_scrutinee = infer_expression env scrutinee_expression in
    let scrutinee_type = typed_scrutinee.expression_type in
    (* Create fresh type variable for the result *)
    let result_type = new_type_variable () in
    (* Type-check each match arm *)
    let typed_match_arms = List.map (fun match_arm ->
      (* Infer pattern type and get bindings *)
      let typed_pattern, pattern_type, arm_environment = Pattern_infer.infer_pattern env match_arm.arm_pattern in
      (* Unify pattern type with scrutinee type *)
      unify env match_arm.arm_location scrutinee_type pattern_type;
      (* Type-check guard if present - must be bool *)
      let typed_guard = match match_arm.arm_guard with
        | None -> None
        | Some guard_expression ->
          let typed_guard_expression = infer_expression arm_environment guard_expression in
          unify env guard_expression.Location.location type_bool typed_guard_expression.expression_type;
          Some typed_guard_expression
      in
      (* Type-check arm body with pattern bindings *)
      let typed_arm_expression = infer_expression arm_environment match_arm.arm_expression in
      (* Unify arm result with overall result type *)
      unify env match_arm.arm_location result_type typed_arm_expression.expression_type;
      {
        Typed_tree.typed_arm_pattern = typed_pattern;
        typed_arm_guard = typed_guard;
        typed_arm_expression;
        typed_arm_location = match_arm.arm_location;
      }
    ) match_arms in
    (* Check exhaustiveness and redundancy *)
    Pattern_check.check_match env loc scrutinee_type typed_match_arms;
    {
      expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
      expression_type = result_type;
      expression_location = loc;
    }

  | ExpressionModuleAccess (module_path, value_name) ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules loc in
    begin match mod_binding.Module_types.mod_type with
    | Module_types.ModTypeSig sig_ ->
      begin match Module_types.find_value_in_sig value_name sig_ with
      | Some val_desc ->
        let ty = Types.instantiate val_desc.val_type in
        let internal_path = Module_type_check.module_path_to_internal_path base_binding.mod_id path_modules in
        {
          expression_desc = TypedExpressionModuleAccess (internal_path, value_name);
          expression_type = ty;
          expression_location = loc;
        }
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "Value %s not found in module" value_name)
      end
    | Module_types.ModTypeFunctor _ ->
      Compiler_error.type_error loc "Cannot access value in a functor"
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error loc "Cannot access value in an abstract module type"
    end

(** [infer_bindings env rec_flag bindings] infers types for a list of bindings.

    @param env The typing environment
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_env)] *)
and infer_bindings env rec_flag bindings =
  match rec_flag with
  | Nonrecursive ->
    enter_level ();
    let typed_bindings, env =
      List.fold_left (fun (bs, env) (binding : binding) ->
        let typed_expr = infer_expression env binding.binding_expression in
        leave_level ();
        (* Value restriction with Garrigue's relaxation:
           - Values: full generalization
           - Non-values: generalize only covariant type variables *)
        let scheme =
          if Value_check.is_value typed_expr then
            generalize typed_expr.expression_type
          else
            (* Relaxed value restriction: generalize covariant-only variables *)
            generalize_with_filter Value_check.can_generalize_relaxed typed_expr.expression_type
        in
        enter_level ();
        let typed_pat, pat_ty, env = Pattern_infer.infer_pattern env binding.binding_pattern in
        unify env binding.binding_location pat_ty typed_expr.expression_type;
        let env = match binding.binding_pattern.Location.value, typed_pat.pattern_desc with
          | PatternVariable name, TypedPatternVariable id ->
            Environment.add_value name id scheme env
          | _ -> env
        in
        let typed_binding = {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        } in
        (typed_binding :: bs, env)
      ) ([], env) bindings
    in
    leave_level ();
    (List.rev typed_bindings, env)

  | Recursive ->
    enter_level ();
    let env, temp_info =
      List.fold_left (fun (env, temps) (binding : binding) ->
        match binding.binding_pattern.Location.value with
        | PatternVariable name ->
          let ty = new_type_variable () in
          let id = Identifier.create name in
          let env = Environment.add_value name id (trivial_scheme ty) env in
          (env, (name, id, ty) :: temps)
        | _ ->
          Compiler_error.type_error binding.binding_location
            "Recursive bindings must be simple variables"
      ) (env, []) bindings
    in
    let typed_bindings =
      List.map2 (fun (binding : binding) (_name, id, expected_ty) ->
        let typed_expr = infer_expression env binding.binding_expression in
        unify env binding.binding_location expected_ty typed_expr.expression_type;
        let typed_pat = {
          pattern_desc = TypedPatternVariable id;
          pattern_type = expected_ty;
          pattern_location = binding.binding_pattern.Location.location;
        } in
        {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        }
      ) bindings (List.rev temp_info)
    in
    leave_level ();
    let env =
      List.fold_left2 (fun env (binding : binding) (name, id, ty) ->
        (* Value restriction with Garrigue's relaxation:
           - Values: full generalization
           - Non-values: generalize only covariant type variables *)
        let typed_expr =
          List.find (fun tb -> tb.Typed_tree.binding_location = binding.binding_location) typed_bindings
        in
        let scheme =
          if Value_check.is_value typed_expr.binding_expression then
            generalize ty
          else
            (* Relaxed value restriction: generalize covariant-only variables *)
            generalize_with_filter Value_check.can_generalize_relaxed ty
        in
        Environment.add_value name id scheme env
      ) env bindings (List.rev temp_info)
    in
    (typed_bindings, env)
