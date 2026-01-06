open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

let type_of_constant = function
  | ConstantInteger _ -> type_int
  | ConstantFloat _ -> type_float
  | ConstantString _ -> type_string
  | ConstantBoolean _ -> type_bool
  | ConstantUnit -> type_unit

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
        let fresh_params =
          List.map (fun _ -> new_type_variable ())
            constructor_info.constructor_type_parameters
        in
        let subst =
          List.combine
            (List.map (fun tv -> tv.id) constructor_info.constructor_type_parameters)
            fresh_params
        in
        let rec substitute type_to_substitute =
          match representative type_to_substitute with
          | TypeVariable type_var ->
            begin match List.assoc_opt type_var.id subst with
            | Some fresh_type -> fresh_type
            | None -> type_to_substitute
            end
          | TypeConstructor (path, type_arguments) ->
            TypeConstructor (path, List.map substitute type_arguments)
          | TypeTuple element_types ->
            TypeTuple (List.map substitute element_types)
          | TypeArrow (argument_type, result_type) ->
            TypeArrow (substitute argument_type, substitute result_type)
          | TypeRecord row ->
            TypeRecord (substitute_row row)
          | TypeRowEmpty ->
            TypeRowEmpty
        and substitute_row row = {
          row_fields = List.map (fun (field_name, field) ->
            (field_name, match field with
              | RowFieldPresent field_type -> RowFieldPresent (substitute field_type))
          ) row.row_fields;
          row_more = substitute row.row_more;
        }
        in
        (Option.map substitute constructor_info.constructor_argument_type,
         substitute constructor_info.constructor_result_type)
      in
      let typed_arg, env = match arg_pattern, expected_arg_ty with
        | None, None -> (None, env)
        | Some p, Some expected_ty ->
          let typed_p, actual_ty, env = infer_pattern env p in
          Unification.unify loc expected_ty actual_ty;
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
    let typed_inner, ty, env = infer_pattern env inner_pattern in
    let id = Identifier.create name in
    let env = Environment.add_value name id (trivial_scheme ty) env in
    let _ = typed_inner in
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
        let fresh_params =
          List.map (fun _ -> new_type_variable ())
            constructor_info.constructor_type_parameters
        in
        let subst =
          List.combine
            (List.map (fun tv -> tv.id) constructor_info.constructor_type_parameters)
            fresh_params
        in
        let rec substitute type_to_substitute =
          match representative type_to_substitute with
          | TypeVariable type_var ->
            begin match List.assoc_opt type_var.id subst with
            | Some fresh_type -> fresh_type
            | None -> type_to_substitute
            end
          | TypeConstructor (path, type_arguments) ->
            TypeConstructor (path, List.map substitute type_arguments)
          | TypeTuple element_types ->
            TypeTuple (List.map substitute element_types)
          | TypeArrow (argument_type, result_type) ->
            TypeArrow (substitute argument_type, substitute result_type)
          | TypeRecord row ->
            TypeRecord (substitute_row row)
          | TypeRowEmpty ->
            TypeRowEmpty
        and substitute_row row = {
          row_fields = List.map (fun (field_name, field) ->
            (field_name, match field with
              | RowFieldPresent field_type -> RowFieldPresent (substitute field_type))
          ) row.row_fields;
          row_more = substitute row.row_more;
        }
        in
        (Option.map substitute constructor_info.constructor_argument_type,
         substitute constructor_info.constructor_result_type)
      in
      let typed_arg = match arg_expr, expected_arg_ty with
        | None, None -> None
        | Some e, Some expected_ty ->
          let typed_e = infer_expression env e in
          Unification.unify loc expected_ty typed_e.expression_type;
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
    Unification.unify loc expected_func_ty typed_func.expression_type;
    {
      expression_desc = TypedExpressionApply (typed_func, typed_args);
      expression_type = result_ty;
      expression_location = loc;
    }

  | ExpressionFunction (param_patterns, body_expr) ->
    enter_level ();
    let typed_params, param_types, inner_env =
      List.fold_left (fun (pats, tys, env) p ->
        let pat, ty, env = infer_pattern env p in
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
    Unification.unify loc type_bool typed_cond.expression_type;
    let typed_then = infer_expression env then_expr in
    let typed_else = match else_expr_opt with
      | Some else_expr ->
        let typed_else = infer_expression env else_expr in
        Unification.unify loc typed_then.expression_type typed_else.expression_type;
        Some typed_else
      | None ->
        Unification.unify loc type_unit typed_then.expression_type;
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
    (* Infer the record expression type *)
    let typed_record_expression = infer_expression env record_expression in
    (* The field type is a fresh variable *)
    let field_type = new_type_variable () in
    (* The row tail is a fresh variable (for row polymorphism) *)
    let row_tail = new_type_variable () in
    (* Expected record type: { field_name: field_type; .. } *)
    let expected_record_type = TypeRecord {
      row_fields = [(field_name, RowFieldPresent field_type)];
      row_more = row_tail;
    } in
    Unification.unify loc expected_record_type typed_record_expression.expression_type;
    {
      expression_desc = TypedExpressionRecordAccess (typed_record_expression, field_name);
      expression_type = field_type;
      expression_location = loc;
    }

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
    Unification.unify loc expected_base_type typed_base_expression.expression_type;
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
      let typed_pattern, pattern_type, arm_environment = infer_pattern env match_arm.arm_pattern in
      (* Unify pattern type with scrutinee type *)
      Unification.unify match_arm.arm_location scrutinee_type pattern_type;
      (* Type-check guard if present - must be bool *)
      let typed_guard = match match_arm.arm_guard with
        | None -> None
        | Some guard_expression ->
          let typed_guard_expression = infer_expression arm_environment guard_expression in
          Unification.unify guard_expression.Location.location type_bool typed_guard_expression.expression_type;
          Some typed_guard_expression
      in
      (* Type-check arm body with pattern bindings *)
      let typed_arm_expression = infer_expression arm_environment match_arm.arm_expression in
      (* Unify arm result with overall result type *)
      Unification.unify match_arm.arm_location result_type typed_arm_expression.expression_type;
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

and infer_bindings env rec_flag bindings =
  match rec_flag with
  | Nonrecursive ->
    enter_level ();
    let typed_bindings, env =
      List.fold_left (fun (bs, env) (binding : binding) ->
        let typed_expr = infer_expression env binding.binding_expression in
        leave_level ();
        let scheme = generalize typed_expr.expression_type in
        enter_level ();
        let typed_pat, pat_ty, env = infer_pattern env binding.binding_pattern in
        Unification.unify binding.binding_location pat_ty typed_expr.expression_type;
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
        Unification.unify binding.binding_location expected_ty typed_expr.expression_type;
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
      List.fold_left2 (fun env (_binding : binding) (name, id, ty) ->
        let scheme = generalize ty in
        Environment.add_value name id scheme env
      ) env bindings (List.rev temp_info)
    in
    (typed_bindings, env)

let process_type_declaration env (type_decl : Parsing.Syntax_tree.type_declaration) =
  let type_params =
    List.map (fun _ ->
      match new_type_variable () with
      | TypeVariable tv -> tv
      | _ -> assert false
    ) type_decl.type_parameters
  in
  let result_type =
    TypeConstructor (PathUser type_decl.type_name.Location.value,
      List.map (fun tv -> TypeVariable tv) type_params)
  in
  let declaration_kind = match type_decl.type_kind with
    | TypeAbstract -> DeclarationAbstract
    | TypeVariant constructors ->
      let constructor_infos =
        List.mapi (fun tag_index (ctor : constructor_declaration) ->
          let arg_type = Option.map (fun _ -> new_type_variable ()) ctor.constructor_argument in
          {
            constructor_name = ctor.constructor_name.Location.value;
            constructor_tag_index = tag_index;
            constructor_type_name = type_decl.type_name.Location.value;
            constructor_argument_type = arg_type;
            constructor_result_type = result_type;
            constructor_type_parameters = type_params;
          }
        ) constructors
      in
      DeclarationVariant constructor_infos
  in
  let type_declaration = {
    declaration_name = type_decl.type_name.Location.value;
    declaration_parameters = type_params;
    declaration_kind;
  } in
  let env = Environment.add_type type_decl.type_name.Location.value type_declaration env in
  let env = match declaration_kind with
    | DeclarationAbstract -> env
    | DeclarationVariant constructors ->
      List.fold_left (fun env ctor ->
        Environment.add_constructor ctor.constructor_name ctor env
      ) env constructors
  in
  (type_declaration, env)

let infer_structure_item env (item : structure_item) =
  match item.Location.value with
  | StructureValue (rec_flag, bindings) ->
    let typed_bindings, env = infer_bindings env rec_flag bindings in
    let typed_item = {
      structure_item_desc = TypedStructureValue (rec_flag, typed_bindings);
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

  | StructureType type_decls ->
    let type_declarations, env =
      List.fold_left (fun (decls, env) type_decl ->
        let decl, env = process_type_declaration env type_decl in
        (decl :: decls, env)
      ) ([], env) type_decls
    in
    let typed_item = {
      structure_item_desc = TypedStructureType (List.rev type_declarations);
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

let infer_structure env structure =
  let typed_items, env =
    List.fold_left (fun (items, env) item ->
      let typed_item, env = infer_structure_item env item in
      (typed_item :: items, env)
    ) ([], env) structure
  in
  (List.rev typed_items, env)
