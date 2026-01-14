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

(** {1 Pattern Binding Collection} *)

(** Collect all variable bindings from a typed pattern.
    Returns a list of (name, identifier, type) tuples. *)
let rec collect_pattern_bindings pattern =
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
    [(Identifier.name id, id, pattern.pattern_type)]
  | TypedPatternWildcard
  | TypedPatternConstant _
  | TypedPatternLocallyAbstract _ ->
    []
  | TypedPatternTuple patterns ->
    List.concat_map collect_pattern_bindings patterns
  | TypedPatternConstructor (_, None) ->
    []
  | TypedPatternConstructor (_, Some p) ->
    collect_pattern_bindings p
  | TypedPatternRecord (fields, _) ->
    List.concat_map (fun f -> collect_pattern_bindings f.typed_pattern_field_pattern) fields
  | TypedPatternPolyVariant (_, None) ->
    []
  | TypedPatternPolyVariant (_, Some p) ->
    collect_pattern_bindings p
  | TypedPatternError _ ->
    []

(** {1 Polymorphic Recursion Support} *)

(** Binding info for recursive bindings - tracks whether standard or polymorphic recursion *)
type rec_binding_info =
  | PolyRecBinding of {
      name : string;
      id : Identifier.t;
      scheme : type_scheme;
      rigid_vars : (string * type_variable) list;
      binding : binding;
    }
  | StandardRecBinding of {
      name : string;
      id : Identifier.t;
      mono_type : type_expression;
      binding : binding;
    }

(** Extract polymorphic recursion annotation from a binding.
    Returns [Some (name, forall_vars, body_ty)] if the binding has form:
    [let name : type a b. body_ty = ...]
    Returns [None] otherwise. *)
let extract_poly_rec_annotation (binding : binding) =
  match binding.binding_pattern.Location.value with
  | PatternConstraint (inner_pat, ty_expr) ->
    begin match ty_expr.Location.value with
    | TypeForall (vars, body) ->
      begin match inner_pat.Location.value with
      | PatternVariable name -> Some (name, vars, body, ty_expr.Location.location)
      | _ -> None
      end
    | _ -> None
    end
  | _ -> None

(** Check a TypeForall annotation for polymorphic recursion.
    Creates rigid type variables for bound names and checks the body type. *)
let check_forall_annotation ctx forall_vars body_ty_expr =
  (* Create rigid type variables for each bound name.
     Note: We accumulate in reverse order during fold_left for O(1) cons,
     then reverse at the end. This is the standard OCaml idiom for mapping
     with state threading, used throughout the inference code. *)
  let rigid_vars, ctx =
    List.fold_left (fun (vars, ctx) name ->
      let tv, ctx = Typing_context.new_rigid_type_variable ctx in
      match tv with
      | TypeVariable tv_rec -> ((name, tv_rec) :: vars, ctx)
      | _ -> Compiler_error.internal_error "new_rigid_type_variable didn't return TypeVariable"
    ) ([], ctx) forall_vars
  in
  let rigid_vars = List.rev rigid_vars in
  (* Check the body type with rigid vars as parameters *)
  let param_names = List.map fst rigid_vars in
  let param_vars = List.map snd rigid_vars in
  let body_type, ctx = Type_expression_check.check_type_expression_with_params ctx param_names param_vars body_ty_expr in
  (* Build scheme: the rigid vars become quantified variables *)
  let scheme = { quantified_variables = param_vars; body = body_type } in
  (scheme, rigid_vars, ctx)

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
      (* Check if the type is private - private types cannot be constructed *)
      let () =
        match Environment.find_type constructor_info.constructor_type_name env with
        | Some type_decl when type_decl.declaration_private ->
          Compiler_error.type_error loc
            (Printf.sprintf "Cannot construct value of private type %s"
               constructor_info.constructor_type_name)
        | _ -> ()
      in
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
        (* Locally abstract types don't contribute to the function signature -
           they only introduce a type into scope. Filter them out when building
           the parameter type list, but keep them in typed_params for the AST. *)
        let tys = match pat.pattern_desc with
          | TypedPatternLocallyAbstract _ -> tys  (* Skip - no runtime parameter *)
          | _ -> ty :: tys
        in
        (pat :: pats, tys, ctx)
      ) ([], [], ctx) param_patterns
    in
    let typed_params = List.rev typed_params in
    let param_types = List.rev param_types in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in
    (* Check for existential type escape in function parameters.
       Existential type variables introduced by GADT patterns in parameters
       cannot appear in the function's return type. *)
    let existential_ids = List.concat_map Gadt.collect_existentials_from_pattern typed_params in
    begin match Gadt.check_existential_escape existential_ids typed_body.expression_type with
    | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
    | None -> ()
    end;
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

  | ExpressionConstraint (inner_expr, type_expr) ->
    (* Type annotation constrains the expression type.
       Use bidirectional type checking: pass the constraint type as the expected type
       to enable GADT type refinement in match expressions. *)
    let constraint_ty, ctx = Type_expression_check.check_type_expression ctx type_expr in
    let typed_inner, ctx = infer_expression_with_expected ctx (Some constraint_ty) inner_expr in
    (* Unify the inferred type with the annotated type *)
    unify ctx loc typed_inner.expression_type constraint_ty;
    (* Return with the annotated type (may be more specific due to rigid variables) *)
    ({ typed_inner with expression_type = constraint_ty }, ctx)

  | ExpressionRecord record_fields ->
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
    (* Check if scrutinee contains rigid type variables (GADT case) *)
    let scrutinee_has_rigid = Gadt.has_rigid_variables scrutinee_type in
    (* Type-check each match arm *)
    let typed_match_arms, ctx = List.fold_left (fun (arms, ctx) match_arm ->
      (* Infer pattern type and get bindings *)
      let typed_pattern, pattern_type, arm_ctx = Pattern_infer.infer_pattern ctx match_arm.arm_pattern in
      (* Extract GADT equations if applicable *)
      let gadt_equations =
        if scrutinee_has_rigid then
          let extraction = Gadt.extract_equations scrutinee_type pattern_type in
          if extraction.success then extraction.equations else []
        else
          []
      in
      (* Unify pattern type with scrutinee type (for non-rigid parts) *)
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
      (* Check for existential type escape.
         Existential type variables introduced by a GADT pattern cannot appear
         in the result type of the match branch. *)
      let existential_ids = Gadt.collect_existentials_from_pattern typed_pattern in
      begin match Gadt.check_existential_escape existential_ids typed_arm_expression.expression_type with
      | Some _escaped_id ->
        Compiler_error.type_error match_arm.arm_location
          "This expression has a type containing an existential type variable \
           that would escape its scope"
      | None -> ()
      end;
      (* Unify arm result with expected result type.
         For GADT branches, apply equations to the result type first, so that
         a rigid type variable like 'a' becomes 'int' after equation a=int. *)
      let expected_result_type =
        if gadt_equations <> [] then
          Gadt.apply_equations gadt_equations result_type
        else
          result_type
      in
      unify ctx match_arm.arm_location expected_result_type typed_arm_expression.expression_type;
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

  | ExpressionPolyVariant (tag, arg_expr) ->
    (* Polymorphic variant expression: `Tag or `Tag expr
       Creates an open "at least" type: [> `Tag] or [> `Tag of ty] *)
    let typed_arg, arg_ty_opt, ctx = match arg_expr with
      | None -> (None, None, ctx)
      | Some e ->
        let typed_e, ctx = infer_expression ctx e in
        (Some typed_e, Some typed_e.expression_type, ctx)
    in
    (* Create a row variable for the open poly variant type *)
    let row_var, ctx = Typing_context.new_type_variable ctx in
    let pv_field = Types.PVFieldPresent arg_ty_opt in
    let pv_type = Types.type_poly_variant_at_least [(tag, pv_field)] ~row_var in
    ({
      expression_desc = TypedExpressionPolyVariant (tag, typed_arg);
      expression_type = pv_type;
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

(** [infer_expression_with_expected ctx expected_type expr] infers the type of an
    expression with an optional expected type for bidirectional type checking.

    For function expressions with an expected arrow type, this pushes the expected
    parameter types down to the pattern inference and expected result type to
    body inference. This is critical for polymorphic recursion with GADTs, where
    the expected parameter type (e.g., [a expr]) must be used to establish type
    equations during pattern matching.

    @param ctx The typing context
    @param expected_type Optional expected type for bidirectional checking
    @param expr The expression to infer
    @return A pair [(typed_expr, updated_ctx)] *)
and infer_expression_with_expected ctx expected_type (expr : expression) =
  match expr.Location.value, expected_type with
  | ExpressionFunction (param_patterns, body_expr), Some expected_ty ->
    (* Bidirectional type checking for functions *)
    let env = Typing_context.environment ctx in
    let loc = expr.Location.location in
    let ctx = Typing_context.enter_level ctx in
    (* Decompose expected arrow type to get parameter and result types *)
    let rec decompose_arrow n expected =
      if n = 0 then ([], expected)
      else match Types.representative expected with
        | Types.TypeArrow (param_ty, result_ty) ->
          let rest_params, final_result = decompose_arrow (n - 1) result_ty in
          (param_ty :: rest_params, final_result)
        | _ ->
          (* Expected type has fewer arrows than parameters - use fresh vars for rest *)
          ([], expected)
    in
    (* Count real parameters (excluding locally abstract types) *)
    let real_param_count = List.fold_left (fun count p ->
      match p.Location.value with
      | PatternLocallyAbstract _ -> count  (* Not a runtime parameter *)
      | _ -> count + 1
    ) 0 param_patterns in
    let expected_param_tys, expected_result_ty =
      decompose_arrow real_param_count expected_ty
    in
    (* Infer patterns and unify with expected parameter types *)
    let typed_params, param_types, inner_ctx =
      let expected_param_iter = ref expected_param_tys in
      List.fold_left (fun (pats, tys, ctx) p ->
        let pat, inferred_ty, ctx = Pattern_infer.infer_pattern ctx p in
        (* Skip locally abstract types for parameter matching *)
        let tys = match pat.pattern_desc with
          | TypedPatternLocallyAbstract _ -> tys
          | _ ->
            (* Unify with expected if available *)
            begin match !expected_param_iter with
            | expected_param_ty :: rest ->
              expected_param_iter := rest;
              Inference_utils.unify_with_env env loc inferred_ty expected_param_ty;
              inferred_ty :: tys
            | [] ->
              (* No more expected types - just use inferred *)
              inferred_ty :: tys
            end
        in
        (pat :: pats, tys, ctx)
      ) ([], [], ctx) param_patterns
    in
    let typed_params = List.rev typed_params in
    let param_types = List.rev param_types in
    (* Infer body with expected result type *)
    let typed_body, _inner_ctx =
      infer_expression_with_expected inner_ctx (Some expected_result_ty) body_expr
    in
    (* Check for existential type escape in function parameters.
       Existential type variables introduced by GADT patterns in parameters
       cannot appear in the function's return type. *)
    let existential_ids = List.concat_map Gadt.collect_existentials_from_pattern typed_params in
    begin match Gadt.check_existential_escape existential_ids typed_body.expression_type with
    | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
    | None -> ()
    end;
    let ctx = Typing_context.leave_level ctx in
    let func_ty =
      List.fold_right (fun param_ty acc ->
        Types.TypeArrow (param_ty, acc)
      ) param_types typed_body.expression_type
    in
    ({
      expression_desc = TypedExpressionFunction (typed_params, typed_body);
      expression_type = func_ty;
      expression_location = loc;
    }, ctx)

  | ExpressionMatch (scrutinee_expression, match_arms), Some expected_ty
    when Gadt.has_rigid_variables expected_ty ->
    (* Special handling for match expressions with expected types containing rigid variables.
       This is critical for GADT pattern matching where the return type is a locally
       abstract type that gets refined in each branch. *)
    let loc = expr.Location.location in
    let env = Typing_context.environment ctx in
    let typed_scrutinee, ctx = infer_expression ctx scrutinee_expression in
    let scrutinee_type = typed_scrutinee.expression_type in
    (* Type-check each match arm *)
    let typed_match_arms, ctx = List.fold_left (fun (arms, ctx) match_arm ->
      let typed_pattern, pattern_type, arm_ctx = Pattern_infer.infer_pattern ctx match_arm.arm_pattern in
      (* Extract GADT equations.
         For GADT branches, we DON'T unify scrutinee with pattern type directly
         because that would try to unify rigid type variables. Instead, equations
         capture the type refinements that happen in each branch. *)
      let gadt_equations =
        let extraction = Gadt.extract_equations scrutinee_type pattern_type in
        if extraction.success then extraction.equations else []
      in
      (* For GADT matches, skip direct unification of scrutinee with pattern type.
         The equations capture the relationship. For non-GADT parts, unify separately. *)
      if gadt_equations = [] then
        unify ctx match_arm.arm_location scrutinee_type pattern_type;
      (* Type-check guard if present *)
      let typed_guard, arm_ctx = match match_arm.arm_guard with
        | None -> (None, arm_ctx)
        | Some guard_expression ->
          let typed_guard_expression, arm_ctx = infer_expression arm_ctx guard_expression in
          unify arm_ctx guard_expression.Location.location type_bool typed_guard_expression.expression_type;
          (Some typed_guard_expression, arm_ctx)
      in
      (* Type-check arm body *)
      let typed_arm_expression, _arm_ctx = infer_expression arm_ctx match_arm.arm_expression in
      (* Check for existential type escape.
         Existential type variables introduced by a GADT pattern cannot appear
         in the result type of the match branch. *)
      let existential_ids = Gadt.collect_existentials_from_pattern typed_pattern in
      begin match Gadt.check_existential_escape existential_ids typed_arm_expression.expression_type with
      | Some _escaped_id ->
        Compiler_error.type_error match_arm.arm_location
          "This expression has a type containing an existential type variable \
           that would escape its scope"
      | None -> ()
      end;
      (* Apply GADT equations to expected type.
         For GADT branches, this transforms 'a' into 'int' when we have equation a=int. *)
      let expected_branch_type = Gadt.apply_equations gadt_equations expected_ty in
      unify ctx match_arm.arm_location expected_branch_type typed_arm_expression.expression_type;
      let typed_arm = {
        Typed_tree.typed_arm_pattern = typed_pattern;
        typed_arm_guard = typed_guard;
        typed_arm_expression;
        typed_arm_location = match_arm.arm_location;
      } in
      (typed_arm :: arms, ctx)
    ) ([], ctx) match_arms in
    let typed_match_arms = List.rev typed_match_arms in
    Pattern_check.check_match env loc scrutinee_type typed_match_arms;
    ({
      expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
      expression_type = expected_ty;
      expression_location = loc;
    }, ctx)

  | _, Some expected_ty ->
    (* Other expressions: infer then unify with expected *)
    let typed_expr, ctx = infer_expression ctx expr in
    let env = Typing_context.environment ctx in
    Inference_utils.unify_with_env env expr.Location.location
      typed_expr.expression_type expected_ty;
    (typed_expr, ctx)

  | _, None ->
    (* No expected type: regular inference *)
    infer_expression ctx expr

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
        (* Check for TypeForall annotation (e.g., let f : type a. a -> a = ...) *)
        match extract_poly_rec_annotation binding with
        | Some (name, forall_vars, body_ty_expr, _loc) ->
          (* Non-recursive binding with TypeForall: similar to poly-rec but simpler *)
          let scheme, rigid_vars, ctx = check_forall_annotation ctx forall_vars body_ty_expr in
          let id = Identifier.create name in
          (* Create fresh rigid type variables for the expected type so that
             the original scheme's type variables remain unlinked. This ensures
             that later instantiation of the scheme works correctly. *)
          let fresh_rigid_vars, ctx =
            List.fold_left (fun (acc, ctx) (var_name, tv) ->
              let fresh, ctx = Typing_context.new_rigid_type_variable ctx in
              let fresh_tv = match fresh with Types.TypeVariable ftv -> ftv | _ -> assert false in
              ((var_name, tv.Types.id, fresh_tv) :: acc, ctx)
            ) ([], ctx) rigid_vars
          in
          let expected_type =
            Type_traversal.map (fun ty ->
              match Types.representative ty with
              | Types.TypeVariable tv ->
                begin match List.find_opt (fun (_, orig_id, _) -> orig_id = tv.Types.id) fresh_rigid_vars with
                | Some (_, _, fresh_tv) -> Types.TypeVariable fresh_tv
                | None -> ty
                end
              | _ -> ty
            ) scheme.Types.body
          in
          (* Add fresh rigid vars as type aliases for body checking *)
          let body_env =
            List.fold_left (fun env (var_name, _, fresh_tv) ->
              let decl = {
                Types.declaration_name = var_name;
                declaration_parameters = [];
                declaration_variances = [];
                declaration_manifest = Some (Types.TypeVariable fresh_tv);
                declaration_kind = Types.DeclarationAbstract;
                declaration_private = false;
                declaration_constraints = [];
              } in
              Environment.add_type var_name decl env
            ) (Typing_context.environment ctx) fresh_rigid_vars
          in
          let body_ctx = Typing_context.with_environment body_env ctx in
          let typed_expr, _body_ctx =
            infer_expression_with_expected body_ctx (Some expected_type) binding.binding_expression
          in
          let ctx = Typing_context.leave_level ctx in
          let env = Typing_context.environment ctx in
          let env = Environment.add_value name id scheme binding.binding_location env in
          let ctx = Typing_context.with_environment env ctx in
          let ctx = Typing_context.enter_level ctx in
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = scheme.body;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: bs, ctx)

        | None ->
          (* Standard non-recursive binding *)
          let typed_expr, ctx = infer_expression ctx binding.binding_expression in
          let ctx = Typing_context.leave_level ctx in
          let level = Typing_context.current_level ctx in
          let ctx = Typing_context.enter_level ctx in
          let typed_pat, pat_ty, ctx = Pattern_infer.infer_pattern ctx binding.binding_pattern in
          unify ctx binding.binding_location pat_ty typed_expr.expression_type;
          let env = Typing_context.environment ctx in
          (* For all patterns (including tuple/record patterns), we need to generalize
             each variable binding. Pattern_infer adds bindings with trivial schemes,
             so we need to re-add them with properly generalized schemes.
             Apply value restriction: only generalize if expression is a value. *)
          let env =
            let pattern_bindings = collect_pattern_bindings typed_pat in
            List.fold_left (fun env (name, id, ty) ->
              let binding_scheme =
                Inference_utils.compute_binding_scheme_with_env ~level ~env typed_expr ty
              in
              Environment.add_value name id binding_scheme binding.binding_location env
            ) env pattern_bindings
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

    (* First pass: analyze bindings and set up environment *)
    let env, binding_info_list, ctx =
      List.fold_left (fun (env, info_list, ctx) (binding : binding) ->
        match extract_poly_rec_annotation binding with
        | Some (name, forall_vars, body_ty_expr, _loc) ->
          (* Polymorphic recursion: create scheme from annotation *)
          let scheme, rigid_vars, ctx = check_forall_annotation ctx forall_vars body_ty_expr in
          let id = Identifier.create name in
          (* Add with full scheme so recursive calls instantiate fresh vars *)
          let env = Environment.add_value name id scheme binding.binding_location env in
          let info = PolyRecBinding { name; id; scheme; rigid_vars; binding } in
          (env, info :: info_list, ctx)

        | None ->
          (* Standard recursive binding *)
          begin match binding.binding_pattern.Location.value with
          | PatternVariable name ->
            let mono_type, ctx = Typing_context.new_type_variable ctx in
            let id = Identifier.create name in
            let env = Environment.add_value name id (trivial_scheme mono_type) binding.binding_location env in
            let info = StandardRecBinding { name; id; mono_type; binding } in
            (env, info :: info_list, ctx)
          | PatternConstraint (inner_pat, _ty_expr) ->
            (* Non-forall constraint - extract name and use standard approach *)
            begin match inner_pat.Location.value with
            | PatternVariable name ->
              let mono_type, ctx = Typing_context.new_type_variable ctx in
              let id = Identifier.create name in
              let env = Environment.add_value name id (trivial_scheme mono_type) binding.binding_location env in
              let info = StandardRecBinding { name; id; mono_type; binding } in
              (env, info :: info_list, ctx)
            | _ ->
              Compiler_error.type_error binding.binding_location
                "Recursive bindings must be simple variables"
            end
          | _ ->
            Compiler_error.type_error binding.binding_location
              "Recursive bindings must be simple variables"
          end
      ) (env, [], ctx) bindings
    in
    let binding_info_list = List.rev binding_info_list in
    let ctx = Typing_context.with_environment env ctx in

    (* Second pass: type-check bodies *)
    let typed_bindings, ctx =
      List.fold_left (fun (typed_bindings, ctx) info ->
        match info with
        | PolyRecBinding { name = _; id; scheme; rigid_vars; binding } ->
          (* Add rigid vars as type aliases for body checking *)
          let body_env =
            List.fold_left (fun env (var_name, tv) ->
              let decl = {
                Types.declaration_name = var_name;
                declaration_parameters = [];
                declaration_variances = [];
                declaration_manifest = Some (TypeVariable tv);
                declaration_kind = Types.DeclarationAbstract;
                declaration_private = false;
                declaration_constraints = [];
              } in
              Environment.add_type var_name decl env
            ) (Typing_context.environment ctx) rigid_vars
          in
          let body_ctx = Typing_context.with_environment body_env ctx in
          (* Type-check the body expression with expected type from annotation *)
          let typed_expr, _body_ctx =
            infer_expression_with_expected body_ctx (Some scheme.body) binding.binding_expression
          in
          (* Unify inferred type with annotated type (may be partially done already) *)
          unify ctx binding.binding_location scheme.body typed_expr.expression_type;
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = scheme.body;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: typed_bindings, ctx)

        | StandardRecBinding { name = _; id; mono_type; binding } ->
          (* Standard recursive binding *)
          let typed_expr, ctx = infer_expression ctx binding.binding_expression in
          unify ctx binding.binding_location mono_type typed_expr.expression_type;
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = mono_type;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: typed_bindings, ctx)
      ) ([], ctx) binding_info_list
    in
    let typed_bindings = List.rev typed_bindings in

    (* Third pass: generalize and update environment *)
    let ctx = Typing_context.leave_level ctx in
    let level = Typing_context.current_level ctx in
    let env = Typing_context.environment ctx in
    let env =
      List.fold_left2 (fun env info typed_binding ->
        match info with
        | PolyRecBinding { name; id; scheme; rigid_vars = _; binding = _ } ->
          (* Poly-rec binding: keep the annotation scheme *)
          Environment.add_value name id scheme typed_binding.binding_location env

        | StandardRecBinding { name; id; mono_type; binding = _ } ->
          (* Standard binding: generalize as normal *)
          let scheme = Inference_utils.compute_binding_scheme_with_env ~level ~env typed_binding.binding_expression mono_type in
          Environment.add_value name id scheme typed_binding.binding_location env
      ) env binding_info_list typed_bindings
    in
    let ctx = Typing_context.with_environment env ctx in
    (typed_bindings, ctx)
