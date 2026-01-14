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
    let result = Inference_utils.lookup_constructor ctx loc name in
    (* Check if the type is private - private types cannot be constructed *)
    Inference_utils.check_private_type ctx loc result.constructor_info;
    Inference_utils.check_constructor_arity loc name
      ~has_arg:(Option.is_some arg_expr)
      ~expects_arg:(Option.is_some result.expected_arg_type);
    let typed_arg, ctx = match arg_expr, result.expected_arg_type with
      | None, None -> (None, ctx)
      | Some expr, Some expected_ty ->
        let typed_expr, ctx = infer_expression ctx expr in
        unify ctx loc expected_ty typed_expr.expression_type;
        (Some typed_expr, ctx)
      | Some _, None | None, Some _ ->
        (* Constructor arity was already checked by check_constructor_arity *)
        Compiler_error.internal_error
          "Constructor arity mismatch after arity check"
    in
    ({
      expression_desc = TypedExpressionConstructor (result.constructor_info, typed_arg);
      expression_type = result.result_type;
      expression_location = loc;
    }, ctx)

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
    Control_infer.infer_if ~infer_expr:infer_expression ctx loc cond_expr then_expr else_expr_opt

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
    Record_infer.infer_record ~infer_expr:infer_expression ctx loc record_fields

  | ExpressionRecordAccess (record_expression, field_name) ->
    Record_infer.infer_record_access ~infer_expr:infer_expression ctx loc record_expression field_name

  | ExpressionRecordUpdate (base_expression, update_fields) ->
    Record_infer.infer_record_update ~infer_expr:infer_expression ctx loc base_expression update_fields

  | ExpressionMatch (scrutinee_expression, match_arms) ->
    Control_infer.infer_match ~infer_expr:infer_expression ctx loc scrutinee_expression match_arms

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
    (* Special handling for match expressions with expected types containing rigid variables. *)
    let loc = expr.Location.location in
    Control_infer.infer_match_with_expected ~infer_expr:infer_expression
      ctx loc expected_ty scrutinee_expression match_arms

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

    Delegates to [Binding_infer.infer_bindings] with the appropriate callbacks.

    @param ctx The typing context
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_ctx)] *)
and infer_bindings ctx rec_flag bindings =
  Binding_infer.infer_bindings
    ~infer_expr:infer_expression
    ~infer_expr_expected:infer_expression_with_expected
    ctx rec_flag bindings
