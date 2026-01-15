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

(** Forward reference for module expression inference.
    Set by Structure_infer to break circular dependency. *)
let infer_module_expression_ref :
    (Typing_context.t -> module_expression -> Typed_tree.typed_module_expression * Typing_context.t) ref =
  ref (fun _ctx _mexpr ->
    failwith "infer_module_expression not initialized - Structure_infer must set this")

(** Unify types using context's environment for alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** Type of constant literals *)
let type_of_constant = Inference_utils.type_of_constant

(** Convert a module_path (string list) to a Types.path *)
let module_path_to_types_path (mpath : Parsing.Syntax_tree.module_path) : Types.path =
  match mpath.Location.value with
  | [] -> Types.PathLocal "(empty)"
  | [name] -> Types.PathLocal name
  | first :: rest ->
    List.fold_left
      (fun acc name -> Types.PathDot (acc, name))
      (Types.PathLocal first)
      rest

(** Convert a syntactic module type path to a Types.path for package types.
    Used for printing/identifying the module type in first-class modules. *)
let rec module_type_to_path (mty : Parsing.Syntax_tree.module_type) : Types.path =
  match mty.Location.value with
  | ModuleTypePath path ->
    module_path_to_types_path path
  | ModuleTypeSignature _ ->
    (* Anonymous signature - use a placeholder path *)
    Types.PathLocal "(anonymous sig)"
  | ModuleTypeFunctor _ ->
    Types.PathLocal "(functor)"
  | ModuleTypeWith (base_mty, _) ->
    (* With constraints - use the base module type's path *)
    module_type_to_path base_mty
  | ModuleTypeOf _ ->
    (* module type of M - use a placeholder path *)
    Types.PathLocal "(module type of)"

(** Convert a longident to a Types.path.
    longident is (longident_desc Location.located). *)
and longident_to_path (lid : Parsing.Syntax_tree.longident) : Types.path =
  match lid.Location.value with
  | Parsing.Syntax_tree.Lident name ->
    Types.PathLocal name
  | Parsing.Syntax_tree.Ldot (prefix, name) ->
    Types.PathDot (longident_to_path prefix, name)

(** Extract up to [max_count] parameter labels from a function type.
    Returns a list of (label, param_type) pairs and the remaining result type.
    This limits extraction to avoid treating nested function returns as parameters. *)
let rec extract_arrow_params_limited max_count ty =
  if max_count <= 0 then ([], ty)
  else match Types.representative ty with
    | Types.TypeArrow (label, param_ty, result_ty) ->
      let rest_params, final_result = extract_arrow_params_limited (max_count - 1) result_ty in
      ((label, param_ty) :: rest_params, final_result)
    | _ -> ([], ty)

(** Check if two labels match for argument reordering.
    Labelled "x" matches Labelled "x", Optional "x" matches Optional "x".
    Additionally, Labelled "x" matches Optional "x" (passing non-optional value to optional param). *)
let labels_match l1 l2 =
  match l1, l2 with
  | Types.Nolabel, Types.Nolabel -> true
  | Types.Labelled s1, Types.Labelled s2 -> s1 = s2
  | Types.Optional s1, Types.Optional s2 -> s1 = s2
  | Types.Labelled s1, Types.Optional s2 -> s1 = s2  (* ~x can fill ?x *)
  | Types.Optional s1, Types.Labelled s2 -> s1 = s2  (* ?x can fill ~x *)
  | _ -> false

(** Reorder arguments to match the expected parameter order.
    Returns (matched_args, unmatched_params, unused_args) where:
    - matched_args: arguments in parameter order (for params that have matching args)
    - unmatched_params: parameters that have no matching argument (for partial application)
    - unused_args: arguments that don't match any parameter (should be empty or error)

    Slot types:
    - `Filled arg: argument provided directly
    - `FilledWrapped arg: labeled arg filling optional param (needs Some wrapping)
    - `OptionalDefault ty: optional param with no arg (needs None default)
    - `Needed ty: required param with no arg (partial application)

    Algorithm:
    - For each expected parameter, try to find a matching argument
    - Labeled params match labeled args with same label
    - Unlabeled params match unlabeled args in order
    - Optional params without args get default None *)
let reorder_arguments expected_params provided_args =
  (* Find and remove an argument matching the given label *)
  let rec find_and_remove_arg label args =
    match args with
    | [] -> None
    | (arg_label, arg) :: rest ->
      if labels_match label arg_label then
        Some ((arg_label, arg), rest)
      else
        match find_and_remove_arg label rest with
        | None -> None
        | Some (found, remaining) -> Some (found, (arg_label, arg) :: remaining)
  in

  (* Split provided args into unlabeled and labeled *)
  let unlabeled_args, labeled_args = List.partition (fun (l, _) ->
    match l with Types.Nolabel -> true | _ -> false
  ) provided_args in

  (* Track remaining unlabeled and labeled args as we consume them *)
  let unlabeled_ref = ref unlabeled_args in
  let labeled_ref = ref labeled_args in

  (* For each param, try to find a matching arg.
     Returns a list of slots (filled with arg or needing a value) *)
  let slots = List.map (fun (param_label, param_ty) ->
    match param_label with
    | Types.Nolabel ->
      (* Unlabeled param - match next unlabeled arg *)
      begin match !unlabeled_ref with
      | [] -> (param_label, `Needed param_ty)
      | (_, arg) :: rest ->
        unlabeled_ref := rest;
        (param_label, `Filled arg)
      end
    | Types.Labelled _ ->
      (* Labeled param - find matching labeled arg *)
      begin match find_and_remove_arg param_label !labeled_ref with
      | Some ((_, arg), rest) ->
        labeled_ref := rest;
        (param_label, `Filled arg)
      | None -> (param_label, `Needed param_ty)
      end
    | Types.Optional _ ->
      (* Optional param - find matching arg or use default None *)
      begin match find_and_remove_arg param_label !labeled_ref with
      | Some ((arg_label, arg), rest) ->
        labeled_ref := rest;
        (* If labeled arg fills optional param, it needs Some wrapping *)
        begin match arg_label with
        | Types.Labelled _ -> (param_label, `FilledWrapped arg)
        | _ -> (param_label, `Filled arg)
        end
      | None ->
        (* No arg provided - optional param gets None default *)
        (param_label, `OptionalDefault param_ty)
      end
  ) expected_params in

  (* Remaining args are unused *)
  let unused_args = !unlabeled_ref @ !labeled_ref in

  (slots, unused_args)

(** Check if a path starts with the given module identifier.
    Used to detect when abstract types from local modules escape their scope. *)
let rec path_is_rooted_at module_id path =
  match path with
  | Types.PathIdent id -> Identifier.equal id module_id
  | Types.PathDot (parent, _) -> path_is_rooted_at module_id parent
  | Types.PathApply (func, _) -> path_is_rooted_at module_id func
  | Types.PathLocal _ | Types.PathBuiltin _ -> false

(** Check if a type contains paths rooted at the given module identifier.
    Returns Some path if an abstract type would escape, None otherwise.
    This is used to detect when types like M.t would escape the scope of
    a local module binding [let module M = ... in ...]. *)
let check_module_type_escape module_id ty =
  let found = ref None in
  Type_traversal.iter (fun t ->
    match t with
    | Types.TypeConstructor (path, _) ->
      if path_is_rooted_at module_id path then
        found := Some path
    | _ -> ()
  ) ty;
  !found

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

  | ExpressionConstructor (longident, arg_expr) ->
    let result = Inference_utils.lookup_constructor_longident ctx loc longident in
    (* Check if the type is private - private types cannot be constructed *)
    Inference_utils.check_private_type ctx loc result.constructor_info;
    let ctor_name = result.constructor_info.Types.constructor_name in
    Inference_utils.check_constructor_arity loc ctor_name
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

  | ExpressionApply (func_expr, labeled_arg_exprs) ->
    (* Type-directed argument reordering:
       1. Infer function type first
       2. Extract expected parameter labels from function type
       3. Infer and reorder arguments to match expected order
       4. Unify and build result *)
    let typed_func, ctx = infer_expression ctx func_expr in

    (* Infer types for all provided arguments *)
    let typed_labeled_args, ctx =
      List.fold_left (fun (acc, ctx) (label, arg_expr) ->
        let typed_arg, ctx = infer_expression ctx arg_expr in
        let types_label = match label with
          | Parsing.Syntax_tree.Nolabel -> Types.Nolabel
          | Parsing.Syntax_tree.Labelled s -> Types.Labelled s
          | Parsing.Syntax_tree.Optional s -> Types.Optional s
        in
        ((types_label, typed_arg) :: acc, ctx)
      ) ([], ctx) labeled_arg_exprs
    in
    let typed_labeled_args = List.rev typed_labeled_args in
    let num_provided_args = List.length typed_labeled_args in

    (* Check if any argument has a label - this affects how many params we extract *)
    let has_labeled_arg = List.exists (fun (label, _) ->
      match label with Types.Nolabel -> false | _ -> true
    ) typed_labeled_args in

    (* Check if function type has any optional parameters that might need defaulting *)
    let rec has_optional_param ty =
      match Types.representative ty with
      | Types.TypeArrow (Types.Optional _, _, _) -> true
      | Types.TypeArrow (_, _, result_ty) -> has_optional_param result_ty
      | _ -> false
    in
    let func_has_optional = has_optional_param typed_func.expression_type in

    (* Extract expected parameters from function type.
       - If any argument is labeled, or function has optional params, extract ALL params
         (for label matching across positions and optional param defaulting)
       - If all arguments are unlabeled and no optional params, extract only as many
         as we have arguments (to avoid treating nested function returns as parameters) *)
    let expected_params, base_result_ty =
      if has_labeled_arg || func_has_optional then
        (* Extract all params - we need to search for matching labels or default optionals *)
        let rec extract_all_arrow_params ty =
          match Types.representative ty with
          | Types.TypeArrow (label, param_ty, result_ty) ->
            let rest_params, final_result = extract_all_arrow_params result_ty in
            ((label, param_ty) :: rest_params, final_result)
          | _ -> ([], ty)
        in
        extract_all_arrow_params typed_func.expression_type
      else
        (* Only extract as many params as we have unlabeled args *)
        extract_arrow_params_limited num_provided_args typed_func.expression_type
    in

    if expected_params = [] then begin
      (* Function type is not an arrow - create fresh arrow type and unify *)
      let result_ty, ctx = Typing_context.new_type_variable ctx in
      let expected_func_ty =
        List.fold_right (fun (label, arg) acc ->
          TypeArrow (label, arg.expression_type, acc)
        ) typed_labeled_args result_ty
      in
      unify ctx loc expected_func_ty typed_func.expression_type;
      ({
        expression_desc = TypedExpressionApply (typed_func, typed_labeled_args);
        expression_type = result_ty;
        expression_location = loc;
      }, ctx)
    end
    else begin
      (* Match arguments to parameters - returns slots and unused args *)
      let slots, unused_args =
        reorder_arguments expected_params typed_labeled_args
      in

      (* Check for unused arguments (don't match any parameter) *)
      if unused_args <> [] then begin
        let unused_unlabeled = List.filter (fun (l, _) ->
          match l with Types.Nolabel -> true | _ -> false
        ) unused_args in
        let unused_labeled = List.filter (fun (l, _) ->
          match l with Types.Nolabel -> false | _ -> true
        ) unused_args in

        if unused_unlabeled <> [] then
          Compiler_error.type_error loc
            (Printf.sprintf "This function is applied to too many arguments (%d extra)"
              (List.length unused_unlabeled))
        else begin
          let unused_names = List.map (fun (l, _) ->
            match l with
            | Types.Labelled s -> "~" ^ s
            | Types.Optional s -> "?" ^ s
            | Types.Nolabel -> "_"
          ) unused_labeled in
          Compiler_error.type_error loc
            (Printf.sprintf "This function has no parameter with label %s"
              (String.concat ", " unused_names))
        end
      end;

      (* Unify filled slots with their expected parameter types.
         Slots are already in parameter order from reorder_arguments,
         so we use positional correspondence.

         For optional params, the expected type is T but we receive:
         - FilledWrapped: arg of type T (will be wrapped in Some)
         - OptionalDefault: will use None
         - Filled: arg of type T option (already wrapped) *)
      List.iter2 (fun (_, param_ty) (_, slot) ->
        match slot with
        | `Filled arg -> unify ctx loc param_ty arg.expression_type
        | `FilledWrapped arg ->
          (* Labeled arg filling optional param - arg has inner type,
             param_ty is the inner type (not option), unify directly *)
          unify ctx loc param_ty arg.expression_type
        | `OptionalDefault _ ->
          (* Optional with no arg - no unification needed, param gets None *)
          ()
        | `Needed _ -> ()
      ) expected_params slots;

      (* Check if all slots are filled (full application) or some are needed (partial) *)
      let all_filled = List.for_all (fun (_, slot) ->
        match slot with
        | `Filled _ | `FilledWrapped _ | `OptionalDefault _ -> true
        | `Needed _ -> false
      ) slots in

      if all_filled then begin
        (* Full application - extract args in order, handling optional wrapping *)
        let ordered_args = List.map (fun (label, slot) ->
          match slot with
          | `Filled arg -> (label, arg)
          | `FilledWrapped arg ->
            (* Wrap the argument in Some *)
            let some_ctor = Environment.some_constructor in
            let option_ty = Types.TypeConstructor (
              Types.PathLocal "option",
              [arg.expression_type]
            ) in
            let wrapped_arg = {
              Typed_tree.expression_desc =
                TypedExpressionConstructor (some_ctor, Some arg);
              expression_type = option_ty;
              expression_location = arg.expression_location;
            } in
            (label, wrapped_arg)
          | `OptionalDefault inner_ty ->
            (* Use None for missing optional argument *)
            let none_ctor = Environment.none_constructor in
            let option_ty = Types.TypeConstructor (
              Types.PathLocal "option",
              [inner_ty]
            ) in
            let none_arg = {
              Typed_tree.expression_desc =
                TypedExpressionConstructor (none_ctor, None);
              expression_type = option_ty;
              expression_location = loc;
            } in
            (label, none_arg)
          | `Needed _ -> failwith "impossible: all slots filled"
        ) slots in

        ({
          expression_desc = TypedExpressionApply (typed_func, ordered_args);
          expression_type = base_result_ty;
          expression_location = loc;
        }, ctx)
      end
      else begin
        (* Partial application - convert slots to typed tree format *)
        let typed_slots = List.map (fun (label, slot) ->
          match slot with
          | `Filled arg -> (label, Typed_tree.SlotFilled arg)
          | `FilledWrapped arg ->
            (* Wrap the argument in Some for partial application *)
            let some_ctor = Environment.some_constructor in
            let option_ty = Types.TypeConstructor (
              Types.PathLocal "option",
              [arg.expression_type]
            ) in
            let wrapped_arg = {
              Typed_tree.expression_desc =
                TypedExpressionConstructor (some_ctor, Some arg);
              expression_type = option_ty;
              expression_location = arg.expression_location;
            } in
            (label, Typed_tree.SlotFilled wrapped_arg)
          | `OptionalDefault inner_ty ->
            (* Use None for missing optional argument *)
            let none_ctor = Environment.none_constructor in
            let option_ty = Types.TypeConstructor (
              Types.PathLocal "option",
              [inner_ty]
            ) in
            let none_arg = {
              Typed_tree.expression_desc =
                TypedExpressionConstructor (none_ctor, None);
              expression_type = option_ty;
              expression_location = loc;
            } in
            (label, Typed_tree.SlotFilled none_arg)
          | `Needed ty -> (label, Typed_tree.SlotNeeded ty)
        ) slots in

        (* Compute result type from needed slots *)
        let result_ty =
          List.fold_right (fun (label, slot) acc ->
            match slot with
            | `Needed param_ty -> TypeArrow (label, param_ty, acc)
            | `Filled _ | `FilledWrapped _ | `OptionalDefault _ -> acc
          ) slots base_result_ty
        in

        ({
          expression_desc = TypedExpressionPartialApply {
            partial_func = typed_func;
            partial_slots = typed_slots;
          };
          expression_type = result_ty;
          expression_location = loc;
        }, ctx)
      end
    end

  | ExpressionFunction (labeled_param_patterns, body_expr) ->
    let ctx = Typing_context.enter_level ctx in
    let typed_labeled_params, labeled_param_types, inner_ctx =
      List.fold_left (fun (pats, tys, ctx) (label, p) ->
        let pat, inner_ty, ctx = Pattern_infer.infer_pattern ctx p in
        (* Convert Syntax_tree.arg_label to Types.arg_label *)
        let types_label = match label with
          | Parsing.Syntax_tree.Nolabel -> Types.Nolabel
          | Parsing.Syntax_tree.Labelled s -> Types.Labelled s
          | Parsing.Syntax_tree.Optional s -> Types.Optional s
        in
        (* For optional parameters:
           - The arrow type uses the inner type T (so signature is ?x:T -> ...)
           - But inside the function, the binding has type T option

           This matches OCaml semantics where ?x:int means x has type int option
           inside the function body, but callers see ?x:int in the signature.

           We need to re-add bindings with option-wrapped type for optional params.
           Pattern inference already added bindings with inner type; we shadow them. *)
        let binding_ty, ctx, pat = match types_label with
          | Types.Optional _ ->
            let option_ty = Types.TypeConstructor (Types.PathLocal "option", [inner_ty]) in
            (* Re-add bindings with option type (shadowing the inner type bindings) *)
            let ctx = match pat.pattern_desc with
              | Typed_tree.TypedPatternVariable id ->
                let name = Common.Identifier.name id in
                let env = Typing_context.environment ctx in
                let env = Environment.add_value name id
                  (Types.trivial_scheme option_ty)
                  pat.Typed_tree.pattern_location env in
                Typing_context.with_environment env ctx
              | _ ->
                (* For non-variable patterns in optional params, keep as-is.
                   This is rare - usually optional params are simple variables. *)
                ctx
            in
            let pat = { pat with Typed_tree.pattern_type = option_ty } in
            (option_ty, ctx, pat)
          | _ ->
            (inner_ty, ctx, pat)
        in

        (* Locally abstract types don't contribute to the function signature -
           they only introduce a type into scope. Filter them out when building
           the parameter type list, but keep them in typed_params for the AST. *)
        let tys = match pat.Typed_tree.pattern_desc with
          | Typed_tree.TypedPatternLocallyAbstract _ -> tys  (* Skip - no runtime parameter *)
          | _ ->
            (* Arrow type uses inner_ty (not binding_ty) for optional params *)
            (types_label, inner_ty) :: tys
        in
        let _ = binding_ty in  (* suppress unused warning - used in ctx update *)
        ((types_label, pat) :: pats, tys, ctx)
      ) ([], [], ctx) labeled_param_patterns
    in
    let typed_labeled_params = List.rev typed_labeled_params in
    let labeled_param_types = List.rev labeled_param_types in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in
    (* Check for existential type escape in function parameters.
       Existential type variables introduced by GADT patterns in parameters
       cannot appear in the function's return type. *)
    let existential_ids = List.concat_map (fun (_, pat) ->
      Gadt.collect_existentials_from_pattern pat
    ) typed_labeled_params in
    begin match Gadt.check_existential_escape existential_ids typed_body.expression_type with
    | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
    | None -> ()
    end;
    let ctx = Typing_context.leave_level ctx in
    let func_ty =
      List.fold_right (fun (label, param_ty) acc ->
        TypeArrow (label, param_ty, acc)
      ) labeled_param_types typed_body.expression_type
    in
    ({
      expression_desc = TypedExpressionFunction (typed_labeled_params, typed_body);
      expression_type = func_ty;
      expression_location = loc;
    }, ctx)

  | ExpressionLet (rec_flag, bindings, body_expr) ->
    let typed_bindings, inner_ctx = infer_bindings ctx rec_flag bindings in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in

    (* Check for existential type escape from GADT patterns in let bindings.
       Existential types introduced by GADT patterns cannot escape their scope. *)
    let existential_ids = List.concat_map (fun binding ->
      Gadt.collect_existentials_from_pattern binding.Typed_tree.binding_pattern
    ) typed_bindings in
    begin match Gadt.check_existential_escape existential_ids typed_body.expression_type with
    | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
    | None -> ()
    end;

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

  | ExpressionPack (module_expr, module_type) ->
    (* First-class module packing: (module ME : MT) *)
    (* 1. Infer the module expression *)
    let typed_mexpr, ctx = !infer_module_expression_ref ctx module_expr in

    (* 2. Check the target module type *)
    let target_mty, ctx = Module_type_check.check_module_type ctx module_type in

    (* 3. Perform signature matching *)
    let match_ctx = Inference_utils.make_match_context ctx in
    begin match Signature_match.match_module_type match_ctx loc typed_mexpr.module_type target_mty with
    | Ok () ->
      (* Matching succeeded - create the package type *)
      let package_path = module_type_to_path module_type in
      let package_type = Types.TypePackage {
        package_path;
        package_signature = [];  (* Type constraints would be extracted from with-constraints *)
      } in
      ({
        expression_desc = TypedExpressionPack (typed_mexpr, target_mty);
        expression_type = package_type;
        expression_location = loc;
      }, ctx)
    | Error err ->
      Compiler_error.type_error loc
        (Printf.sprintf "Module does not match signature: %s"
          (Signature_match.format_match_error err))
    end

  | ExpressionLetModule (name, module_expr, body) ->
    (* Local module binding: let module M = ME in body *)
    (* 1. Infer the module expression *)
    let typed_mexpr, ctx = !infer_module_expression_ref ctx module_expr in

    (* 2. Create a module binding and add to environment *)
    let module_id = Identifier.create name.Location.value in
    (* Strengthen the module type so abstract types become qualified (M.t) *)
    let module_path = Types.PathIdent module_id in
    let strengthened_mty = Signature_match.strengthen_module_type module_path typed_mexpr.module_type in
    let module_binding = Module_types.{
      binding_name = name.Location.value;
      binding_id = module_id;
      binding_type = strengthened_mty;
      binding_alias = None;
    } in
    let env = Typing_context.environment ctx in
    let env = Environment.add_module name.Location.value module_binding env in
    let ctx = Typing_context.with_environment env ctx in

    (* 3. Infer the body expression in the extended environment *)
    let typed_body, ctx = infer_expression ctx body in

    (* 4. Check for abstract type escape - M.t cannot escape scope of let module *)
    begin match check_module_type_escape module_id typed_body.expression_type with
    | Some escaped_path ->
      Compiler_error.type_error loc
        (Printf.sprintf "The type constructor %s would escape its scope"
          (Types.path_to_string escaped_path))
    | None -> ()
    end;

    ({
      expression_desc = TypedExpressionLetModule (module_id, typed_mexpr, typed_body);
      expression_type = typed_body.expression_type;
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
  | ExpressionFunction (labeled_param_patterns, body_expr), Some expected_ty ->
    (* Bidirectional type checking for functions *)
    let env = Typing_context.environment ctx in
    let loc = expr.Location.location in
    let ctx = Typing_context.enter_level ctx in
    (* Decompose expected arrow type to get parameter and result types *)
    let rec decompose_arrow n expected =
      if n = 0 then ([], expected)
      else match Types.representative expected with
        | Types.TypeArrow (label, param_ty, result_ty) ->
          let rest_params, final_result = decompose_arrow (n - 1) result_ty in
          ((label, param_ty) :: rest_params, final_result)
        | _ ->
          (* Expected type has fewer arrows than parameters - use fresh vars for rest *)
          ([], expected)
    in
    (* Count real parameters (excluding locally abstract types) *)
    let real_param_count = List.fold_left (fun count (_, p) ->
      match p.Location.value with
      | PatternLocallyAbstract _ -> count  (* Not a runtime parameter *)
      | _ -> count + 1
    ) 0 labeled_param_patterns in
    let expected_param_tys, expected_result_ty =
      decompose_arrow real_param_count expected_ty
    in
    (* Infer patterns and unify with expected parameter types *)
    let typed_labeled_params, labeled_param_types, inner_ctx =
      let expected_param_iter = ref expected_param_tys in
      List.fold_left (fun (pats, tys, ctx) (label, p) ->
        let pat, inferred_ty, ctx = Pattern_infer.infer_pattern ctx p in
        (* Convert Syntax_tree.arg_label to Types.arg_label *)
        let types_label = match label with
          | Parsing.Syntax_tree.Nolabel -> Types.Nolabel
          | Parsing.Syntax_tree.Labelled s -> Types.Labelled s
          | Parsing.Syntax_tree.Optional s -> Types.Optional s
        in
        (* Skip locally abstract types for parameter matching *)
        let tys = match pat.pattern_desc with
          | TypedPatternLocallyAbstract _ -> tys
          | _ ->
            (* Unify with expected if available *)
            begin match !expected_param_iter with
            | (_, expected_param_ty) :: rest ->
              expected_param_iter := rest;
              Inference_utils.unify_with_env env loc inferred_ty expected_param_ty;
              (types_label, inferred_ty) :: tys
            | [] ->
              (* No more expected types - just use inferred *)
              (types_label, inferred_ty) :: tys
            end
        in
        ((types_label, pat) :: pats, tys, ctx)
      ) ([], [], ctx) labeled_param_patterns
    in
    let typed_labeled_params = List.rev typed_labeled_params in
    let labeled_param_types = List.rev labeled_param_types in
    (* Infer body with expected result type *)
    let typed_body, _inner_ctx =
      infer_expression_with_expected inner_ctx (Some expected_result_ty) body_expr
    in
    (* Check for existential type escape in function parameters.
       Existential type variables introduced by GADT patterns in parameters
       cannot appear in the function's return type. *)
    let existential_ids = List.concat_map (fun (_, pat) ->
      Gadt.collect_existentials_from_pattern pat
    ) typed_labeled_params in
    begin match Gadt.check_existential_escape existential_ids typed_body.expression_type with
    | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
    | None -> ()
    end;
    let ctx = Typing_context.leave_level ctx in
    let func_ty =
      List.fold_right (fun (label, param_ty) acc ->
        Types.TypeArrow (label, param_ty, acc)
      ) labeled_param_types typed_body.expression_type
    in
    ({
      expression_desc = TypedExpressionFunction (typed_labeled_params, typed_body);
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
