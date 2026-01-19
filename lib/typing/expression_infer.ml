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
    Compiler_error.internal_error
      "infer_module_expression not initialized - Structure_infer must set this")

(** Unify types using context's environment for alias expansion. *)
let unify = Inference_utils.unify

(** Check if a name is a binary operator. *)
let is_binary_operator name =
  match name with
  | "+" | "-" | "*" | "/" | "^" | "==" | "!=" | "<" | ">" | "<=" | ">="
  | "+." | "-." | "*." | "/." | "&&" | "||" -> true
  | _ -> false

(** Describe an operator's expected operand type. *)
let operator_type_description op_name =
  match op_name with
  | "+" | "-" | "*" | "/" -> "integers"
  | "+." | "-." | "*." | "/." -> "floats"
  | "^" -> "strings"
  | "==" | "!=" | "<" | ">" | "<=" | ">=" -> "comparable values"
  | "&&" | "||" -> "booleans"
  | _ -> "values"

(** Unify function argument with context-aware error message.
    @param ctx Typing context
    @param arg_loc Location of the argument expression
    @param param_label The parameter's label (Nolabel, Labelled, Optional)
    @param arg_index 1-based index of the argument
    @param operator_name Optional name of the operator (for operator-specific messages)
    @param param_ty Expected parameter type
    @param arg_ty Actual argument type *)
let unify_function_argument ctx arg_loc ~param_label ~arg_index ?operator_name param_ty arg_ty =
  try
    unify ctx arg_loc param_ty arg_ty
  with Unification.Unification_error err ->
    let ordinal index =
      match index with
      | 1 -> "1st" | 2 -> "2nd" | 3 -> "3rd"
      | n -> Printf.sprintf "%dth" n
    in
    let base_message = match operator_name with
      | Some op when is_binary_operator op ->
        let side = if arg_index = 1 then "left" else "right" in
        let expected_desc = operator_type_description op in
        Printf.sprintf "The `%s` operator expects %s, but the %s operand has type `%s`."
          op expected_desc side (Types.type_expression_to_string arg_ty)
      | _ ->
        let arg_desc = match param_label with
          | Types.Labelled name -> Printf.sprintf "labeled argument `~%s`" name
          | Types.Optional name -> Printf.sprintf "optional argument `?%s`" name
          | Types.Nolabel -> Printf.sprintf "%s argument" (ordinal arg_index)
        in
        Printf.sprintf "This function's %s expects type `%s`, but you gave it `%s`."
          arg_desc
          (Types.type_expression_to_string param_ty)
          (Types.type_expression_to_string arg_ty)
    in
    let _, _, hint = Type_explain.explain_mismatch ~expected:param_ty ~actual:arg_ty in
    let message = match hint with
      | Some hint_text -> Printf.sprintf "%s\n\n%s" base_message hint_text
      | None -> base_message
    in
    raise (Unification.Unification_error { err with message; location = arg_loc })

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

(** Extract parameter labels from a function type.
    Returns a list of (label, param_type) pairs and the remaining result type.

    @param max_count Optional limit on parameters to extract. If None, extracts all.
                     Used to avoid treating nested function returns as parameters. *)
let rec extract_arrow_params ?max_count ty =
  match max_count with
  | Some 0 -> ([], ty)
  | _ ->
    match Types.representative ty with
    | Types.TypeArrow (label, param_ty, result_ty) ->
      let next_count = Option.map (fun n -> n - 1) max_count in
      let rest_params, final_result = extract_arrow_params ?max_count:next_count result_ty in
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
  | Types.Labelled s1, Types.Optional s2 -> s1 = s2
  | Types.Optional s1, Types.Labelled s2 -> s1 = s2
  | _ -> false

(** Find and remove an argument matching the given label from a list.
    Returns [(found_opt, remaining_args)] where [found_opt] is the matching arg if any. *)
let rec find_and_remove_labeled_arg label args =
  match args with
  | [] -> (None, [])
  | (arg_label, arg) :: rest ->
      if labels_match label arg_label then (Some (arg_label, arg), rest)
      else
        let found, remaining = find_and_remove_labeled_arg label rest in
        (found, (arg_label, arg) :: remaining)

(** {2 Optional Argument Wrapping}

    These helpers handle wrapping arguments for optional parameters.
    Used in both full and partial application. *)

(** Wrap an argument expression in [Some] constructor.
    Used when a labeled argument fills an optional parameter. *)
let wrap_in_some arg =
  let option_ty = Types.TypeConstructor (Types.PathLocal "option", [arg.Typed_tree.expression_type]) in
  {
    Typed_tree.expression_desc = TypedExpressionConstructor (Environment.some_constructor, Some arg);
    expression_type = option_ty;
    expression_location = arg.expression_location;
  }

(** Create a [None] expression for an optional parameter with no argument.
    @param loc Location for the None expression
    @param inner_ty The inner type of the option (the parameter's expected type) *)
let make_none_arg loc inner_ty =
  let option_ty = Types.TypeConstructor (Types.PathLocal "option", [inner_ty]) in
  {
    Typed_tree.expression_desc = TypedExpressionConstructor (Environment.none_constructor, None);
    expression_type = option_ty;
    expression_location = loc;
  }

(** Result of resolving a slot in function application. *)
type resolved_slot =
  | SlotResolved of Typed_tree.typed_expression
  | SlotPending of Types.type_expression

(** Resolve a slot to either a filled argument or a pending type.
    Handles wrapping for optional parameters. *)
let resolve_slot loc = function
  | `Filled arg -> SlotResolved arg
  | `FilledWrapped arg -> SlotResolved (wrap_in_some arg)
  | `OptionalDefault inner_ty -> SlotResolved (make_none_arg loc inner_ty)
  | `Needed ty -> SlotPending ty

(** Reorder arguments to match the expected parameter order.
    Returns (matched_args, unused_args) where:
    - matched_args: slots in parameter order
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
  let unlabeled_args, labeled_args =
    List.partition (fun (l, _) -> match l with Types.Nolabel -> true | _ -> false) provided_args
  in

  let match_param_to_slot (param_label, param_ty) unlabeled labeled =
    match param_label with
    | Types.Nolabel ->
        begin match unlabeled with
        | [] -> ((param_label, `Needed param_ty), unlabeled, labeled)
        | (_, arg) :: rest -> ((param_label, `Filled arg), rest, labeled)
        end

    | Types.Labelled _ ->
        let found, remaining = find_and_remove_labeled_arg param_label labeled in
        begin match found with
        | Some (_, arg) -> ((param_label, `Filled arg), unlabeled, remaining)
        | None -> ((param_label, `Needed param_ty), unlabeled, remaining)
        end

    | Types.Optional _ ->
        let found, remaining = find_and_remove_labeled_arg param_label labeled in
        begin match found with
        | Some (arg_label, arg) ->
            let slot = match arg_label with
              | Types.Labelled _ -> (param_label, `FilledWrapped arg)
              | _ -> (param_label, `Filled arg)
            in
            (slot, unlabeled, remaining)
        | None ->
            ((param_label, `OptionalDefault param_ty), unlabeled, remaining)
        end
  in

  let slots_rev, remaining_unlabeled, remaining_labeled =
    List.fold_left (fun (slots, unlabeled, labeled) param ->
      let slot, unlabeled', labeled' = match_param_to_slot param unlabeled labeled in
      (slot :: slots, unlabeled', labeled')
    ) ([], unlabeled_args, labeled_args) expected_params
  in

  (List.rev slots_rev, remaining_unlabeled @ remaining_labeled)

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

(** {2 Application Inference}

    Function application with type-directed argument reordering.
    Handles labeled/optional arguments and partial application. *)

(** Validate that there are no unused arguments after reordering.
    Raises an appropriate type error if unused arguments are present. *)
let validate_unused_arguments loc unused_args =
  if unused_args = [] then ()
  else
    let unused_unlabeled =
      List.filter (fun (l, _) -> match l with Types.Nolabel -> true | _ -> false) unused_args
    in
    let unused_labeled =
      List.filter (fun (l, _) -> match l with Types.Nolabel -> false | _ -> true) unused_args
    in

    if unused_unlabeled <> [] then
      Compiler_error.type_error loc
        (Printf.sprintf "This function is applied to too many arguments (%d extra)"
          (List.length unused_unlabeled))
    else
      let unused_names = List.map (fun (label, _) ->
        Inference_utils.format_arg_label label
      ) unused_labeled in
      Compiler_error.type_error loc
        (Printf.sprintf "This function has no parameter with label %s"
          (String.concat ", " unused_names))

(** Build a full application expression from filled slots. *)
let build_full_application loc typed_func slots base_result_ty =
  let ordered_args = List.map (fun (label, slot) ->
    match resolve_slot loc slot with
    | SlotResolved arg -> (label, arg)
    | SlotPending _ ->
        Compiler_error.internal_error "Pending slot in full application"
  ) slots in

  {
    Typed_tree.expression_desc = Typed_tree.TypedExpressionApply (typed_func, ordered_args);
    expression_type = base_result_ty;
    expression_location = loc;
  }

(** Build a partial application expression from slots with unfilled params. *)
let build_partial_application loc typed_func slots base_result_ty =
  let typed_slots = List.map (fun (label, slot) ->
    match resolve_slot loc slot with
    | SlotResolved arg -> (label, Typed_tree.SlotFilled arg)
    | SlotPending ty -> (label, Typed_tree.SlotNeeded ty)
  ) slots in

  let result_ty =
    List.fold_right (fun (label, slot) acc ->
      match resolve_slot loc slot with
      | SlotPending param_ty -> Types.TypeArrow (label, param_ty, acc)
      | SlotResolved _ -> acc
    ) slots base_result_ty
  in

  {
    Typed_tree.expression_desc = Typed_tree.TypedExpressionPartialApply {
      partial_func = typed_func;
      partial_slots = typed_slots;
    };
    expression_type = result_ty;
    expression_location = loc;
  }

(** Extract operator name from function expression if it's a simple variable operator. *)
let extract_operator_name func_expr =
  match func_expr.Location.value with
  | ExpressionVariable name when is_binary_operator name -> Some name
  | _ -> None

(** Infer type for function application.
    Handles labeled arguments, optional argument defaulting, and partial application.

    @param infer_expr Function to infer sub-expression types (for recursion)
    @param unify_fn Function to unify types
    @param ctx Typing context
    @param loc Source location
    @param func_expr The function being applied
    @param labeled_arg_exprs The argument expressions with labels *)
let rec infer_apply ~infer_expr ~unify_fn ctx loc func_expr labeled_arg_exprs =
  let operator_name = extract_operator_name func_expr in
  let typed_func, ctx = infer_expr ctx func_expr in

  let typed_labeled_args, ctx =
    List.fold_left (fun (acc, ctx) (label, arg_expr) ->
      let typed_arg, ctx = infer_expr ctx arg_expr in
      let types_label = Inference_utils.convert_syntax_label label in
      ((types_label, typed_arg) :: acc, ctx)
    ) ([], ctx) labeled_arg_exprs
  in
  let typed_labeled_args = List.rev typed_labeled_args in
  let num_provided_args = List.length typed_labeled_args in

  let has_labeled_arg = List.exists (fun (label, _) ->
    match label with Types.Nolabel -> false | _ -> true
  ) typed_labeled_args in

  let rec has_optional_param ty =
    match Types.representative ty with
    | Types.TypeArrow (Types.Optional _, _, _) -> true
    | Types.TypeArrow (_, _, result_ty) -> has_optional_param result_ty
    | _ -> false
  in
  let func_has_optional = has_optional_param typed_func.expression_type in

  let expected_params, base_result_ty =
    if has_labeled_arg || func_has_optional then
      extract_arrow_params typed_func.expression_type
    else
      extract_arrow_params ~max_count:num_provided_args typed_func.expression_type
  in

  if expected_params = [] then begin
    (* Function type is not an arrow - create fresh arrow type and unify *)
    let result_ty, ctx = Typing_context.new_type_variable ctx in
    let expected_func_ty =
      List.fold_right (fun (label, arg) acc ->
        Types.TypeArrow (label, arg.Typed_tree.expression_type, acc)
      ) typed_labeled_args result_ty
    in
    unify_fn ctx loc expected_func_ty typed_func.expression_type;
    ({
      Typed_tree.expression_desc = Typed_tree.TypedExpressionApply (typed_func, typed_labeled_args);
      expression_type = result_ty;
      expression_location = loc;
    }, ctx)
  end
  else begin
    let slots, unused_args = reorder_arguments expected_params typed_labeled_args in

    validate_unused_arguments loc unused_args;

    List.iteri (fun index ((param_label, param_ty), (_, slot)) ->
      match slot with
      | `Filled arg ->
        unify_function_argument ctx arg.Typed_tree.expression_location
          ~param_label ~arg_index:(index + 1) ?operator_name param_ty arg.Typed_tree.expression_type
      | `FilledWrapped arg ->
        unify_function_argument ctx arg.Typed_tree.expression_location
          ~param_label ~arg_index:(index + 1) ?operator_name param_ty arg.Typed_tree.expression_type
      | `OptionalDefault _ | `Needed _ -> ()
    ) (List.combine expected_params slots);

    let all_filled = List.for_all (fun (_, slot) ->
      match slot with
      | `Filled _ | `FilledWrapped _ | `OptionalDefault _ -> true
      | `Needed _ -> false
    ) slots in

    let result_expr =
      if all_filled then build_full_application loc typed_func slots base_result_ty
      else build_partial_application loc typed_func slots base_result_ty
    in
    (result_expr, ctx)
  end

(** [infer_expression ctx expr] infers the type of an expression.

    @param ctx The typing context
    @param expr The expression to infer
    @return A pair [(typed_expr, updated_ctx)] *)
and infer_expression ctx (expr : expression) =
  let env = Typing_context.environment ctx in
  let loc = expr.Location.location in
  match expr.Location.value with
  | ExpressionVariable name ->
    begin match Environment.find_value name env with
    | None -> Inference_utils.error_unbound_variable_with_env ~env loc name
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
    infer_apply ~infer_expr:infer_expression ~unify_fn:unify ctx loc func_expr labeled_arg_exprs

  | ExpressionFunction (labeled_param_patterns, body_expr) ->
    let ctx = Typing_context.enter_level ctx in
    let typed_labeled_params, labeled_param_types, inner_ctx =
      List.fold_left (fun (pats, tys, ctx) (label, p) ->
        let pat, inner_ty, ctx = Pattern_infer.infer_pattern ctx p in
        let types_label = Inference_utils.convert_syntax_label label in
        (* For optional parameters:
           - The arrow type uses the inner type T (so signature is ?x:T -> ...)
           - But inside the function, the binding has type T option

           This matches OCaml semantics where ?x:int means x has type int option
           inside the function body, but callers see ?x:int in the signature.

           Re-add bindings with option-wrapped type for optional params.
           Pattern inference already added bindings with inner type; we shadow them. *)
        let ctx, pat = match types_label with
          | Types.Optional _ ->
            let option_ty = Types.TypeConstructor (Types.PathLocal "option", [inner_ty]) in
            let ctx = match pat.pattern_desc with
              | Typed_tree.TypedPatternVariable id ->
                let name = Common.Identifier.name id in
                let env = Typing_context.environment ctx in
                let env = Environment.add_value name id
                  (Types.trivial_scheme option_ty)
                  pat.Typed_tree.pattern_location env in
                Typing_context.with_environment env ctx
              | _ -> ctx
            in
            let pat = { pat with Typed_tree.pattern_type = option_ty } in
            (ctx, pat)
          | _ ->
            (ctx, pat)
        in

        (* Locally abstract types don't contribute to the function signature -
           they only introduce a type into scope. Filter them out when building
           the parameter type list, but keep them in typed_params for the AST. *)
        let tys = match pat.Typed_tree.pattern_desc with
          | Typed_tree.TypedPatternLocallyAbstract _ -> tys
          | _ -> (types_label, inner_ty) :: tys
        in
        ((types_label, pat) :: pats, tys, ctx)
      ) ([], [], ctx) labeled_param_patterns
    in
    let typed_labeled_params = List.rev typed_labeled_params in
    let labeled_param_types = List.rev labeled_param_types in
    let typed_body, _inner_ctx = infer_expression inner_ctx body_expr in

    let param_patterns = List.map snd typed_labeled_params in
    Inference_utils.check_existential_escape_or_error loc param_patterns
      typed_body.expression_type;

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

    let binding_patterns = List.map (fun b -> b.Typed_tree.binding_pattern) typed_bindings in
    Inference_utils.check_existential_escape_or_error loc binding_patterns
      typed_body.expression_type;

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
    let open Module_types in
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules loc in

    Inference_utils.ensure_module_accessible loc mod_binding.binding_type;

    begin match mod_binding.binding_type with
    | ModTypeSig sig_ ->
      begin match find_value_in_sig value_name sig_ with
      | Some val_desc ->
        let ty, ctx = Typing_context.instantiate ctx val_desc.value_type in
        let internal_path = Module_type_check.module_path_to_internal_path base_binding.binding_id path_modules in
        ({ expression_desc = TypedExpressionModuleAccess (internal_path, value_name);
           expression_type = ty;
           expression_location = loc }, ctx)
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "Value %s not found in module" value_name)
      end
    | ModTypeFunctor _ | ModTypeIdent _ ->
      Compiler_error.internal_error
        "Expected signature after module accessibility check"
    end

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

  | ExpressionAssert cond_expr ->
    (* assert e : unit when e : bool *)
    let typed_cond, ctx = infer_expression ctx cond_expr in
    unify ctx loc typed_cond.expression_type Types.type_bool;
    ({
      expression_desc = TypedExpressionAssert typed_cond;
      expression_type = Types.type_unit;
      expression_location = loc;
    }, ctx)

  | ExpressionWhile (cond_expr, body_expr) ->
    (* while cond do body done : unit when cond : bool and body : unit *)
    let typed_cond, ctx = infer_expression ctx cond_expr in
    unify ctx loc typed_cond.expression_type Types.type_bool;
    let typed_body, ctx = infer_expression ctx body_expr in
    unify ctx loc typed_body.expression_type Types.type_unit;
    ({
      expression_desc = TypedExpressionWhile (typed_cond, typed_body);
      expression_type = Types.type_unit;
      expression_location = loc;
    }, ctx)

  | ExpressionFor (var_name, start_expr, end_expr, direction, body_expr) ->
    (* for i = start to/downto end do body done : unit
       var : int, start : int, end : int, body : unit *)
    let typed_start, ctx = infer_expression ctx start_expr in
    unify ctx loc typed_start.expression_type Types.type_int;
    let typed_end, ctx = infer_expression ctx end_expr in
    unify ctx loc typed_end.expression_type Types.type_int;

    let var_id = Identifier.create var_name in
    let var_scheme = Types.trivial_scheme Types.type_int in
    let env = Typing_context.environment ctx in
    let env' = Environment.add_value var_name var_id var_scheme loc env in
    let ctx' = Typing_context.with_environment env' ctx in

    let typed_body, _ctx' = infer_expression ctx' body_expr in
    unify ctx loc typed_body.expression_type Types.type_unit;
    ({
      expression_desc = TypedExpressionFor (var_id, typed_start, typed_end, direction, typed_body);
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

  | ExpressionLetOp (letop, bindings, body) ->
    (* Binding operators: desugar and infer the desugared form.

       let* p = e1 in e2  =>  ( let* ) e1 (fun p -> e2)

       let* p1 = e1 and* p2 = e2 in e3  =>
         ( let* ) (( and* ) e1 e2) (fun (p1, p2) -> e3)

       General case with n and-bindings:
         let* p1 = e1 and* p2 = e2 ... and* pn = en in body  =>
         ( let* ) (( and* ) ... (( and* ) e1 e2) ... en) (fun (((p1, p2), ...), pn) -> body)
    *)
    let mk_loc value = { Location.value; location = loc } in

    let mk_op_var op_name =
      mk_loc (ExpressionVariable op_name)
    in

    let mk_apply fn args =
      mk_loc (ExpressionApply (fn, List.map (fun arg -> (Parsing.Syntax_tree.Nolabel, arg)) args))
    in

    let mk_fun pattern body =
      mk_loc (ExpressionFunction ([(Parsing.Syntax_tree.Nolabel, pattern)], body))
    in

    let mk_tuple_pattern p1 p2 =
      mk_loc (PatternTuple [p1; p2])
    in

    (* Combine bindings using and* operators into a single expression and pattern *)
    let rec combine_and_bindings (combined_expr, combined_pattern) rest_bindings =
      match rest_bindings with
      | [] -> (combined_expr, combined_pattern)
      | binding :: rest ->
        match binding.letop_and with
        | None ->
          (* Should not happen - first binding shouldn't have and* *)
          Compiler_error.internal_error "First binding in let-op should not have and-operator"
        | Some andop ->
          let andop_var = mk_op_var andop in
          let new_expr = mk_apply andop_var [combined_expr; binding.letop_expression] in
          let new_pattern = mk_tuple_pattern combined_pattern binding.letop_pattern in
          combine_and_bindings (new_expr, new_pattern) rest
    in

    (* Get the first binding and remaining bindings *)
    let first_binding, rest_bindings = match bindings with
      | [] -> Compiler_error.internal_error "Empty bindings in let-op"
      | first :: rest -> (first, rest)
    in

    let combined_expr, combined_pattern =
      combine_and_bindings (first_binding.letop_expression, first_binding.letop_pattern) rest_bindings
    in

    (* Construct the final desugared expression:
       ( letop ) combined_expr (fun combined_pattern -> body) *)
    let letop_var = mk_op_var letop in
    let callback_fn = mk_fun combined_pattern body in
    let desugared = mk_apply letop_var [combined_expr; callback_fn] in

    (* Type-check the desugared expression *)
    infer_expression ctx desugared

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
  Context_fold.fold_map infer_expression ctx exprs

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
        let types_label = Inference_utils.convert_syntax_label label in
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
