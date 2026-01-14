(** Module type checking.

    Handles conversion of syntactic module types to semantic module types,
    including signature checking and with-constraints.

    Type expression checking has been moved to {!Type_expression_check}. *)

open Common
open Parsing.Syntax_tree
open Types

(** Convert a syntax module path to an internal path *)
let module_path_to_internal_path base_id path_modules =
  match path_modules with
  | [] -> Types.PathIdent base_id
  | _ :: rest ->
    List.fold_left (fun acc name ->
      Types.PathDot (acc, name)
    ) (Types.PathIdent base_id) rest

(** Look up a module by path. Returns (base_module_binding, final_module_binding) *)
let lookup_module_path env path_modules loc =
  match path_modules with
  | [] -> Compiler_error.type_error loc "Empty module path"
  | [name] ->
    begin match Environment.find_module name env with
    | Some binding -> (binding, binding)
    | None -> Inference_utils.error_unbound_module loc name
    end
  | first :: rest ->
    let rec follow_path mod_type path =
      match path with
      | [] -> Compiler_error.type_error loc "Empty path in module lookup"
      | [name] ->
        begin match mod_type with
        | Module_types.ModTypeSig sig_ ->
          begin match Module_types.find_module_in_sig name sig_ with
          | Some mty ->
            let id = Identifier.create name in
            Module_types.{ binding_name = name; binding_id = id; binding_type = mty; binding_alias = None }
          | None ->
            Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" name)
          end
        | _ ->
          Compiler_error.type_error loc "Expected a signature"
        end
      | name :: rest ->
        begin match mod_type with
        | Module_types.ModTypeSig sig_ ->
          begin match Module_types.find_module_in_sig name sig_ with
          | Some mty -> follow_path mty rest
          | None ->
            Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" name)
          end
        | _ ->
          Compiler_error.type_error loc "Expected a signature"
        end
    in
    begin match Environment.find_module first env with
    | Some base_binding ->
      let final_binding = follow_path base_binding.binding_type rest in
      (base_binding, final_binding)
    | None -> Inference_utils.error_unbound_module loc first
    end

(** Check a syntax module type and convert to semantic module type.

    @param ctx The typing context
    @param mty The syntactic module type
    @return A pair [(module_type, updated_ctx)] *)
let rec check_module_type ctx (mty : module_type) : Module_types.module_type * Typing_context.t =
  let env = Typing_context.environment ctx in
  let loc = mty.Location.location in
  match mty.Location.value with
  | ModuleTypePath module_path ->
    (* Module type path like S or M.S *)
    let path_modules = module_path.Location.value in
    begin match path_modules with
    | [name] ->
      (* Simple module type name *)
      begin match Environment.find_module_type name env with
      | Some (Some resolved_mty) -> (resolved_mty, ctx)
      | Some None ->
        Compiler_error.type_error loc (Printf.sprintf "Module type %s is abstract" name)
      | None ->
        Inference_utils.error_unbound_module_type loc name
      end
    | path_parts ->
      (* Qualified path like M.N.S: module path is M.N, type name is S *)
      let rec split_last = function
        | [] -> failwith "impossible: empty path"
        | [x] -> ([], x)
        | x :: rest ->
          let (prefix, last) = split_last rest in
          (x :: prefix, last)
      in
      let (module_path, type_name) = split_last path_parts in
      begin match module_path with
      | [] ->
        (* Should never happen since pattern [name] is handled above *)
        Compiler_error.type_error loc "Invalid module type path"
      | _ ->
        let (_, final_binding) = lookup_module_path env module_path loc in
        begin match final_binding.binding_type with
        | Module_types.ModTypeSig sig_ ->
          begin match Module_types.find_module_type_in_sig type_name sig_ with
          | Some (Some resolved) -> (resolved, ctx)
          | Some None ->
            Compiler_error.type_error loc (Printf.sprintf "Module type %s is abstract" type_name)
          | None ->
            Compiler_error.type_error loc (Printf.sprintf "Module type %s not found in module" type_name)
          end
        | Module_types.ModTypeFunctor _ ->
          Compiler_error.type_error loc "Cannot find module type in functor"
        | Module_types.ModTypeIdent _ ->
          Compiler_error.type_error loc "Cannot find module type in abstract module"
        end
      end
    end

  | ModuleTypeSignature sig_items ->
    (* Explicit signature: sig ... end *)
    let semantic_sig, ctx = check_signature ctx sig_items in
    (Module_types.ModTypeSig semantic_sig, ctx)

  | ModuleTypeFunctor (params, result) ->
    (* Functor type: functor (X : S) -> MT *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc "Functor type must have at least one parameter"
    | [param] ->
      let parameter_name = param.functor_param_name.Location.value in
      let parameter_type, ctx = check_module_type ctx param.functor_param_type in
      let parameter_id = Identifier.create parameter_name in
      let param_binding = Module_types.{
        binding_name = parameter_name;
        binding_id = parameter_id;
        binding_type = parameter_type;
        binding_alias = None;
      } in
      (* Add parameter to environment for checking result type *)
      let result_env = Environment.add_module parameter_name param_binding env in
      let result_ctx = Typing_context.with_environment result_env ctx in
      let result_mty, ctx = check_module_type result_ctx result in
      let semantic_param = Module_types.{
        parameter_name;
        parameter_id;
        parameter_type;
      } in
      (Module_types.ModTypeFunctor (semantic_param, result_mty), ctx)
    | param :: rest ->
      (* Multiple parameters - desugar to nested functor type *)
      let inner_type = {
        Location.value = ModuleTypeFunctor (rest, result);
        location = loc;
      } in
      let single_parameter_type = {
        Location.value = ModuleTypeFunctor ([param], inner_type);
        location = loc;
      } in
      check_module_type ctx single_parameter_type
    end

  | ModuleTypeWith (base_mty, constraints) ->
    (* Check the base module type *)
    let base, ctx = check_module_type ctx base_mty in
    (* Apply each constraint *)
    let result, ctx =
      List.fold_left (fun (mty, ctx) constraint_ ->
        apply_with_constraint ctx mty constraint_ loc
      ) (base, ctx) constraints
    in
    (result, ctx)

(** Apply a single with-constraint to a module type *)
and apply_with_constraint ctx mty (constraint_ : Parsing.Syntax_tree.with_constraint) loc =
  match constraint_ with
  | WithType (path, params, type_expr) ->
    apply_type_constraint ctx mty path params type_expr loc
  | WithTypeDestructive (path, params, type_expr) ->
    apply_destructive_type_constraint ctx mty path params type_expr loc
  | WithModule (path, target) ->
    apply_module_constraint ctx mty path target loc

(** Apply a module constraint: with module M = N *)
and apply_module_constraint ctx mty path target loc =
  let env = Typing_context.environment ctx in
  (* Look up the target module in the environment *)
  let target_modules = target.Location.value in
  let (_, target_binding) = lookup_module_path env target_modules loc in
  let target_mty = target_binding.Module_types.binding_type in
  (* Apply the constraint to the signature *)
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig = refine_module_in_sig ctx sig_ path target_mty loc in
    (Module_types.ModTypeSig refined_sig, ctx)
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to functor type"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to abstract module type"

(** Refine a module in a signature by path *)
and refine_module_in_sig ctx sig_ (path : Parsing.Syntax_tree.longident) target_mty loc =
  match path.Location.value with
  | Lident name ->
    (* Direct module: with module M = N *)
    let found = ref false in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, _) when n = name ->
        found := true;
        Module_types.SigModule (n, target_mty)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" name);
    refined_sig
  | Ldot (prefix, name) ->
    (* Nested: with module M.N = P *)
    refine_nested_module_in_sig ctx sig_ prefix name target_mty loc

(** Refine a nested module in a signature *)
and refine_nested_module_in_sig ctx sig_ (prefix : Parsing.Syntax_tree.longident) mod_name target_mty loc =
  match prefix.Location.value with
  | Lident outer_name ->
    let found = ref false in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = outer_name ->
        found := true;
        begin match inner_mty with
        | Module_types.ModTypeSig inner_sig ->
          let inner_path = { Location.value = Parsing.Syntax_tree.Lident mod_name; location = loc } in
          let refined_inner_sig = refine_module_in_sig ctx inner_sig inner_path target_mty loc in
          Module_types.SigModule (n, Module_types.ModTypeSig refined_inner_sig)
        | _ ->
          Compiler_error.type_error loc "Cannot refine module in non-signature"
        end
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" outer_name);
    refined_sig
  | Ldot (deeper_prefix, outer_name) ->
    (* Even deeper nesting - recurse *)
    let found = ref false in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = outer_name ->
        found := true;
        begin match inner_mty with
        | Module_types.ModTypeSig inner_sig ->
          let remaining_path = { Location.value = Parsing.Syntax_tree.Ldot (deeper_prefix, mod_name); location = loc } in
          let refined_inner_sig = refine_module_in_sig ctx inner_sig remaining_path target_mty loc in
          Module_types.SigModule (n, Module_types.ModTypeSig refined_inner_sig)
        | _ ->
          Compiler_error.type_error loc "Cannot refine module in non-signature"
        end
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" outer_name);
    refined_sig

(** Apply a type constraint: with type path = type_expr *)
and apply_type_constraint ctx mty path params type_expr loc =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig, ctx = refine_type_in_sig ctx sig_ path params type_expr loc in
    (Module_types.ModTypeSig refined_sig, ctx)
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to functor type"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to abstract module type"

(** Refine a type in a signature by path *)
and refine_type_in_sig ctx sig_ (path : Parsing.Syntax_tree.longident) params type_expr loc =
  match path.Location.value with
  | Lident name ->
    (* Direct type: with type t = ... *)
    let found = ref false in
    let manifest_type_ref = ref None in
    let ctx_ref = ref ctx in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigType (n, decl) when n = name ->
        found := true;
        (* Verify parameter count matches *)
        if List.length params <> List.length decl.declaration_parameters then
          Compiler_error.type_error loc
            (Printf.sprintf "Type %s has %d parameters but constraint has %d"
              name (List.length decl.declaration_parameters) (List.length params));
        (* Convert the type expression to a semantic type and store as manifest *)
        let manifest_type, ctx' = Type_expression_check.check_type_expression !ctx_ref type_expr in
        ctx_ref := ctx';
        manifest_type_ref := Some manifest_type;
        let refined_decl = { decl with declaration_manifest = Some manifest_type } in
        Module_types.SigType (n, refined_decl)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Type %s not found in signature" name);
    (refined_sig, !ctx_ref)
  | Ldot (prefix, name) ->
    (* Nested: with type M.t = ... - find module M, then refine type t inside *)
    refine_type_in_nested_sig ctx sig_ prefix name params type_expr loc

(** Refine a type in a nested module signature *)
and refine_type_in_nested_sig ctx sig_ (prefix : Parsing.Syntax_tree.longident) type_name params type_expr loc =
  (* Get the module name from the prefix *)
  match prefix.Location.value with
  | Lident mod_name ->
    (* Simple module prefix: with type M.t = ... *)
    let found = ref false in
    let ctx_ref = ref ctx in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let type_path = { Location.value = Parsing.Syntax_tree.Lident type_name; location = loc } in
        let refined_inner, ctx' = apply_type_constraint !ctx_ref inner_mty type_path params type_expr loc in
        ctx_ref := ctx';
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    (refined_sig, !ctx_ref)
  | Ldot (deeper_prefix, mod_name) ->
    (* Deeper nesting: with type M.N.t = ... - recursively find M, then N.t *)
    let found = ref false in
    let ctx_ref = ref ctx in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let remaining_path = { Location.value = Parsing.Syntax_tree.Ldot (deeper_prefix, type_name); location = loc } in
        let refined_inner, ctx' = apply_type_constraint !ctx_ref inner_mty remaining_path params type_expr loc in
        ctx_ref := ctx';
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    (refined_sig, !ctx_ref)

(** Apply a destructive type constraint: with type path := type_expr
    This substitutes all occurrences of the type and removes it from the signature. *)
and apply_destructive_type_constraint ctx mty path params type_expr loc =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig, ctx = destructive_refine_type_in_sig ctx sig_ path params type_expr loc in
    (Module_types.ModTypeSig refined_sig, ctx)
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to functor type"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to abstract module type"

(** Destructively refine a type in a signature - substitutes and removes the type *)
and destructive_refine_type_in_sig ctx sig_ (path : Parsing.Syntax_tree.longident) params type_expr loc =
  match path.Location.value with
  | Lident name ->
    (* Direct type: with type t := ... *)
    let found = ref false in
    let ctx_ref = ref ctx in
    let replacement_type = ref None in
    let type_params = ref [] in
    (* First pass: find the type, verify params, get replacement type *)
    List.iter (fun item ->
      match item with
      | Module_types.SigType (n, decl) when n = name ->
        found := true;
        (* Verify parameter count matches *)
        if List.length params <> List.length decl.declaration_parameters then
          Compiler_error.type_error loc
            (Printf.sprintf "Type %s has %d parameters but constraint has %d"
              name (List.length decl.declaration_parameters) (List.length params));
        (* Store the type parameters for substitution *)
        type_params := decl.declaration_parameters;
        (* Convert the type expression to a semantic type *)
        let manifest_type, ctx' = Type_expression_check.check_type_expression !ctx_ref type_expr in
        ctx_ref := ctx';
        replacement_type := Some manifest_type
      | _ -> ()
    ) sig_;
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Type %s not found in signature" name);
    (* Second pass: substitute and filter out the type *)
    let subst_type = match !replacement_type with
      | Some ty -> ty
      | None -> Compiler_error.internal_error "Replacement type not found after first pass"
    in
    let refined_sig = List.filter_map (fun item ->
      match item with
      | Module_types.SigType (n, _) when n = name ->
        (* Remove the type declaration *)
        None
      | Module_types.SigValue (n, vd) ->
        (* Substitute type in value description *)
        let new_body = substitute_type_in_type name !type_params subst_type vd.Module_types.value_type.body in
        let new_scheme = { vd.Module_types.value_type with body = new_body } in
        Some (Module_types.SigValue (n, { vd with Module_types.value_type = new_scheme }))
      | Module_types.SigType (n, decl) ->
        (* Substitute type in other type declarations' manifests *)
        let new_manifest = Option.map (substitute_type_in_type name !type_params subst_type) decl.declaration_manifest in
        Some (Module_types.SigType (n, { decl with declaration_manifest = new_manifest }))
      | Module_types.SigModule (n, inner_mty) ->
        (* Substitute type in nested module *)
        let new_inner = substitute_type_in_module_type name !type_params subst_type inner_mty in
        Some (Module_types.SigModule (n, new_inner))
      | Module_types.SigModuleType (n, mty_opt) ->
        (* Substitute type in module type definition *)
        let new_mty_opt = Option.map (substitute_type_in_module_type name !type_params subst_type) mty_opt in
        Some (Module_types.SigModuleType (n, new_mty_opt))
    ) sig_ in
    (refined_sig, !ctx_ref)
  | Ldot (prefix, name) ->
    (* Nested: with type M.t := ... - find module M, then destructively refine type t inside *)
    destructive_refine_type_in_nested_sig ctx sig_ prefix name params type_expr loc

(** Destructively refine a type in a nested module signature *)
and destructive_refine_type_in_nested_sig ctx sig_ (prefix : Parsing.Syntax_tree.longident) type_name params type_expr loc =
  match prefix.Location.value with
  | Lident mod_name ->
    (* Simple module prefix: with type M.t := ... *)
    let found = ref false in
    let ctx_ref = ref ctx in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let type_path = { Location.value = Parsing.Syntax_tree.Lident type_name; location = loc } in
        let refined_inner, ctx' = apply_destructive_type_constraint !ctx_ref inner_mty type_path params type_expr loc in
        ctx_ref := ctx';
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    (refined_sig, !ctx_ref)
  | Ldot (deeper_prefix, mod_name) ->
    (* Deeper nesting: with type M.N.t := ... *)
    let found = ref false in
    let ctx_ref = ref ctx in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let remaining_path = { Location.value = Parsing.Syntax_tree.Ldot (deeper_prefix, type_name); location = loc } in
        let refined_inner, ctx' = apply_destructive_type_constraint !ctx_ref inner_mty remaining_path params type_expr loc in
        ctx_ref := ctx';
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    (refined_sig, !ctx_ref)

(** Substitute a type path with a replacement type in a type expression *)
and substitute_type_in_type type_name type_params replacement ty =
  match Types.representative ty with
  | Types.TypeVariable _ -> ty
  | Types.TypeConstructor (path, args) ->
    let args' = List.map (substitute_type_in_type type_name type_params replacement) args in
    begin match path with
    | Types.PathLocal name when name = type_name ->
      (* This is the type we're substituting - replace with the replacement type *)
      (* If there are type parameters, we need to substitute them in the replacement *)
      if List.length type_params = 0 then
        replacement
      else
        (* Substitute type params in replacement with the actual args *)
        Type_utils.substitute_type_params type_params args' replacement
    | _ ->
      Types.TypeConstructor (path, args')
    end
  | Types.TypeArrow (t1, t2) ->
    Types.TypeArrow (
      substitute_type_in_type type_name type_params replacement t1,
      substitute_type_in_type type_name type_params replacement t2
    )
  | Types.TypeTuple tys ->
    Types.TypeTuple (List.map (substitute_type_in_type type_name type_params replacement) tys)
  | Types.TypeRecord row ->
    let new_fields = List.map (fun (name, field) ->
      match field with
      | Types.RowFieldPresent ty ->
        (name, Types.RowFieldPresent (substitute_type_in_type type_name type_params replacement ty))
    ) row.row_fields in
    let new_more = substitute_type_in_type type_name type_params replacement row.row_more in
    Types.TypeRecord { row_fields = new_fields; row_more = new_more }
  | Types.TypePolyVariant pv_row ->
    let new_fields = List.map (fun (name, field) ->
      let new_field = match field with
        | Types.PVFieldPresent (Some ty) ->
          Types.PVFieldPresent (Some (substitute_type_in_type type_name type_params replacement ty))
        | Types.PVFieldPresent None -> Types.PVFieldPresent None
        | Types.PVFieldAbsent -> Types.PVFieldAbsent
      in
      (name, new_field)
    ) pv_row.pv_fields in
    let new_more = substitute_type_in_type type_name type_params replacement pv_row.pv_more in
    Types.TypePolyVariant { pv_fields = new_fields; pv_more = new_more; pv_closed = pv_row.pv_closed }
  | Types.TypeRowEmpty -> ty

(** Substitute a type in a module type *)
and substitute_type_in_module_type type_name type_params replacement mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.ModTypeSig (substitute_type_in_signature type_name type_params replacement sig_)
  | Module_types.ModTypeFunctor (param, result) ->
    let new_param_type = substitute_type_in_module_type type_name type_params replacement param.parameter_type in
    let new_param = { param with Module_types.parameter_type = new_param_type } in
    let new_result = substitute_type_in_module_type type_name type_params replacement result in
    Module_types.ModTypeFunctor (new_param, new_result)
  | Module_types.ModTypeIdent _ -> mty

(** Substitute a type in a signature *)
and substitute_type_in_signature type_name type_params replacement sig_ =
  List.map (fun item ->
    match item with
    | Module_types.SigValue (n, vd) ->
      let new_body = substitute_type_in_type type_name type_params replacement vd.Module_types.value_type.body in
      let new_scheme = { vd.Module_types.value_type with body = new_body } in
      Module_types.SigValue (n, { vd with Module_types.value_type = new_scheme })
    | Module_types.SigType (n, decl) ->
      let new_manifest = Option.map (substitute_type_in_type type_name type_params replacement) decl.declaration_manifest in
      Module_types.SigType (n, { decl with declaration_manifest = new_manifest })
    | Module_types.SigModule (n, inner_mty) ->
      Module_types.SigModule (n, substitute_type_in_module_type type_name type_params replacement inner_mty)
    | Module_types.SigModuleType (n, mty_opt) ->
      Module_types.SigModuleType (n, Option.map (substitute_type_in_module_type type_name type_params replacement) mty_opt)
  ) sig_

(** Check a syntax signature and convert to semantic signature.

    @param ctx The typing context
    @param sig_items The syntactic signature items
    @return A pair [(signature, updated_ctx)] *)
and check_signature ctx (sig_items : signature) : Module_types.signature * Typing_context.t =
  let items, ctx =
    List.fold_left (fun (items, ctx) item ->
      let item_opt, ctx = check_signature_item ctx item in
      let items = match item_opt with
        | Some sig_item -> items @ [sig_item]
        | None -> items
      in
      (items, ctx)
    ) ([], ctx) sig_items
  in
  (items, ctx)

(** Check a signature item.

    @param ctx The typing context
    @param item The syntactic signature item
    @return A pair [(sig_item_opt, updated_ctx)] *)
and check_signature_item ctx (item : signature_item) : Module_types.signature_item option * Typing_context.t =
  let loc = item.Location.location in
  match item.Location.value with
  | SignatureValue (name, type_expr) ->
    let name_str = name.Location.value in
    (* Convert syntax type to semantic type *)
    let ty, ctx = Type_expression_check.check_type_expression ctx type_expr in
    (* Use level 0 for signature type generalization so variables at level 1 are generalized *)
    let scheme = Type_scheme.generalize ~level:0 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name_str, val_desc)), ctx)

  | SignatureType type_decls ->
    (* Process signature type declarations and add them to the environment
       so that later items can reference them *)
    begin match type_decls with
    | decl :: _ ->
      let name = decl.type_name.Location.value in
      (* Create fresh type variables for each parameter *)
      let type_params, ctx =
        List.fold_left (fun (params, ctx) _ ->
          let tv, ctx = Typing_context.new_type_variable ctx in
          match tv with
          | Types.TypeVariable tv -> (params @ [tv], ctx)
          | _ ->
            Common.Compiler_error.internal_error
              "new_type_variable did not return a TypeVariable"
        ) ([], ctx) decl.type_parameters
      in
      (* Determine the kind based on the syntax *)
      let kind = match decl.type_kind with
        | Parsing.Syntax_tree.TypeAbstract -> Types.DeclarationAbstract
        | Parsing.Syntax_tree.TypeVariant _ -> Types.DeclarationAbstract (* Simplified for signatures *)
        | Parsing.Syntax_tree.TypeAlias _ -> Types.DeclarationAbstract (* Aliases are abstract in signatures *)
      in
      (* Create a type declaration for the signature *)
      (* Abstract types in signatures default to invariant (most restrictive) *)
      let sig_decl = Types.{
        declaration_name = name;
        declaration_parameters = type_params;
        declaration_variances = List.map (fun _ -> Types.Invariant) type_params;
        declaration_manifest = None;
        declaration_kind = kind;
        declaration_private = decl.type_private;
        declaration_constraints = [];
      } in
      (* Add the type to the environment so later signature items can reference it *)
      let env = Typing_context.environment ctx in
      let env = Environment.add_type name sig_decl env in
      let ctx = Typing_context.with_environment env ctx in
      (Some (Module_types.SigType (name, sig_decl)), ctx)
    | [] -> (None, ctx)
    end

  | SignatureModule (name, mty) ->
    let name_str = name.Location.value in
    let semantic_mty, ctx = check_module_type ctx mty in
    (Some (Module_types.SigModule (name_str, semantic_mty)), ctx)

  | SignatureModuleType (name, mty_opt) ->
    let name_str = name.Location.value in
    let semantic_mty_opt, ctx = match mty_opt with
      | Some mty ->
        let resolved, ctx = check_module_type ctx mty in
        (Some resolved, ctx)
      | None -> (None, ctx)
    in
    (Some (Module_types.SigModuleType (name_str, semantic_mty_opt)), ctx)

  | SignatureOpen _path ->
    (* Open in signature - skip for now *)
    (None, ctx)

  | SignatureInclude _mty ->
    (* Include in signature - skip for now *)
    (None, ctx)

  | SignatureExternal ext_decl ->
    (* External declarations in signatures contribute value bindings *)
    let name = ext_decl.external_name.Location.value in
    let ty, ctx = Type_expression_check.check_type_expression ctx ext_decl.external_type in
    (* Use level 0 for signature type generalization so variables at level 1 are generalized *)
    let scheme = Type_scheme.generalize ~level:0 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name, val_desc)), ctx)
