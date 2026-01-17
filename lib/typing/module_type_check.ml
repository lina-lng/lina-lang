(** Module type checking. *)

open Common
open Parsing.Syntax_tree
open Types

(** Forward reference for module expression inference.
    Set by Structure_infer to break circular dependency.
    Used for [module type of M] to get the signature of a module expression. *)
let infer_module_expression_ref :
    (Typing_context.t -> module_expression -> Typed_tree.typed_module_expression * Typing_context.t) ref =
  ref (fun _ctx _mexpr ->
    Compiler_error.internal_error
      "infer_module_expression not initialized - Structure_infer must set this")

let module_path_to_internal_path base_id path_modules =
  match path_modules with
  | [] -> Types.PathIdent base_id
  | _ :: rest ->
      List.fold_left
        (fun acc name -> Types.PathDot (acc, name))
        (Types.PathIdent base_id)
        rest

let find_module_in_type_or_error mod_type name loc =
  match mod_type with
  | Module_types.ModTypeSig sig_ ->
      begin match Module_types.find_module_in_sig name sig_ with
      | Some mty -> mty
      | None ->
          Compiler_error.type_error loc
            (Printf.sprintf "Module %s not found in signature" name)
      end
  | _ ->
      Inference_utils.error_cannot_access_in_module_type loc mod_type "access module"

let make_module_binding name mty =
  let id = Identifier.create name in
  Module_types.{ binding_name = name; binding_id = id; binding_type = mty; binding_alias = None }

let split_last list =
  let rec collect_prefix prefix_rev = function
    | [] -> Compiler_error.internal_error "split_last called with empty list"
    | [last] -> (List.rev prefix_rev, last)
    | x :: rest -> collect_prefix (x :: prefix_rev) rest
  in
  collect_prefix [] list

(** {2 Signature Refinement Helpers}

    These helpers search for and transform items in signatures.
    The generic [search_and_refine_sig] captures the common pattern. *)

(** Generic signature item search and transform.

    Searches through [sig_] for an item matching [match_and_transform].
    Returns [(new_signature, result)] where the matching item is replaced.

    @param match_and_transform Returns [Some (new_item, result)] for matching items
    @param not_found_msg Error message if no item matches
    @param sig_ The signature to search
    @param loc Location for error reporting *)
let search_and_refine_sig ~match_and_transform ~not_found_msg sig_ loc =
  let rec loop acc = function
    | [] -> Compiler_error.type_error loc not_found_msg
    | item :: rest ->
        match match_and_transform item with
        | Some (new_item, result) -> (List.rev_append acc (new_item :: rest), result)
        | None -> loop (item :: acc) rest
  in
  loop [] sig_

let refine_sig_module name sig_ loc ~transform ~not_found_msg =
  let match_and_transform = function
    | Module_types.SigModule (n, mty) when n = name ->
        Some (Module_types.SigModule (n, transform mty), ())
    | _ -> None
  in
  let result, () = search_and_refine_sig ~match_and_transform ~not_found_msg sig_ loc in
  result

let refine_sig_module_with_ctx name sig_ loc ctx ~transform ~not_found_msg =
  let match_and_transform = function
    | Module_types.SigModule (n, mty) when n = name ->
        let new_mty, new_ctx = transform ctx mty in
        Some (Module_types.SigModule (n, new_mty), new_ctx)
    | _ -> None
  in
  search_and_refine_sig ~match_and_transform ~not_found_msg sig_ loc

let refine_sig_type name sig_ loc ~transform ~not_found_msg =
  let match_and_transform = function
    | Module_types.SigType (n, decl) when n = name -> Some (transform decl)
    | _ -> None
  in
  search_and_refine_sig ~match_and_transform ~not_found_msg sig_ loc

(** Dispatch on a nested path (M.N.x) by matching the outermost module.
    Creates the inner path and delegates to [refine]. *)
let dispatch_nested_path prefix leaf_name loc ~refine =
  match prefix.Location.value with
  | Lident mod_name ->
      let inner_path = { Location.value = Parsing.Syntax_tree.Lident leaf_name; location = loc } in
      refine mod_name inner_path
  | Ldot (deeper_prefix, mod_name) ->
      let inner_path = { Location.value = Parsing.Syntax_tree.Ldot (deeper_prefix, leaf_name); location = loc } in
      refine mod_name inner_path

let lookup_module_path env path_modules loc =
  match path_modules with
  | [] ->
      Compiler_error.type_error loc "Empty module path"

  | [name] ->
      begin match Environment.find_module name env with
      | Some binding -> (binding, binding)
      | None -> Inference_utils.error_unbound_module loc name
      end

  | first :: rest ->
      let rec follow_path mod_type path =
        match path with
        | [] ->
            Compiler_error.type_error loc "Empty path in module lookup"
        | [name] ->
            let mty = find_module_in_type_or_error mod_type name loc in
            make_module_binding name mty
        | name :: remaining ->
            let mty = find_module_in_type_or_error mod_type name loc in
            follow_path mty remaining
      in

      begin match Environment.find_module first env with
      | Some base_binding ->
          let final_binding = follow_path base_binding.binding_type rest in
          (base_binding, final_binding)
      | None ->
          Inference_utils.error_unbound_module loc first
      end

let rec check_module_type ctx (mty : module_type) =
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
        | _ ->
          Inference_utils.error_cannot_access_in_module_type loc final_binding.binding_type "find module type"
        end
      end
    end

  | ModuleTypeSignature sig_items ->
    let semantic_sig, ctx = check_signature ctx sig_items in
    (Module_types.ModTypeSig semantic_sig, ctx)

  | ModuleTypeFunctor (params, result) ->
    (* Functor type: functor (X : S) -> MT or functor () -> MT *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc "Functor type must have at least one parameter"
    | [param] ->
      begin match param with
      | FunctorParamNamed (name_loc, param_type) ->
        let parameter_name = name_loc.Location.value in
        let parameter_type, ctx = check_module_type ctx param_type in
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
        let semantic_param = Module_types.FunctorParamNamed {
          parameter_name;
          parameter_id;
          parameter_type;
        } in
        (Module_types.ModTypeFunctor (semantic_param, result_mty), ctx)
      | FunctorParamUnit _ ->
        (* Generative functor: functor () -> MT *)
        let result_mty, ctx = check_module_type ctx result in
        (Module_types.ModTypeFunctor (Module_types.FunctorParamUnit, result_mty), ctx)
      end
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
    let base, ctx = check_module_type ctx base_mty in
    let result, ctx =
      List.fold_left (fun (mty, ctx) constraint_ ->
        apply_with_constraint ctx mty constraint_ loc
      ) (base, ctx) constraints
    in
    (result, ctx)

  | ModuleTypeOf module_expr ->
    (* module type of M - extract the signature of the module expression *)
    let infer_module_expression = !infer_module_expression_ref in
    let typed_mexpr, ctx = infer_module_expression ctx module_expr in
    (typed_mexpr.Typed_tree.module_type, ctx)

and apply_with_constraint ctx mty constraint_ loc =
  match constraint_ with
  | WithType (path, params, type_expr) ->
    apply_type_constraint ctx mty path params type_expr loc
  | WithTypeDestructive (path, params, type_expr) ->
    apply_destructive_type_constraint ctx mty path params type_expr loc
  | WithModule (path, target) ->
    apply_module_constraint ctx mty path target loc

and apply_module_constraint ctx mty path target loc =
  let env = Typing_context.environment ctx in
  let target_modules = target.Location.value in
  let (_, target_binding) = lookup_module_path env target_modules loc in
  let target_mty = target_binding.Module_types.binding_type in

  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig = refine_module_in_sig ctx sig_ path target_mty loc in
    (Module_types.ModTypeSig refined_sig, ctx)
  | _ ->
    Inference_utils.error_cannot_access_in_module_type loc mty "apply with-constraint"

and refine_module_in_sig ctx sig_ path target_mty loc =
  match path.Location.value with
  | Lident name ->
      refine_sig_module name sig_ loc
        ~transform:(fun _ -> target_mty)
        ~not_found_msg:(Printf.sprintf "Module %s not found in signature" name)

  | Ldot (prefix, name) ->
      refine_nested_module_in_sig ctx sig_ prefix name target_mty loc

and refine_nested_module_in_sig ctx sig_ prefix mod_name target_mty loc =
  let refine outer_name inner_path =
    let transform_inner inner_mty =
      match inner_mty with
      | Module_types.ModTypeSig inner_sig ->
          let refined = refine_module_in_sig ctx inner_sig inner_path target_mty loc in
          Module_types.ModTypeSig refined
      | _ ->
          Compiler_error.type_error loc "Cannot refine module in non-signature"
    in
    refine_sig_module outer_name sig_ loc
      ~transform:transform_inner
      ~not_found_msg:(Printf.sprintf "Module %s not found in signature" outer_name)
  in
  dispatch_nested_path prefix mod_name loc ~refine

and apply_type_constraint_with ~refine_fn ctx mty path params type_expr loc =
  match mty with
  | Module_types.ModTypeSig sig_ ->
      let refined_sig, ctx = refine_fn ctx sig_ path params type_expr loc in
      (Module_types.ModTypeSig refined_sig, ctx)
  | _ ->
      Inference_utils.error_cannot_access_in_module_type loc mty "apply with-constraint"

and apply_type_constraint ctx mty path params type_expr loc =
  apply_type_constraint_with ~refine_fn:refine_type_in_sig ctx mty path params type_expr loc

and refine_type_in_sig ctx sig_ path params type_expr loc =
  match path.Location.value with
  | Lident name ->
      let transform decl =
        if List.length params <> List.length decl.Types.declaration_parameters then
          Compiler_error.type_error loc
            (Printf.sprintf "Type %s has %d parameters but constraint has %d"
              name (List.length decl.declaration_parameters) (List.length params));

        (* Use the declaration's type parameters so that references in the
           manifest type (like 'a in "'a -> unit") correctly match the
           declaration's parameters for proper substitution later. *)
        let manifest_type, new_ctx =
          Type_expression_check.check_type_expression_with_params
            ctx params decl.Types.declaration_parameters type_expr
        in
        let refined_decl = { decl with declaration_manifest = Some manifest_type } in
        (Module_types.SigType (name, refined_decl), new_ctx)
      in
      refine_sig_type name sig_ loc ~transform
        ~not_found_msg:(Printf.sprintf "Type %s not found in signature" name)

  | Ldot (prefix, name) ->
      refine_type_in_nested_sig ctx sig_ prefix name params type_expr loc

and refine_type_in_nested_sig ctx sig_ prefix type_name params type_expr loc =
  let refine mod_name type_path =
    refine_sig_module_with_ctx mod_name sig_ loc ctx
      ~transform:(fun ctx inner_mty ->
        apply_type_constraint ctx inner_mty type_path params type_expr loc)
      ~not_found_msg:(Printf.sprintf "Module %s not found in signature" mod_name)
  in
  dispatch_nested_path prefix type_name loc ~refine

and apply_destructive_type_constraint ctx mty path params type_expr loc =
  apply_type_constraint_with ~refine_fn:destructive_refine_type_in_sig
    ctx mty path params type_expr loc

(** Two-pass algorithm: find/validate the type, then substitute and remove it. *)
and destructive_refine_type_in_sig ctx sig_ path params type_expr loc =
  match path.Location.value with
  | Lident name ->
      (* Phase 1: Find and validate the type declaration *)
      let type_decl = List.find_map (function
        | Module_types.SigType (n, decl) when n = name -> Some decl
        | _ -> None
      ) sig_ in

      let decl = match type_decl with
        | Some d -> d
        | None -> Compiler_error.type_error loc
            (Printf.sprintf "Type %s not found in signature" name)
      in

      if List.length params <> List.length decl.Types.declaration_parameters then
        Compiler_error.type_error loc
          (Printf.sprintf "Type %s has %d parameters but constraint has %d"
            name (List.length decl.declaration_parameters) (List.length params));

      (* Phase 2: Compute the replacement type *)
      let replacement_type, new_ctx = Type_expression_check.check_type_expression ctx type_expr in
      let decl_params = decl.declaration_parameters in

      (* Phase 3: Substitute and remove the type from signature *)
      let substitute_in_item = function
        | Module_types.SigType (n, _) when n = name ->
            None
        | Module_types.SigValue (n, vd) ->
            let new_body = substitute_type_in_type name decl_params replacement_type vd.Module_types.value_type.body in
            let new_scheme = { vd.Module_types.value_type with body = new_body } in
            Some (Module_types.SigValue (n, { vd with Module_types.value_type = new_scheme }))
        | Module_types.SigType (n, other_decl) ->
            let new_manifest = Option.map (substitute_type_in_type name decl_params replacement_type) other_decl.declaration_manifest in
            Some (Module_types.SigType (n, { other_decl with declaration_manifest = new_manifest }))
        | Module_types.SigModule (n, inner_mty) ->
            let new_inner = substitute_type_in_module_type name decl_params replacement_type inner_mty in
            Some (Module_types.SigModule (n, new_inner))
        | Module_types.SigModuleType (n, mty_opt) ->
            let new_mty_opt = Option.map (substitute_type_in_module_type name decl_params replacement_type) mty_opt in
            Some (Module_types.SigModuleType (n, new_mty_opt))
        | Module_types.SigExtensionConstructor ctor ->
            (* Extension constructors reference types by path, not substitutable *)
            Some (Module_types.SigExtensionConstructor ctor)
      in

      let refined_sig = List.filter_map substitute_in_item sig_ in

      (refined_sig, new_ctx)

  | Ldot (prefix, name) ->
      destructive_refine_type_in_nested_sig ctx sig_ prefix name params type_expr loc

and destructive_refine_type_in_nested_sig ctx sig_ prefix type_name params type_expr loc =
  let refine mod_name type_path =
    refine_sig_module_with_ctx mod_name sig_ loc ctx
      ~transform:(fun ctx inner_mty ->
        apply_destructive_type_constraint ctx inner_mty type_path params type_expr loc)
      ~not_found_msg:(Printf.sprintf "Module %s not found in signature" mod_name)
  in
  dispatch_nested_path prefix type_name loc ~refine

and substitute_type_in_type type_name type_params replacement ty =
  Type_traversal.map (function
    | Types.TypeConstructor (Types.PathLocal name, args) when name = type_name ->
        if type_params = [] then replacement
        else Type_utils.substitute_type_params type_params args replacement
    | t -> t
  ) ty

and substitute_type_in_module_type type_name type_params replacement mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.ModTypeSig (substitute_type_in_signature type_name type_params replacement sig_)
  | Module_types.ModTypeFunctor (param, result) ->
      let new_param = match param with
        | Module_types.FunctorParamNamed { parameter_name; parameter_id; parameter_type } ->
            let new_type = substitute_type_in_module_type type_name type_params replacement parameter_type in
            Module_types.FunctorParamNamed {
              parameter_name;
              parameter_id;
              parameter_type = new_type;
            }
        | Module_types.FunctorParamUnit -> Module_types.FunctorParamUnit
      in
      let new_result = substitute_type_in_module_type type_name type_params replacement result in
      Module_types.ModTypeFunctor (new_param, new_result)
  | Module_types.ModTypeIdent _ -> mty

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
    | Module_types.SigExtensionConstructor ctor ->
      Module_types.SigExtensionConstructor ctor
  ) sig_

and check_signature ctx sig_items =
  let rev_items, ctx =
    List.fold_left (fun (rev_items, ctx) item ->
      let item_opt, ctx = check_signature_item ctx item in
      match item_opt with
      | Some sig_item -> (sig_item :: rev_items, ctx)
      | None -> (rev_items, ctx)
    ) ([], ctx) sig_items
  in
  (List.rev rev_items, ctx)

and check_signature_item ctx item =
  let loc = item.Location.location in
  match item.Location.value with
  | SignatureValue (name, type_expr) ->
    let name_str = name.Location.value in
    let ty, ctx = Type_expression_check.check_type_expression ctx type_expr in
    let scheme = Type_scheme.generalize ~level:0 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name_str, val_desc)), ctx)

  | SignatureType type_decls ->
    begin match type_decls with
    | decl :: _ ->
      let name = decl.type_name.Location.value in
      let rev_type_params, ctx =
        List.fold_left (fun (rev_params, ctx) _ ->
          let tv, ctx = Typing_context.new_type_variable ctx in
          match tv with
          | Types.TypeVariable tv -> (tv :: rev_params, ctx)
          | _ ->
            Common.Compiler_error.internal_error
              "new_type_variable did not return a TypeVariable"
        ) ([], ctx) decl.type_parameters
      in
      let type_params = List.rev rev_type_params in

      (* Get parameter names for mapping when checking type expressions *)
      let param_names = List.map (fun (param : Parsing.Syntax_tree.type_parameter) ->
        param.parameter_name
      ) decl.type_parameters in

      (* Build the result type: T or 'a T or ('a, 'b) T *)
      let result_type =
        TypeConstructor (PathLocal name, List.map (fun tv -> TypeVariable tv) type_params)
      in

      (* Add a preliminary declaration so constructors can reference the type *)
      let env = Typing_context.environment ctx in
      let preliminary_decl = Types.{
        declaration_name = name;
        declaration_parameters = type_params;
        declaration_variances = List.map (fun _ -> Types.Invariant) type_params;
        declaration_injectivities = List.map (fun _ -> true) type_params;
        declaration_manifest = None;
        declaration_kind = DeclarationAbstract;
        declaration_private = decl.type_private;
        declaration_constraints = [];
      } in
      let env_with_type = Environment.add_type name preliminary_decl env in
      let ctx_with_type = Typing_context.with_environment env_with_type ctx in

      let (kind, manifest, ctx) = match decl.type_kind with
        | Parsing.Syntax_tree.TypeAbstract -> (Types.DeclarationAbstract, None, ctx)
        | Parsing.Syntax_tree.TypeVariant constructors ->
          let constructor_infos, ctx =
            Constructor_check.check_constructors ctx_with_type param_names type_params
              result_type name constructors
          in
          (Types.DeclarationVariant constructor_infos, None, ctx)
        | Parsing.Syntax_tree.TypeAlias ty_expr ->
          let manifest_type, ctx = Type_expression_check.check_type_expression_with_params
            ctx_with_type param_names type_params ty_expr in
          (Types.DeclarationAbstract, Some manifest_type, ctx)
        | Parsing.Syntax_tree.TypeExtensible ->
          (Types.DeclarationExtensible, None, ctx)
      in

      (* Compute injectivities: use explicit !'a annotation, or default to true for abstract types *)
      let injectivities = List.map2 (fun (param : Parsing.Syntax_tree.type_parameter) _ ->
        param.parameter_injective || true  (* Abstract types in signatures default to injective *)
      ) decl.type_parameters type_params in

      let sig_decl = Types.{
        declaration_name = name;
        declaration_parameters = type_params;
        declaration_variances = List.map (fun _ -> Types.Invariant) type_params;
        declaration_injectivities = injectivities;
        declaration_manifest = manifest;
        declaration_kind = kind;
        declaration_private = decl.type_private;
        declaration_constraints = [];
      } in

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
    (None, ctx)

  | SignatureInclude _mty ->
    (None, ctx)

  | SignatureExternal ext_decl ->
    let name = ext_decl.external_name.Location.value in
    let ty, ctx = Type_expression_check.check_type_expression ctx ext_decl.external_type in
    let scheme = Type_scheme.generalize ~level:0 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name, val_desc)), ctx)
