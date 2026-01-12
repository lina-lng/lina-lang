(** Structure and module expression type inference.

    This module handles type inference for:
    - Structure items (let bindings, type declarations, modules)
    - Module expressions (struct, path, functor, apply, constraint)
    - Module type checking and signature matching
    - Signature extraction from typed structures *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Create a module type lookup function from the environment.
    This handles both simple paths (PathIdent) and qualified paths (PathDot). *)
let make_module_type_lookup env =
  let rec lookup_mty_by_path path =
    match path with
    | Types.PathIdent id ->
      (* Simple module type name - look up directly in environment *)
      begin match Environment.find_module_type (Identifier.name id) env with
      | Some (Some mty) -> Some mty
      | Some None -> None  (* Abstract - can't expand *)
      | None -> None  (* Not found *)
      end
    | Types.PathDot (base_path, name) ->
      (* Qualified path like M.S - resolve module path, then find in signature *)
      begin match lookup_module_by_path base_path with
      | Some (Module_types.ModTypeSig sig_) ->
        begin match Module_types.find_module_type_in_sig name sig_ with
        | Some (Some mty) -> Some mty
        | Some None -> None  (* Abstract *)
        | None -> None  (* Not found in signature *)
        end
      | _ -> None
      end
    | Types.PathLocal name ->
      (* Local module type name - look up directly *)
      begin match Environment.find_module_type name env with
      | Some (Some mty) -> Some mty
      | _ -> None
      end
    | _ -> None
  and lookup_module_by_path path =
    match path with
    | Types.PathIdent id ->
      begin match Environment.find_module (Identifier.name id) env with
      | Some binding -> Some binding.Module_types.mod_type
      | None -> None
      end
    | Types.PathDot (base_path, name) ->
      begin match lookup_module_by_path base_path with
      | Some (Module_types.ModTypeSig sig_) ->
        Module_types.find_module_in_sig name sig_
      | _ -> None
      end
    | _ -> None
  in
  lookup_mty_by_path

(** Set up signature matching with module type lookup from environment *)
let setup_signature_matching env =
  Signature_match.set_module_type_lookup (make_module_type_lookup env)

(** Check if a type is already an option type. *)
let is_option_type ty =
  match ty with
  | Types.TypeConstructor (Types.PathLocal "option", [_]) -> true
  | _ -> false

(** Wrap the return type of a function in option for @return(nullable).
    For example: (int -> string) becomes (int -> string option).
    If the return type is already option, don't double-wrap it. *)
let rec wrap_return_in_option ty =
  match ty with
  | Types.TypeArrow (arg, ret) ->
    Types.TypeArrow (arg, wrap_return_in_option ret)
  | _ when is_option_type ty ->
    (* Already option, don't double-wrap *)
    ty
  | _ ->
    (* Wrap the final return type in option *)
    Types.TypeConstructor (Types.PathLocal "option", [ty])

(** Process a type declaration and add it to the environment *)
let process_type_declaration env (type_decl : Parsing.Syntax_tree.type_declaration) =
  (* Create semantic type variables for each type parameter *)
  let type_params =
    List.map (fun _ ->
      match new_type_variable () with
      | TypeVariable tv -> tv
      | _ -> assert false
    ) type_decl.type_parameters
  in
  (* Build the result type: T or 'a T or ('a, 'b) T *)
  let result_type =
    TypeConstructor (PathLocal type_decl.type_name.Location.value,
      List.map (fun tv -> TypeVariable tv) type_params)
  in
  (* Get parameter names for mapping (e.g., ["'a"; "'b"]) *)
  let param_names = List.map (fun (param : type_parameter) -> param.param_name) type_decl.type_parameters in

  (* For recursive types (like 'a list = Nil | Cons of 'a * 'a list),
     we need to add the type to the environment BEFORE parsing constructor
     arguments, so that self-references resolve correctly. *)
  let preliminary_declaration = {
    declaration_name = type_decl.type_name.Location.value;
    declaration_parameters = type_params;
    declaration_variances = List.map (fun _ -> Types.Covariant) type_params;  (* Placeholder, will be computed later *)
    declaration_manifest = None;
    declaration_kind = DeclarationAbstract;  (* Placeholder *)
  } in
  let env_with_type = Environment.add_type type_decl.type_name.Location.value preliminary_declaration env in

  let (declaration_kind, manifest) = match type_decl.type_kind with
    | TypeAbstract -> (DeclarationAbstract, None)
    | TypeVariant constructors ->
      let constructor_infos =
        List.mapi (fun tag_index (ctor : constructor_declaration) ->
          (* Parse the constructor argument type using the parameter mapping.
             This ensures that 'a in "Some of 'a" refers to the same variable
             as the 'a type parameter. Use env_with_type so recursive references work. *)
          let arg_type = Option.map (fun ty_expr ->
            Module_type_check.check_type_expression_with_params env_with_type param_names type_params ty_expr
          ) ctor.constructor_argument in
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
      (DeclarationVariant constructor_infos, None)
    | TypeAlias ty_expr ->
      (* Convert syntax type expression to semantic type, preserving parameter sharing *)
      let manifest_type = Module_type_check.check_type_expression_with_params env_with_type param_names type_params ty_expr in
      (DeclarationAbstract, Some manifest_type)
  in
  (* Create the final type declaration with proper kind *)

  (* Convert explicit variance annotations from syntax to Types.variance *)
  let explicit_variances =
    List.map (fun (param : type_parameter) ->
      match param.param_variance with
      | Some VarianceCovariant -> Some Types.Covariant
      | Some VarianceContravariant -> Some Types.Contravariant
      | None -> None
    ) type_decl.type_parameters
  in

  (* Infer variances from the type definition *)
  let inferred_variances = Variance_infer.infer_declaration_variances
    type_params declaration_kind in

  (* Merge explicit annotations with inferred variances *)
  let final_variances = Variance_infer.merge_variances
    explicit_variances inferred_variances in

  let type_declaration = {
    declaration_name = type_decl.type_name.Location.value;
    declaration_parameters = type_params;
    declaration_variances = final_variances;
    declaration_manifest = manifest;
    declaration_kind;
  } in
  (* Update the environment with the final declaration (replaces preliminary) *)
  let env = Environment.add_type type_decl.type_name.Location.value type_declaration env in
  let env = match declaration_kind with
    | DeclarationAbstract -> env
    | DeclarationVariant constructors ->
      List.fold_left (fun env ctor ->
        Environment.add_constructor ctor.constructor_name ctor env
      ) env constructors
    | DeclarationRecord _ -> env
  in
  (type_declaration, env)

(** Unification error details for tolerant inference. *)
type unification_error_details = {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Common.Location.t;
  message : string;
}

(** Error information from tolerant inference. *)
type inference_error =
  | CompilerError of Common.Compiler_error.t
  | UnificationError of unification_error_details

(** [infer_structure_item env item] infers types for a structure item.

    @param env The typing environment
    @param item The structure item to infer
    @return A pair [(typed_item, updated_env)] *)
let rec infer_structure_item env (item : structure_item) =
  match item.Location.value with
  | StructureValue (rec_flag, bindings) ->
    let typed_bindings, env = Expression_infer.infer_bindings env rec_flag bindings in
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

  | StructureModule module_binding ->
    let module_name = module_binding.module_name.Location.value in
    (* Desugar functor shorthand: module F(X : S) = ME becomes module F = functor (X : S) -> ME *)
    let actual_expr = match module_binding.module_params with
      | [] -> module_binding.module_expr
      | params ->
        let loc = module_binding.module_location in
        { Location.value = ModuleFunctor (params, module_binding.module_expr); location = loc }
    in
    let typed_mexpr, env = infer_module_expression env actual_expr in
    (* Check if there's a signature constraint *)
    let final_mexpr, final_mty =
      match module_binding.module_type with
      | None ->
        (* No constraint - use inferred type *)
        (typed_mexpr, typed_mexpr.module_type)
      | Some constraint_mty ->
        (* Check the constraint and perform signature matching *)
        let spec_mty = Module_type_check.check_module_type env constraint_mty in
        setup_signature_matching env;
        begin match Signature_match.match_module_type item.Location.location typed_mexpr.module_type spec_mty with
        | Ok () ->
          (* Matching succeeded - use the constrained type *)
          let constrained_mexpr = {
            Typed_tree.module_desc = TypedModuleConstraint (typed_mexpr, spec_mty);
            module_type = spec_mty;
            module_location = typed_mexpr.module_location;
          } in
          (constrained_mexpr, spec_mty)
        | Error err ->
          Compiler_error.type_error item.Location.location
            (Printf.sprintf "Module %s does not match signature: %s"
              module_name (Signature_match.format_match_error err))
        end
    in
    (* Register the module in the environment *)
    let module_id = Identifier.create module_name in
    (* Strengthen the module type so that abstract types become concrete *)
    let module_path = Types.PathIdent module_id in
    let strengthened_mty = Signature_match.strengthen_module_type module_path final_mty in
    let mod_binding = Module_types.{
      mod_name = module_name;
      mod_id = module_id;
      mod_type = strengthened_mty;
      mod_alias = None;
    } in
    let env = Environment.add_module module_name mod_binding env in
    let typed_item = {
      structure_item_desc = TypedStructureModule (module_id, final_mexpr);
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

  | StructureModuleType (name_loc, mty) ->
    (* Check the module type expression *)
    let name = name_loc.Location.value in
    let resolved_mty = Module_type_check.check_module_type env mty in
    (* Add the module type to the environment *)
    let env = Environment.add_module_type name (Some resolved_mty) env in
    let typed_item = {
      structure_item_desc = TypedStructureModuleType (name, resolved_mty);
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

  | StructureOpen module_path ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules item.Location.location in
    begin match mod_binding.Module_types.mod_type with
    | Module_types.ModTypeSig sig_ ->
      let (env, opened_bindings) = Environment.open_module sig_ env in
      let path = Module_type_check.module_path_to_internal_path base_binding.mod_id path_modules in
      let typed_item = {
        structure_item_desc = TypedStructureOpen (path, opened_bindings);
        structure_item_location = item.Location.location;
      } in
      (typed_item, env)
    | Module_types.ModTypeFunctor _ ->
      Compiler_error.type_error item.Location.location "Cannot open a functor"
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error item.Location.location "Cannot open an abstract module type"
    end

  | StructureInclude mexpr ->
    (* Type-check the included module expression *)
    let (typed_mexpr, env) = infer_module_expression env mexpr in
    begin match typed_mexpr.module_type with
    | Module_types.ModTypeSig sig_ ->
      (* Bring all signature contents into scope (like open) *)
      let (env, included_bindings) = Environment.open_module sig_ env in
      let typed_item = {
        structure_item_desc = TypedStructureInclude (typed_mexpr, included_bindings);
        structure_item_location = item.Location.location;
      } in
      (typed_item, env)
    | Module_types.ModTypeFunctor _ ->
      Compiler_error.type_error item.Location.location "Cannot include a functor"
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error item.Location.location "Cannot include an abstract module type"
    end

  | StructureExternal ext_decl ->
    (* Type-check the external declaration *)
    let name = ext_decl.external_name.Location.value in
    let location = ext_decl.external_location in
    (* Check the type expression *)
    let external_type = Module_type_check.check_type_expression env ext_decl.external_type in
    (* Compute arity from type (count function arrows) *)
    let rec compute_arity ty =
      match ty with
      | Types.TypeArrow (_, ret) -> 1 + compute_arity ret
      | _ -> 0
    in
    let arity = compute_arity external_type in
    (* Build and validate the FFI spec *)
    let ffi_spec = match Typing_ffi.Check.build_ffi_spec
      ~attrs:ext_decl.external_attributes
      ~primitive:ext_decl.external_primitive
      ~arity
      ~location
    with
    | Ok spec -> spec
    | Error err ->
      Compiler_error.type_error location (Typing_ffi.Check.error_message err)
    in
    (* If @return(nullable), wrap the return type in option *)
    let final_type =
      if ffi_spec.Typing_ffi.Types.ffi_return_nullable then
        wrap_return_in_option external_type
      else
        external_type
    in
    (* Create identifier for this external *)
    let external_id = Identifier.create name in
    (* Add to environment as a value binding *)
    let scheme = Types.generalize final_type in
    let env = Environment.add_value name external_id scheme env in
    (* Create the typed external *)
    let typed_ext = {
      Typed_tree.external_id;
      external_type = final_type;
      external_spec = ffi_spec;
      external_location = location;
    } in
    let typed_item = {
      structure_item_desc = TypedStructureExternal typed_ext;
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

(** [infer_module_expression env mexpr] infers the type of a module expression.

    @param env The typing environment
    @param mexpr The module expression to infer
    @return A pair [(typed_mexpr, updated_env)] *)
and infer_module_expression env (mexpr : module_expression) =
  let loc = mexpr.Location.location in
  match mexpr.Location.value with
  | ModuleStructure structure ->
    (* Type-check the structure and infer its signature *)
    let typed_structure, _inner_env = infer_structure env structure in
    let sig_ = signature_of_typed_structure typed_structure in
    let module_type = Module_types.ModTypeSig sig_ in
    let typed_mexpr = {
      Typed_tree.module_desc = TypedModuleStructure typed_structure;
      module_type;
      module_location = loc;
    } in
    (typed_mexpr, env)

  | ModulePath module_path ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules loc in
    let internal_path = Module_type_check.module_path_to_internal_path base_binding.mod_id path_modules in
    let typed_mexpr = {
      Typed_tree.module_desc = TypedModulePath internal_path;
      module_type = mod_binding.mod_type;
      module_location = loc;
    } in
    (typed_mexpr, env)

  | ModuleFunctor (params, body) ->
    (* Type-check functor: functor (X : S) -> ME *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc "Functor must have at least one parameter"
    | [param] ->
      (* Single parameter functor *)
      let param_name = param.functor_param_name.Location.value in
      let param_mty = Module_type_check.check_module_type env param.functor_param_type in
      let param_id = Identifier.create param_name in
      let param_binding = Module_types.{
        mod_name = param_name;
        mod_id = param_id;
        mod_type = param_mty;
        mod_alias = None;
      } in
      (* Add parameter to environment for checking body *)
      let body_env = Environment.add_module param_name param_binding env in
      (* Type-check the body *)
      let (typed_body, _body_env) = infer_module_expression body_env body in
      (* Build the functor parameter *)
      let semantic_param = Module_types.{
        param_name;
        param_id;
        param_type = param_mty;
      } in
      (* Build functor type *)
      let functor_type = Module_types.ModTypeFunctor (semantic_param, typed_body.module_type) in
      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleFunctor (semantic_param, typed_body);
        module_type = functor_type;
        module_location = loc;
      } in
      (typed_mexpr, env)
    | param :: rest ->
      (* Multiple parameters - desugar to nested functors *)
      let inner_functor = {
        Location.value = ModuleFunctor (rest, body);
        location = loc;
      } in
      let single_param_functor = {
        Location.value = ModuleFunctor ([param], inner_functor);
        location = loc;
      } in
      infer_module_expression env single_param_functor
    end

  | ModuleApply (func_expr, arg_expr) ->
    (* Type-check functor application: F(M) *)
    let (typed_func, env) = infer_module_expression env func_expr in
    let (typed_arg, env) = infer_module_expression env arg_expr in
    (* Extract paths from typed expressions for applicative semantics *)
    let extract_path mexpr =
      match mexpr.Typed_tree.module_desc with
      | Typed_tree.TypedModulePath path -> Some path
      | _ -> None
    in
    let func_path_opt = extract_path typed_func in
    let arg_path_opt = extract_path typed_arg in
    (* Check that func has functor type *)
    begin match typed_func.module_type with
    | Module_types.ModTypeFunctor (param, result_mty) ->
      (* Check that argument matches parameter type *)
      begin match typed_arg.module_type, param.param_type with
      | Module_types.ModTypeSig arg_sig, Module_types.ModTypeSig param_sig ->
        setup_signature_matching env;
        begin match Signature_match.match_signature loc arg_sig param_sig with
        | Ok () ->
          (* Matching succeeded - apply applicative semantics *)
          let final_mty =
            match func_path_opt, arg_path_opt with
            | Some func_path, Some arg_path ->
              (* 1. Substitute parameter path with argument path in result *)
              let param_path = Types.PathIdent param.param_id in
              let substituted_mty = Signature_match.substitute_path_in_module_type param_path arg_path result_mty in
              (* 2. Create PathApply for the result and strengthen *)
              let apply_path = Types.PathApply (func_path, arg_path) in
              Signature_match.strengthen_module_type apply_path substituted_mty
            | _ ->
              (* Complex expressions - just use result type directly *)
              result_mty
          in
          let typed_mexpr = {
            Typed_tree.module_desc = TypedModuleApply (typed_func, typed_arg);
            module_type = final_mty;
            module_location = loc;
          } in
          (typed_mexpr, env)
        | Error err ->
          Compiler_error.type_error loc
            (Printf.sprintf "Functor argument does not match parameter: %s"
              (Signature_match.format_match_error err))
        end
      | _ ->
        (* Non-signature types - just pass through *)
        let typed_mexpr = {
          Typed_tree.module_desc = TypedModuleApply (typed_func, typed_arg);
          module_type = result_mty;
          module_location = loc;
        } in
        (typed_mexpr, env)
      end
    | _ ->
      Compiler_error.type_error loc "Cannot apply non-functor module"
    end

  | ModuleConstraint (inner_mexpr, constraint_mty) ->
    (* Type-check the inner expression *)
    let (typed_inner, env) = infer_module_expression env inner_mexpr in
    (* Check the constraint module type *)
    let spec_mty = Module_type_check.check_module_type env constraint_mty in
    (* Match implementation against specification *)
    setup_signature_matching env;
    begin match Signature_match.match_module_type loc typed_inner.module_type spec_mty with
    | Ok () ->
      (* Matching succeeded - return with the constrained (more restrictive) type *)
      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleConstraint (typed_inner, spec_mty);
        module_type = spec_mty;  (* Use spec type to hide implementation details *)
        module_location = loc;
      } in
      (typed_mexpr, env)
    | Error err ->
      Compiler_error.type_error loc (Signature_match.format_match_error err)
    end

(** Extract a signature from a typed structure *)
and signature_of_typed_structure (structure : typed_structure) : Module_types.signature =
  List.concat_map signature_items_of_typed_structure_item structure

(** Extract signature items from a single structure item (may return multiple items) *)
and signature_items_of_typed_structure_item (item : typed_structure_item) : Module_types.signature_item list =
  match item.structure_item_desc with
  | TypedStructureValue (_rec_flag, bindings) ->
    (* Extract signature items for ALL bindings (including mutual recursion) *)
    List.filter_map (fun (binding : typed_binding) ->
      match binding.binding_pattern.pattern_desc with
      | TypedPatternVariable id ->
        let name = Identifier.name id in
        let scheme = Types.generalize binding.binding_expression.expression_type in
        let val_desc = Module_types.{
          val_type = scheme;
          val_location = item.structure_item_location;
        } in
        Some (Module_types.SigValue (name, val_desc))
      | _ -> None
    ) bindings

  | TypedStructureType type_decls ->
    (* Return ALL type declarations as signature items *)
    List.map (fun decl ->
      Module_types.SigType (decl.declaration_name, decl)
    ) type_decls

  | TypedStructureModule (name_id, typed_mexpr) ->
    [Module_types.SigModule (Identifier.name name_id, typed_mexpr.module_type)]

  | TypedStructureModuleType (name, mty) ->
    [Module_types.SigModuleType (name, Some mty)]

  | TypedStructureOpen _ ->
    []  (* Open doesn't contribute to the signature *)

  | TypedStructureInclude (mexpr, _included_bindings) ->
    (* Include contributes all items from the included module's signature *)
    begin match mexpr.module_type with
    | Module_types.ModTypeSig included_sig -> included_sig
    | _ -> []
    end

  | TypedStructureExternal ext ->
    (* External contributes a value to the signature *)
    let name = Identifier.name ext.external_id in
    let scheme = Types.generalize ext.external_type in
    let val_desc = Module_types.{
      val_type = scheme;
      val_location = ext.external_location;
    } in
    [Module_types.SigValue (name, val_desc)]

(** [infer_structure env structure] infers types for a complete structure.

    @param env The typing environment
    @param structure The structure to infer
    @return A pair [(typed_structure, updated_env)] *)
and infer_structure env structure =
  let typed_items, env =
    List.fold_left (fun (items, env) item ->
      let typed_item, env = infer_structure_item env item in
      (typed_item :: items, env)
    ) ([], env) structure
  in
  (List.rev typed_items, env)

(** [infer_structure_tolerant env structure] infers types for a structure,
    continuing after errors to accumulate as much environment as possible.

    This is used by LSP features like completion that need the environment
    even when the code has errors (e.g., incomplete expressions being typed).

    @param env The typing environment
    @param structure The structure to infer
    @return A triple [(typed_structure_opt, accumulated_env, errors)] where
            typed_structure_opt is Some if all items succeeded, None if any failed,
            and errors is the list of errors encountered *)
and infer_structure_tolerant env structure =
  let typed_items, env, errors =
    List.fold_left (fun (items, env, errors) item ->
      try
        let typed_item, env = infer_structure_item env item in
        (typed_item :: items, env, errors)
      with
      | Common.Compiler_error.Error compiler_err ->
          (* Record error and continue with current env *)
          (items, env, CompilerError compiler_err :: errors)
      | Unification.Unification_error { expected; actual; location; message } ->
          (* Record error and continue with current env *)
          let err_details = { expected; actual; location; message } in
          (items, env, UnificationError err_details :: errors)
    ) ([], env, []) structure
  in
  let typed_ast = if errors <> [] then None else Some (List.rev typed_items) in
  (typed_ast, env, List.rev errors)
