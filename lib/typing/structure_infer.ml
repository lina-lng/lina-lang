(** Structure and module expression type inference.

    This module handles type inference for:
    - Structure items (let bindings, type declarations, modules)
    - Module expressions (struct, path, functor, apply, constraint)
    - Module type checking and signature matching
    - Signature extraction from typed structures

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable]. *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Create a signature matching context from a typing context.
    Delegates to {!Inference_utils.make_match_context}. *)
let make_match_context = Inference_utils.make_match_context

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

(** Process a type declaration and add it to the context *)
let process_type_declaration ctx (type_decl : Parsing.Syntax_tree.type_declaration) =
  let env = Typing_context.environment ctx in
  (* Create semantic type variables for each type parameter *)
  let type_params, ctx =
    List.fold_left (fun (params, ctx) _ ->
      let tv, ctx = Typing_context.new_type_variable ctx in
      match tv with
      | TypeVariable tv -> (params @ [tv], ctx)
      | _ ->
        (* new_type_variable always returns a TypeVariable *)
        Compiler_error.internal_error
          "new_type_variable did not return a TypeVariable"
    ) ([], ctx) type_decl.type_parameters
  in
  (* Build the result type: T or 'a T or ('a, 'b) T *)
  let result_type =
    TypeConstructor (PathLocal type_decl.type_name.Location.value,
      List.map (fun tv -> TypeVariable tv) type_params)
  in
  (* Get parameter names for mapping (e.g., ["'a"; "'b"]) *)
  let param_names = List.map (fun (param : type_parameter) -> param.parameter_name) type_decl.type_parameters in

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

  (* Create a context with the type added for recursive references *)
  let ctx_with_type = Typing_context.with_environment env_with_type ctx in

  let (declaration_kind, manifest, ctx) = match type_decl.type_kind with
    | TypeAbstract -> (DeclarationAbstract, None, ctx)
    | TypeVariant constructors ->
      let indexed_constructors = List.mapi (fun index ctor -> (index, ctor)) constructors in
      let constructor_infos, ctx =
        List.fold_left (fun (infos, ctx) (tag_index, (ctor : constructor_declaration)) ->
          (* Parse the constructor argument type using the parameter mapping.
             This ensures that 'a in "Some of 'a" refers to the same variable
             as the 'a type parameter. Use ctx_with_type so recursive references work. *)
          let arg_type, ctx = match ctor.constructor_argument with
            | Some ty_expr ->
              let ty, ctx = Module_type_check.check_type_expression_with_params ctx_with_type param_names type_params ty_expr in
              (Some ty, ctx)
            | None -> (None, ctx)
          in
          let ctor_info = {
            constructor_name = ctor.constructor_name.Location.value;
            constructor_tag_index = tag_index;
            constructor_type_name = type_decl.type_name.Location.value;
            constructor_argument_type = arg_type;
            constructor_result_type = result_type;
            constructor_type_parameters = type_params;
          } in
          (infos @ [ctor_info], ctx)
        ) ([], ctx) indexed_constructors
      in
      (DeclarationVariant constructor_infos, None, ctx)
    | TypeAlias ty_expr ->
      (* Convert syntax type expression to semantic type, preserving parameter sharing *)
      let manifest_type, ctx = Module_type_check.check_type_expression_with_params ctx_with_type param_names type_params ty_expr in
      (DeclarationAbstract, Some manifest_type, ctx)
  in
  (* Create the final type declaration with proper kind *)

  (* Convert explicit variance annotations from syntax to Types.variance *)
  let explicit_variances =
    List.map (fun (param : type_parameter) ->
      match param.parameter_variance with
      | Some VarianceCovariant -> Some Types.Covariant
      | Some VarianceContravariant -> Some Types.Contravariant
      | None -> None
    ) type_decl.type_parameters
  in

  (* Infer variances from the type definition *)
  let inferred_variances = Variance_infer.infer_declaration_variances
    type_params declaration_kind in

  (* Validate that explicit annotations are compatible with inferred variances *)
  let () =
    match Variance_infer.validate_annotations
      ~param_names:param_names
      ~explicit:explicit_variances
      ~inferred:inferred_variances
    with
    | Ok () -> ()
    | Error err ->
      Compiler_error.type_error type_decl.type_location
        (Variance_infer.format_variance_error err)
  in

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

  (* Check for type alias cycles *)
  let () =
    try
      Cycle_check.check_type_definition
        ~env:env_with_type
        ~loc:type_decl.type_location
        type_decl.type_name.Location.value
        type_params
        declaration_kind
        manifest
    with Cycle_check.Cycle_detected err ->
      Compiler_error.type_error type_decl.type_location
        (Cycle_check.format_error err)
  in

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
  let ctx = Typing_context.with_environment env ctx in
  (type_declaration, ctx)


(** [infer_structure_item ctx item] infers types for a structure item.

    @param ctx The typing context
    @param item The structure item to infer
    @return A pair [(typed_item, updated_ctx)] *)
let rec infer_structure_item ctx (item : structure_item) =
  let env = Typing_context.environment ctx in
  match item.Location.value with
  | StructureValue (rec_flag, bindings) ->
    let typed_bindings, ctx = Expression_infer.infer_bindings ctx rec_flag bindings in
    let typed_item = {
      structure_item_desc = TypedStructureValue (rec_flag, typed_bindings);
      structure_item_location = item.Location.location;
    } in
    (typed_item, ctx)

  | StructureType type_decls ->
    let type_declarations, ctx =
      List.fold_left (fun (decls, ctx) type_decl ->
        let decl, ctx = process_type_declaration ctx type_decl in
        (decl :: decls, ctx)
      ) ([], ctx) type_decls
    in
    let typed_item = {
      structure_item_desc = TypedStructureType (List.rev type_declarations);
      structure_item_location = item.Location.location;
    } in
    (typed_item, ctx)

  | StructureModule module_binding ->
    let module_name = module_binding.module_name.Location.value in
    (* Desugar functor shorthand: module F(X : S) = ME becomes module F = functor (X : S) -> ME *)
    let actual_expr = match module_binding.module_params with
      | [] -> module_binding.module_expr
      | params ->
        let loc = module_binding.module_location in
        { Location.value = ModuleFunctor (params, module_binding.module_expr); location = loc }
    in
    let typed_mexpr, ctx = infer_module_expression ctx actual_expr in
    (* Check if there's a signature constraint *)
    let final_mexpr, final_mty, ctx =
      match module_binding.module_type with
      | None ->
        (* No constraint - use inferred type *)
        (typed_mexpr, typed_mexpr.module_type, ctx)
      | Some constraint_mty ->
        (* Check the constraint and perform signature matching *)
        let spec_mty, ctx = Module_type_check.check_module_type ctx constraint_mty in
        let match_ctx = make_match_context ctx in
        begin match Signature_match.match_module_type match_ctx item.Location.location typed_mexpr.module_type spec_mty with
        | Ok () ->
          (* Matching succeeded - use the constrained type *)
          let constrained_mexpr = {
            Typed_tree.module_desc = TypedModuleConstraint (typed_mexpr, spec_mty);
            module_type = spec_mty;
            module_location = typed_mexpr.module_location;
          } in
          (constrained_mexpr, spec_mty, ctx)
        | Error err ->
          Compiler_error.type_error item.Location.location
            (Printf.sprintf "Module %s does not match signature: %s"
              module_name (Signature_match.format_match_error err))
        end
    in
    let env = Typing_context.environment ctx in
    (* Register the module in the environment *)
    let module_id = Identifier.create module_name in
    (* Strengthen the module type so that abstract types become concrete *)
    let module_path = Types.PathIdent module_id in
    let strengthened_mty = Signature_match.strengthen_module_type module_path final_mty in
    let mod_binding = Module_types.{
      binding_name = module_name;
      binding_id = module_id;
      binding_type = strengthened_mty;
      binding_alias = None;
    } in
    let env = Environment.add_module module_name mod_binding env in
    let ctx = Typing_context.with_environment env ctx in
    let typed_item = {
      structure_item_desc = TypedStructureModule (module_id, final_mexpr);
      structure_item_location = item.Location.location;
    } in
    (typed_item, ctx)

  | StructureModuleType (name_loc, mty) ->
    (* Check the module type expression *)
    let name = name_loc.Location.value in
    let resolved_mty, ctx = Module_type_check.check_module_type ctx mty in
    let env = Typing_context.environment ctx in
    (* Add the module type to the environment *)
    let env = Environment.add_module_type name (Some resolved_mty) env in
    let ctx = Typing_context.with_environment env ctx in
    let typed_item = {
      structure_item_desc = TypedStructureModuleType (name, resolved_mty);
      structure_item_location = item.Location.location;
    } in
    (typed_item, ctx)

  | StructureOpen module_path ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules item.Location.location in
    begin match mod_binding.Module_types.binding_type with
    | Module_types.ModTypeSig sig_ ->
      let (env, opened_bindings) = Environment.open_module sig_ env in
      let ctx = Typing_context.with_environment env ctx in
      let path = Module_type_check.module_path_to_internal_path base_binding.binding_id path_modules in
      let typed_item = {
        structure_item_desc = TypedStructureOpen (path, opened_bindings);
        structure_item_location = item.Location.location;
      } in
      (typed_item, ctx)
    | Module_types.ModTypeFunctor _ ->
      Compiler_error.type_error item.Location.location "Cannot open a functor"
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error item.Location.location "Cannot open an abstract module type"
    end

  | StructureInclude mexpr ->
    (* Type-check the included module expression *)
    let (typed_mexpr, ctx) = infer_module_expression ctx mexpr in
    begin match typed_mexpr.module_type with
    | Module_types.ModTypeSig sig_ ->
      (* Bring all signature contents into scope (like open) *)
      let env = Typing_context.environment ctx in
      let (env, included_bindings) = Environment.open_module sig_ env in
      let ctx = Typing_context.with_environment env ctx in
      let typed_item = {
        structure_item_desc = TypedStructureInclude (typed_mexpr, included_bindings);
        structure_item_location = item.Location.location;
      } in
      (typed_item, ctx)
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
    let external_type, ctx = Module_type_check.check_type_expression ctx ext_decl.external_type in
    let env = Typing_context.environment ctx in
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
    let scheme = Typing_context.generalize ctx final_type in
    let env = Environment.add_value name external_id scheme env in
    let ctx = Typing_context.with_environment env ctx in
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
    (typed_item, ctx)

(** [infer_module_expression ctx mexpr] infers the type of a module expression.

    @param ctx The typing context
    @param mexpr The module expression to infer
    @return A pair [(typed_mexpr, updated_ctx)] *)
and infer_module_expression ctx (mexpr : module_expression) =
  let env = Typing_context.environment ctx in
  let loc = mexpr.Location.location in
  match mexpr.Location.value with
  | ModuleStructure structure ->
    (* Type-check the structure and infer its signature *)
    let typed_structure, _inner_ctx = infer_structure ctx structure in
    let sig_ = signature_of_typed_structure typed_structure in
    let module_type = Module_types.ModTypeSig sig_ in
    let typed_mexpr = {
      Typed_tree.module_desc = TypedModuleStructure typed_structure;
      module_type;
      module_location = loc;
    } in
    (typed_mexpr, ctx)

  | ModulePath module_path ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = Module_type_check.lookup_module_path env path_modules loc in
    let internal_path = Module_type_check.module_path_to_internal_path base_binding.binding_id path_modules in
    let typed_mexpr = {
      Typed_tree.module_desc = TypedModulePath internal_path;
      module_type = mod_binding.binding_type;
      module_location = loc;
    } in
    (typed_mexpr, ctx)

  | ModuleFunctor (params, body) ->
    (* Type-check functor: functor (X : S) -> ME *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc "Functor must have at least one parameter"
    | [param] ->
      (* Single parameter functor *)
      let parameter_name = param.functor_param_name.Location.value in
      let parameter_type, ctx = Module_type_check.check_module_type ctx param.functor_param_type in
      let env = Typing_context.environment ctx in
      let parameter_id = Identifier.create parameter_name in
      let param_binding = Module_types.{
        binding_name = parameter_name;
        binding_id = parameter_id;
        binding_type = parameter_type;
        binding_alias = None;
      } in
      (* Add parameter to environment for checking body *)
      let body_env = Environment.add_module parameter_name param_binding env in
      let body_ctx = Typing_context.with_environment body_env ctx in
      (* Type-check the body *)
      let (typed_body, _body_ctx) = infer_module_expression body_ctx body in
      (* Build the functor parameter *)
      let semantic_param = Module_types.{
        parameter_name;
        parameter_id;
        parameter_type;
      } in
      (* Build functor type *)
      let functor_type = Module_types.ModTypeFunctor (semantic_param, typed_body.module_type) in
      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleFunctor (semantic_param, typed_body);
        module_type = functor_type;
        module_location = loc;
      } in
      (typed_mexpr, ctx)
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
      infer_module_expression ctx single_param_functor
    end

  | ModuleApply (func_expr, arg_expr) ->
    (* Type-check functor application: F(M) *)
    let (typed_func, ctx) = infer_module_expression ctx func_expr in
    let (typed_arg, ctx) = infer_module_expression ctx arg_expr in
    (* Extract paths from typed expressions for applicative semantics *)
    let func_path_opt = Inference_utils.extract_typed_module_path typed_func in
    let arg_path_opt = Inference_utils.extract_typed_module_path typed_arg in
    (* Check that func has functor type *)
    begin match typed_func.module_type with
    | Module_types.ModTypeFunctor (param, result_mty) ->
      (* Check that argument matches parameter type *)
      begin match typed_arg.module_type, param.parameter_type with
      | Module_types.ModTypeSig arg_sig, Module_types.ModTypeSig param_sig ->
        let match_ctx = make_match_context ctx in
        begin match Signature_match.match_signature match_ctx loc arg_sig param_sig with
        | Ok () ->
          (* Matching succeeded - apply applicative semantics *)
          let final_mty =
            match func_path_opt, arg_path_opt with
            | Some func_path, Some arg_path ->
              (* 1. Substitute parameter path with argument path in result *)
              let param_path = Types.PathIdent param.parameter_id in
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
          (typed_mexpr, ctx)
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
        (typed_mexpr, ctx)
      end
    | _ ->
      Compiler_error.type_error loc "Cannot apply non-functor module"
    end

  | ModuleConstraint (inner_mexpr, constraint_mty) ->
    (* Type-check the inner expression *)
    let (typed_inner, ctx) = infer_module_expression ctx inner_mexpr in
    (* Check the constraint module type *)
    let spec_mty, ctx = Module_type_check.check_module_type ctx constraint_mty in
    (* Match implementation against specification *)
    let match_ctx = make_match_context ctx in
    begin match Signature_match.match_module_type match_ctx loc typed_inner.module_type spec_mty with
    | Ok () ->
      (* Matching succeeded - return with the constrained (more restrictive) type *)
      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleConstraint (typed_inner, spec_mty);
        module_type = spec_mty;  (* Use spec type to hide implementation details *)
        module_location = loc;
      } in
      (typed_mexpr, ctx)
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
        (* Apply value restriction for top-level generalization *)
        let scheme = Inference_utils.compute_binding_scheme ~level:1
          binding.binding_expression binding.binding_expression.expression_type in
        let val_desc = Module_types.{
          value_type = scheme;
          value_location = item.structure_item_location;
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
    (* Externals are always syntactic values (primitives), so full generalization
       is safe - no need for value restriction check *)
    let scheme = Type_scheme.generalize ~level:1 ext.external_type in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = ext.external_location;
    } in
    [Module_types.SigValue (name, val_desc)]

(** [infer_structure ctx structure] infers types for a complete structure.

    @param ctx The typing context
    @param structure The structure to infer
    @return A pair [(typed_structure, updated_ctx)] *)
and infer_structure ctx structure =
  let typed_items, ctx =
    List.fold_left (fun (items, ctx) item ->
      let typed_item, ctx = infer_structure_item ctx item in
      (typed_item :: items, ctx)
    ) ([], ctx) structure
  in
  (List.rev typed_items, ctx)

(** [infer_structure_tolerant ctx structure] infers types for a structure,
    continuing after errors to accumulate as much context as possible.

    This is used by LSP features like completion that need the environment
    even when the code has errors (e.g., incomplete expressions being typed).

    @param ctx The typing context
    @param structure The structure to infer
    @return A triple [(typed_structure_opt, accumulated_ctx, errors)] where
            typed_structure_opt is Some if all items succeeded, None if any failed,
            and errors is the list of errors encountered *)
and infer_structure_tolerant ctx structure =
  let typed_items, ctx, errors =
    List.fold_left (fun (items, ctx, errors) item ->
      try
        let typed_item, ctx = infer_structure_item ctx item in
        (typed_item :: items, ctx, errors)
      with
      | Common.Compiler_error.Error compiler_err ->
          (* Record error and continue with current ctx *)
          (items, ctx, Inference_utils.CompilerError compiler_err :: errors)
      | Unification.Unification_error { expected; actual; location; message } ->
          (* Record error and continue with current ctx *)
          let err_details : Inference_utils.unification_error_details = { expected; actual; location; message } in
          (items, ctx, Inference_utils.UnificationError err_details :: errors)
    ) ([], ctx, []) structure
  in
  let typed_ast = if errors <> [] then None else Some (List.rev typed_items) in
  (typed_ast, ctx, List.rev errors)
