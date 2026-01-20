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
  | Types.TypeArrow (_, arg, ret) ->
    Types.TypeArrow (Nolabel, arg, wrap_return_in_option ret)
  | _ when is_option_type ty ->
    (* Already option, don't double-wrap *)
    ty
  | _ ->
    (* Wrap the final return type in option *)
    Types.TypeConstructor (Types.PathLocal "option", [ty])

(** Check if a type is already a result type. *)
let is_result_type ty =
  match ty with
  | Types.TypeConstructor (Types.PathLocal "result", [_; _]) -> true
  | _ -> false

(** Wrap the return type of a function in result for @return(pcall).
    For example: (unit -> int) -> int becomes (unit -> int) -> (int, string) result.
    If the return type is already result, don't double-wrap it. *)
let rec wrap_return_in_result ty =
  match ty with
  | Types.TypeArrow (_, arg, ret) ->
    Types.TypeArrow (Nolabel, arg, wrap_return_in_result ret)
  | _ when is_result_type ty ->
    (* Already result, don't double-wrap *)
    ty
  | _ ->
    (* Wrap the final return type in (ty, string) result *)
    Types.TypeConstructor (Types.PathLocal "result", [ty; type_string])

(** Process a type declaration with pre-created type parameters.
    Used for mutually recursive type definitions where all type names
    must be added to the environment before processing any definitions. *)
let rec process_type_declaration_with_params ctx type_decl type_params =
  let env = Typing_context.environment ctx in

  (* Build the result type: T or 'a T or ('a, 'b) T *)
  let result_type =
    TypeConstructor (PathLocal type_decl.type_name.Location.value,
      List.map (fun tv -> TypeVariable tv) type_params)
  in

  (* Get parameter names for mapping *)
  let param_names = List.map (fun (param : type_parameter) -> param.parameter_name) type_decl.type_parameters in

  (* The type is already in the environment from first pass *)
  let ctx_with_type = ctx in

  let (declaration_kind, manifest, ctx) = match type_decl.type_kind with
    | TypeAbstract -> (DeclarationAbstract, None, ctx)
    | TypeVariant constructors ->
      let type_name = type_decl.type_name.Location.value in
      let constructor_infos, ctx =
        Constructor_check.check_constructors ctx_with_type param_names type_params
          result_type type_name constructors
      in
      (DeclarationVariant constructor_infos, None, ctx)
    | TypeAlias ty_expr ->
      let manifest_type, ctx = Type_expression_check.check_type_expression_with_params ctx_with_type param_names type_params ty_expr in
      (DeclarationAbstract, Some manifest_type, ctx)
    | TypeExtensible ->
      (DeclarationExtensible, None, ctx)
  in

  (* Convert explicit variance annotations *)
  let explicit_variances =
    List.map (fun (param : type_parameter) ->
      match param.parameter_variance with
      | Some VarianceCovariant -> Some Types.Covariant
      | Some VarianceContravariant -> Some Types.Contravariant
      | None -> None
    ) type_decl.type_parameters
  in

  let inferred_variances = Variance_infer.infer_declaration_variances type_params declaration_kind in

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

  let final_variances = Variance_infer.merge_variances explicit_variances inferred_variances in

  (* Convert type constraints *)
  let semantic_constraints, ctx =
    List.fold_left (fun (constraints, ctx) (syntax_constraint : Parsing.Syntax_tree.type_constraint) ->
      let var_name = syntax_constraint.constraint_variable in
      let constraint_var = match List.assoc_opt var_name (List.combine param_names type_params) with
        | Some tv -> tv
        | None ->
          Compiler_error.type_error syntax_constraint.constraint_location
            (Printf.sprintf "The type variable `%s` in this constraint is not a parameter of this type.\n\n\
                             Constraints can only reference type parameters declared in the type definition."
               var_name)
      in
      let constraint_type, ctx =
        Type_expression_check.check_type_expression_with_params ctx param_names type_params
          syntax_constraint.constraint_type
      in
      let semantic_constraint = Types.{
        constraint_variable = constraint_var;
        constraint_type;
      } in
      (semantic_constraint :: constraints, ctx)
    ) ([], ctx) type_decl.type_constraints
  in
  let semantic_constraints = List.rev semantic_constraints in

  let is_datatype = match declaration_kind with
    | DeclarationVariant _ | DeclarationRecord _ | DeclarationExtensible -> true
    | DeclarationAbstract -> Option.is_none manifest
  in
  let final_injectivities = List.map2 (fun (param : type_parameter) _ ->
    param.parameter_injective || is_datatype
  ) type_decl.type_parameters type_params in

  let type_declaration = {
    declaration_name = type_decl.type_name.Location.value;
    declaration_parameters = type_params;
    declaration_variances = final_variances;
    declaration_injectivities = final_injectivities;
    declaration_manifest = manifest;
    declaration_kind;
    declaration_private = type_decl.type_private;
    declaration_constraints = semantic_constraints;
  } in

  (* Check for type alias cycles - use the current env which has all types *)
  let () =
    try
      Cycle_check.check_type_definition
        ~env
        ~loc:type_decl.type_location
        type_decl.type_name.Location.value
        type_params
        declaration_kind
        manifest
    with Cycle_check.Cycle_detected err ->
      Compiler_error.type_error type_decl.type_location
        (Cycle_check.format_error err)
  in

  (* Update environment with final declaration (replaces preliminary) *)
  let env = Environment.add_type type_decl.type_name.Location.value type_declaration env in
  let env = match declaration_kind with
    | DeclarationAbstract -> env
    | DeclarationVariant constructors ->
      List.fold_left (fun env ctor ->
        Environment.add_constructor ctor.constructor_name ctor env
      ) env constructors
    | DeclarationRecord _ -> env
    | DeclarationExtensible -> env
  in
  let ctx = Typing_context.with_environment env ctx in
  (type_declaration, ctx)


(** [infer_structure_item ctx item] infers types for a structure item.

    @param ctx The typing context
    @param item The structure item to infer
    @return A pair [(typed_item, updated_ctx)] *)
and infer_structure_item ctx (item : structure_item) =
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
    (* For mutually recursive types (defined with 'and'), we need to first add
       all type names to the environment before processing any definitions.
       This allows types to reference each other. *)

    (* First pass: collect all type names and create preliminary declarations *)
    let preliminary_decls_and_params, ctx =
      List.fold_left (fun (acc, ctx) type_decl ->
        let rev_type_params, ctx =
          List.fold_left (fun (rev_params, ctx) _ ->
            let tv, ctx = Typing_context.new_type_variable ctx in
            match tv with
            | TypeVariable tv -> (tv :: rev_params, ctx)
            | _ -> Compiler_error.internal_error "new_type_variable did not return TypeVariable"
          ) ([], ctx) type_decl.type_parameters
        in
        let type_params = List.rev rev_type_params in
        let preliminary_declaration = {
          Types.declaration_name = type_decl.type_name.Location.value;
          declaration_parameters = type_params;
          declaration_variances = List.map (fun _ -> Types.Covariant) type_params;
          declaration_injectivities = List.map (fun _ -> true) type_params;
          declaration_manifest = None;
          declaration_kind = Types.DeclarationAbstract;
          declaration_private = type_decl.type_private;
          declaration_constraints = [];
        } in
        ((type_decl, type_params, preliminary_declaration) :: acc, ctx)
      ) ([], ctx) type_decls
    in
    let preliminary_decls_and_params = List.rev preliminary_decls_and_params in

    (* Add all type names to environment *)
    let env_with_all_types =
      List.fold_left (fun env (type_decl, _, preliminary_decl) ->
        Environment.add_type type_decl.type_name.Location.value preliminary_decl env
      ) (Typing_context.environment ctx) preliminary_decls_and_params
    in
    let ctx = Typing_context.with_environment env_with_all_types ctx in

    (* Second pass: process each type declaration with all types visible *)
    let type_declarations, ctx =
      List.fold_left (fun (decls, ctx) (type_decl, type_params, _) ->
        let decl, ctx = process_type_declaration_with_params ctx type_decl type_params in
        (decl :: decls, ctx)
      ) ([], ctx) preliminary_decls_and_params
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
        (* For functor bindings with constraints, we need special handling.
           The constraint applies to the RESULT type, not the whole functor type.

           We use the functor parameters from the INFERRED type (typed_mexpr)
           so that path references like X.t are consistent. *)
        let rec extract_functor_parts mty =
          match mty with
          | Module_types.ModTypeFunctor (param, result) ->
            let params, final_result = extract_functor_parts result in
            (param :: params, final_result)
          | _ -> ([], mty)
        in
        let inferred_params, inferred_result = extract_functor_parts typed_mexpr.module_type in

        (* Build an environment with the inferred functor parameters for checking
           the constraint type. This ensures X.t references resolve correctly. *)
        let ctx_with_params =
          List.fold_left (fun ctx param ->
            match param with
            | Module_types.FunctorParamNamed { parameter_name; parameter_id; parameter_type } ->
              let param_binding = Module_types.{
                binding_name = parameter_name;
                binding_id = parameter_id;
                binding_type = parameter_type;
                binding_alias = None;
              } in
              let env = Typing_context.environment ctx in
              let env = Environment.add_module parameter_name param_binding env in
              Typing_context.with_environment env ctx
            | Module_types.FunctorParamUnit -> ctx
          ) ctx inferred_params
        in
        let result_spec_mty, _ctx_with_params = Module_type_check.check_module_type ctx_with_params constraint_mty in

        (* Match the inferred result against the specified result constraint *)
        let match_ctx = make_match_context ctx_with_params in
        begin match Signature_match.match_module_type match_ctx item.Location.location inferred_result result_spec_mty with
        | Ok () ->
          (* Matching succeeded - rebuild the full functor type with constraint result *)
          let spec_mty =
            List.fold_right (fun param acc ->
              Module_types.ModTypeFunctor (param, acc)
            ) inferred_params result_spec_mty
          in
          let constrained_mexpr = {
            Typed_tree.module_desc = TypedModuleConstraint (typed_mexpr, spec_mty);
            module_type = spec_mty;
            module_location = typed_mexpr.module_location;
          } in
          (constrained_mexpr, spec_mty, ctx)
        | Error err ->
          Compiler_error.type_error item.Location.location
            (Printf.sprintf "The module `%s` does not satisfy its signature constraint.\n\n%s"
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

  | StructureRecModule rec_bindings ->
    (* Recursive modules require a three-pass approach:
       1. First pass: Create identifiers and add modules with placeholder signatures
       2. Second pass: Check all signatures (all modules now in scope)
       3. Third pass: Type-check each module implementation *)

    (* First pass: Create identifiers and add modules with placeholder abstract signatures *)
    let rev_preliminary_infos, ctx = List.fold_left (fun (rev_infos, ctx) (binding : Parsing.Syntax_tree.rec_module_binding) ->
      let name = binding.rec_module_name.Location.value in
      let module_id = Identifier.create name in

      let placeholder_mty = Module_types.ModTypeSig [] in
      let mod_binding = Module_types.{
        binding_name = name;
        binding_id = module_id;
        binding_type = placeholder_mty;
        binding_alias = None;
      } in
      let env = Typing_context.environment ctx in
      let env = Environment.add_module name mod_binding env in
      let ctx = Typing_context.with_environment env ctx in

      ((module_id, binding) :: rev_infos, ctx)
    ) ([], ctx) rec_bindings in
    let preliminary_infos = List.rev rev_preliminary_infos in

    (* Second pass: Check all signatures and update environment with real types *)
    let rev_module_infos, ctx = List.fold_left (fun (rev_infos, ctx) (module_id, (binding : Parsing.Syntax_tree.rec_module_binding)) ->
      let name = binding.rec_module_name.Location.value in
      let declared_mty, ctx = Module_type_check.check_module_type ctx binding.rec_module_type in

      let module_path = Types.PathIdent module_id in
      let strengthened_mty = Signature_match.strengthen_module_type module_path declared_mty in

      let mod_binding = Module_types.{
        binding_name = name;
        binding_id = module_id;
        binding_type = strengthened_mty;
        binding_alias = None;
      } in
      let env = Typing_context.environment ctx in
      let env = Environment.add_module name mod_binding env in
      let ctx = Typing_context.with_environment env ctx in

      ((module_id, binding, declared_mty, strengthened_mty) :: rev_infos, ctx)
    ) ([], ctx) preliminary_infos in
    let module_infos = List.rev rev_module_infos in

    (* Third pass: Type-check each module implementation *)
    let rev_typed_bindings, ctx = List.fold_left (fun (rev_bindings, ctx) (module_id, (binding : Parsing.Syntax_tree.rec_module_binding), declared_mty, _strengthened_mty) ->
      let typed_mexpr, ctx = infer_module_expression ctx binding.rec_module_expr in

      let match_ctx = make_match_context ctx in
      begin match Signature_match.match_module_type match_ctx binding.rec_module_location typed_mexpr.module_type declared_mty with
      | Ok () ->
        let final_mexpr = {
          Typed_tree.module_desc = TypedModuleConstraint (typed_mexpr, declared_mty);
          module_type = declared_mty;
          module_location = typed_mexpr.module_location;
        } in
        let typed_binding = {
          Typed_tree.rec_module_id = module_id;
          rec_module_type = declared_mty;
          rec_module_expr = final_mexpr;
          rec_module_location = binding.rec_module_location;
        } in
        (typed_binding :: rev_bindings, ctx)
      | Error err ->
        let name = binding.rec_module_name.Location.value in
        Compiler_error.type_error binding.rec_module_location
          (Printf.sprintf "The recursive module `%s` does not satisfy its declared signature.\n\n%s"
            name (Signature_match.format_match_error err))
      end
    ) ([], ctx) module_infos in
    let typed_bindings = List.rev rev_typed_bindings in

    let typed_item = {
      structure_item_desc = TypedStructureRecModule typed_bindings;
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
      Compiler_error.type_error item.Location.location
        "We can't open a functor directly.\n\n\
         Functors must be applied to an argument before their contents can be accessed. \
         Try `open F(SomeModule)` instead."
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error item.Location.location
        "We can't open an abstract module type.\n\n\
         The module's contents are hidden by its signature."
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
      Compiler_error.type_error item.Location.location
        "We can't include a functor directly.\n\n\
         Functors must be applied to an argument before their contents can be included. \
         Try `include F(SomeModule)` instead."
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error item.Location.location
        "We can't include an abstract module type.\n\n\
         The module's contents are hidden by its signature."
    end

  | StructureExternal ext_decl ->
    (* Type-check the external declaration *)
    let name = ext_decl.external_name.Location.value in
    let location = ext_decl.external_location in
    (* Check the type expression *)
    let external_type, ctx = Type_expression_check.check_type_expression ctx ext_decl.external_type in
    let env = Typing_context.environment ctx in
    (* Compute arity and unit_params from type (count function arrows) *)
    let rec compute_arity_and_unit_params ty =
      match ty with
      | Types.TypeArrow (_, arg_ty, ret) ->
          let is_unit = match arg_ty with
            | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinUnit, []) -> true
            | _ -> false
          in
          let rest_arity, rest_unit_params = compute_arity_and_unit_params ret in
          (1 + rest_arity, is_unit :: rest_unit_params)
      | _ -> (0, [])
    in
    let arity, unit_params = compute_arity_and_unit_params external_type in
    (* Build and validate the FFI spec *)
    let ffi_spec = match Typing_ffi.Check.build_ffi_spec
      ~attrs:ext_decl.external_attributes
      ~primitive:ext_decl.external_primitive
      ~arity
      ~unit_params
      ~location
    with
    | Ok spec -> spec
    | Error err ->
      Compiler_error.type_error location (Typing_ffi.Check.error_message err)
    in
    (* If @return(nullable), wrap the return type in option.
       If @return(pcall), wrap the return type in result. *)
    let final_type =
      if ffi_spec.Typing_ffi.Types.ffi_return_nullable then
        wrap_return_in_option external_type
      else if ffi_spec.Typing_ffi.Types.ffi_return_pcall then
        wrap_return_in_result external_type
      else
        external_type
    in
    (* Create identifier for this external *)
    let external_id = Identifier.create name in
    (* Add to environment as a value binding *)
    let scheme = Typing_context.generalize ctx final_type in
    let env = Environment.add_value name external_id scheme location env in
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

  | StructureTypeExtension ext ->
    (* Type extension: type t += Constructor1 | Constructor2 of ty *)
    let env = Typing_context.environment ctx in
    let loc = item.Location.location in

    (* 1. Resolve the type path being extended *)
    let type_path = Expression_infer.longident_to_path ext.extension_type_name in
    let type_name = match type_path with
      | PathLocal name -> name
      | PathDot (_, name) -> name
      | _ -> Compiler_error.type_error loc
               "This type path is not valid for a type extension.\n\n\
                Type extensions must reference a named type like `t` or `M.t`."
    in

    (* 2. Look up the type declaration and verify it's extensible *)
    let type_decl = match Environment.find_type_by_path type_path env with
      | Some decl -> decl
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "We couldn't find a type named `%s` to extend.\n\n\
                           Make sure the type is defined and in scope."
             (Types.path_to_string type_path))
    in

    begin match type_decl.declaration_kind with
    | DeclarationExtensible -> ()
    | _ ->
      Compiler_error.type_error loc
        (Printf.sprintf "The type `%s` is not extensible.\n\n\
                         Only types declared with `type t = ..` (extensible types) can be extended. \
                         Regular variant types cannot have new constructors added."
           (Types.path_to_string type_path))
    end;

    (* 3. Create fresh type parameters for the extension *)
    let param_names = List.map (fun (p : type_parameter) -> p.parameter_name) ext.extension_type_params in
    let rev_type_params, ctx = List.fold_left (fun (rev_params, ctx) _ ->
      let tv, ctx = Typing_context.new_type_variable ctx in
      match tv with
      | TypeVariable tv -> (tv :: rev_params, ctx)
      | _ -> Compiler_error.internal_error "new_type_variable must return TypeVariable"
    ) ([], ctx) ext.extension_type_params in
    let type_params = List.rev rev_type_params in

    (* Verify parameter count matches *)
    if List.length type_params <> List.length type_decl.declaration_parameters then
      Compiler_error.type_error loc
        (Printf.sprintf "This type extension has %d type parameter(s), but `%s` was declared with %d.\n\n\
                         The number of type parameters in an extension must match the original type definition."
          (List.length type_params)
          (Types.path_to_string type_path)
          (List.length type_decl.declaration_parameters));

    (* Build result type: t applied to parameters *)
    let result_type = TypeConstructor (type_path, List.map (fun tv -> TypeVariable tv) type_params) in

    (* Add type to context for recursive references in constructor arguments *)
    let env_with_type = Environment.add_type type_name type_decl env in
    let ctx_with_type = Typing_context.with_environment env_with_type ctx in

    (* 4. Process each constructor *)
    let rev_constructor_infos, ctx =
      List.fold_left (fun (rev_infos, ctx) (ctor : constructor_declaration) ->
        let arg_type, ctx = match ctor.constructor_argument with
          | Some ty_expr ->
            let ty, ctx = Type_expression_check.check_type_expression_with_params ctx_with_type param_names type_params ty_expr in
            (Some ty, ctx)
          | None -> (None, ctx)
        in

        let ctor_info = {
          constructor_name = ctor.constructor_name.Location.value;
          constructor_tag_index = -1;
          constructor_type_name = type_name;
          constructor_argument_type = arg_type;
          constructor_result_type = result_type;
          constructor_type_parameters = type_params;
          constructor_is_gadt = false;
          constructor_existentials = [];
        } in
        (ctor_info :: rev_infos, ctx)
      ) ([], ctx) ext.extension_constructors
    in
    let constructor_infos = List.rev rev_constructor_infos in

    (* 5. Add constructors to environment *)
    let env = Typing_context.environment ctx in
    let env = List.fold_left (fun env ctor ->
      Environment.add_constructor ctor.constructor_name ctor env
    ) env constructor_infos in
    let ctx = Typing_context.with_environment env ctx in

    (* 6. Create typed structure item *)
    let typed_ext = {
      extension_type_path = type_path;
      extension_type_params = type_params;
      extension_constructors = constructor_infos;
      extension_location = loc;
    } in
    let typed_item = {
      structure_item_desc = TypedStructureTypeExtension typed_ext;
      structure_item_location = loc;
    } in
    (typed_item, ctx)

  | StructureExpression expr ->
    (* Top-level expression (e.g., let x = 1 in x, or print "hello") *)
    let typed_expr, ctx = Expression_infer.infer_expression ctx expr in
    let typed_item = {
      structure_item_desc = TypedStructureExpression typed_expr;
      structure_item_location = item.Location.location;
    } in
    (typed_item, ctx)

  | StructureError error_info ->
    (* Error structure items are preserved in typed tree *)
    let typed_item = {
      structure_item_desc = TypedStructureError error_info;
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
    (* Type-check functor: functor (X : S) -> ME or functor () -> ME *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc
        "A functor must have at least one parameter.\n\n\
         Use `functor (X : S) -> ...` for applicative functors or `functor () -> ...` for generative functors."
    | [param] ->
      begin match param with
      | FunctorParamNamed (name_loc, param_type) ->
        (* Named parameter functor (applicative) *)
        let parameter_name = name_loc.Location.value in
        let parameter_type, ctx = Module_type_check.check_module_type ctx param_type in
        let env = Typing_context.environment ctx in
        let parameter_id = Identifier.create parameter_name in
        (* Strengthen the parameter type so that types like `t` in the signature
           become qualified as `S.t` when accessed from the functor body.
           This is essential for functor application to work correctly. *)
        let parameter_path = Types.PathIdent parameter_id in
        let strengthened_param_type = Signature_match.strengthen_module_type parameter_path parameter_type in
        let param_binding = Module_types.{
          binding_name = parameter_name;
          binding_id = parameter_id;
          binding_type = strengthened_param_type;
          binding_alias = None;
        } in
        (* Add parameter to environment for checking body *)
        let body_env = Environment.add_module parameter_name param_binding env in
        let body_ctx = Typing_context.with_environment body_env ctx in
        (* Type-check the body *)
        let (typed_body, _body_ctx) = infer_module_expression body_ctx body in
        (* Build the functor parameter *)
        let semantic_param = Module_types.FunctorParamNamed {
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
      | FunctorParamUnit _ ->
        (* Unit parameter functor (generative) *)
        let (typed_body, _body_ctx) = infer_module_expression ctx body in
        let semantic_param = Module_types.FunctorParamUnit in
        let functor_type = Module_types.ModTypeFunctor (semantic_param, typed_body.module_type) in
        let typed_mexpr = {
          Typed_tree.module_desc = TypedModuleFunctor (semantic_param, typed_body);
          module_type = functor_type;
          module_location = loc;
        } in
        (typed_mexpr, ctx)
      end
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
    (* Type-check functor application: F(M) or F() *)
    let (typed_func, ctx) = infer_module_expression ctx func_expr in
    (* Extract paths from typed expressions for applicative semantics *)
    let func_path_opt = Inference_utils.extract_typed_module_path typed_func in
    (* Check that func has functor type *)
    begin match typed_func.module_type with
    | Module_types.ModTypeFunctor (param, result_mty) ->
      begin match param with
      | Module_types.FunctorParamNamed { parameter_id; parameter_type; _ } ->
        (* Applicative functor - check argument matches parameter *)
        let (typed_arg, ctx) = infer_module_expression ctx arg_expr in
        let arg_path_opt = Inference_utils.extract_typed_module_path typed_arg in
        begin match typed_arg.module_type, parameter_type with
        | Module_types.ModTypeSig arg_sig, Module_types.ModTypeSig param_sig ->
          let match_ctx = make_match_context ctx in
          begin match Signature_match.match_signature match_ctx loc arg_sig param_sig with
          | Ok () ->
            (* Matching succeeded - apply applicative semantics *)
            let final_mty =
              match func_path_opt, arg_path_opt with
              | Some func_path, Some arg_path ->
                (* 1. Substitute parameter path with argument path in result *)
                let param_path = Types.PathIdent parameter_id in
                let substituted_mty = Type_utils.substitute_path_in_module_type ~old_path:param_path ~new_path:arg_path result_mty in
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
              (Printf.sprintf "The functor argument does not match the expected parameter signature.\n\n%s"
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
      | Module_types.FunctorParamUnit ->
        (* Generative functor - argument must be unit (empty struct) *)
        let (typed_arg, ctx) = infer_module_expression ctx arg_expr in
        (* For generative functors, types are NOT path-dependent.
           Each application creates fresh abstract types. *)
        let typed_mexpr = {
          Typed_tree.module_desc = TypedModuleApply (typed_func, typed_arg);
          module_type = result_mty;
          module_location = loc;
        } in
        (typed_mexpr, ctx)
      end
    | _ ->
      Compiler_error.type_error loc
        "We can't apply this module because it's not a functor.\n\n\
         Only functors can be applied with `F(Arg)` syntax. This module has a regular signature."
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
      Compiler_error.type_error loc
        (Printf.sprintf "The module does not satisfy its signature constraint.\n\n%s"
           (Signature_match.format_match_error err))
    end

  | ModuleUnpack (expr, module_type_syntax) ->
    (* First-class module unpacking: (val e : MT) *)
    (* 1. Infer the expression type *)
    let typed_expr, ctx = Expression_infer.infer_expression ctx expr in

    (* 2. Check that the expression has a package type *)
    let expr_ty = Types.representative typed_expr.expression_type in
    begin match expr_ty with
    | Types.TypePackage _pkg ->
      (* 3. Check the target module type *)
      let target_mty, ctx = Module_type_check.check_module_type ctx module_type_syntax in

      (* The unpacked module has the specified module type.
         In a full implementation, we'd verify that the package's signature
         is compatible with the target type, but for now we trust the annotation. *)
      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleUnpack (typed_expr, target_mty);
        module_type = target_mty;
        module_location = loc;
      } in
      (typed_mexpr, ctx)

    | Types.TypeVariable _ ->
      (* Uninstantiated type variable - unify with a package type *)
      let target_mty, ctx = Module_type_check.check_module_type ctx module_type_syntax in
      let package_path = match module_type_syntax.Location.value with
        | ModuleTypePath path ->
          Expression_infer.module_path_to_types_path path
        | _ -> Types.PathLocal "(module)"
      in
      let expected_package_ty = Types.TypePackage {
        package_path;
        package_signature = [];
      } in
      let env = Typing_context.environment ctx in
      Inference_utils.unify_with_env env loc typed_expr.expression_type expected_package_ty;

      let typed_mexpr = {
        Typed_tree.module_desc = TypedModuleUnpack (typed_expr, target_mty);
        module_type = target_mty;
        module_location = loc;
      } in
      (typed_mexpr, ctx)

    | _ ->
      Compiler_error.type_error loc
        (Printf.sprintf "We expected a first-class module here, but found %s.\n\n\
                         The `(val e : MT)` syntax unpacks a first-class module value."
          (Type_explain.explain expr_ty))
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

  | TypedStructureRecModule rec_bindings ->
    (* Recursive modules contribute their declared signatures *)
    List.map (fun (binding : Typed_tree.typed_rec_module_binding) ->
      Module_types.SigModule (Identifier.name binding.rec_module_id, binding.rec_module_type)
    ) rec_bindings

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

  | TypedStructureTypeExtension ext ->
    (* Type extensions add constructors to the signature so they can be accessed
       via qualified names like M.Constructor *)
    List.map (fun ctor -> Module_types.SigExtensionConstructor ctor) ext.extension_constructors

  | TypedStructureExpression _ ->
    (* Top-level expressions don't contribute to the signature *)
    []

  | TypedStructureError _ ->
    (* Error items don't contribute to the signature *)
    []

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
      | Unification.Unification_error { expected; actual; location; message; trace } ->
          (* Record error and continue with current ctx - include trace in message *)
          let message_with_trace =
            if trace = [] then message
            else message ^ Unification.format_trace trace
          in
          let err_details : Inference_utils.unification_error_details = { expected; actual; location; message = message_with_trace } in
          (items, ctx, Inference_utils.UnificationError err_details :: errors)
    ) ([], ctx, []) structure
  in
  let typed_ast = if errors <> [] then None else Some (List.rev typed_items) in
  (typed_ast, ctx, List.rev errors)

(** Initialize the forward reference for module expression inference.
    This breaks the circular dependency between Expression_infer and Structure_infer. *)
let () =
  Expression_infer.infer_module_expression_ref := infer_module_expression

(** Initialize the forward reference for module expression inference in Module_type_check.
    This breaks the circular dependency needed for [module type of M]. *)
let () =
  Module_type_check.infer_module_expression_ref := infer_module_expression
