(** Module type checking.

    Handles conversion of syntactic module types to semantic module types,
    including signature checking and with-constraints.

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable]. *)

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

(** Check a type expression with a mapping from type variable names to semantic variables.
    This preserves sharing: multiple occurrences of ['a] map to the same variable.

    @param ctx The typing context
    @param var_map Mapping from type parameter names to their semantic type variables
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
let rec check_type_expression_impl ctx (var_map : (string * type_variable) list)
    (ty_expr : Parsing.Syntax_tree.type_expression) : type_expression * Typing_context.t =
  let env = Typing_context.environment ctx in
  let loc = ty_expr.Location.location in
  match ty_expr.Location.value with
  | TypeVariable name ->
    (* Look up in the variable mapping first *)
    begin match List.assoc_opt name var_map with
    | Some tv -> (TypeVariable tv, ctx)
    | None ->
      (* Not a declared parameter - create fresh variable *)
      Typing_context.new_type_variable ctx
    end

  | TypeConstructor (name, args) ->
    let arg_types, ctx =
      List.fold_left (fun (types, ctx) arg ->
        let ty, ctx = check_type_expression_impl ctx var_map arg in
        (types @ [ty], ctx)
      ) ([], ctx) args
    in
    let result = begin match name with
    | "int" -> type_int
    | "float" -> type_float
    | "string" -> type_string
    | "bool" -> type_bool
    | "unit" -> type_unit
    | _ ->
      (* Look up the type in environment *)
      begin match Environment.find_type name env with
      | Some decl ->
        TypeConstructor (PathLocal decl.declaration_name, arg_types)
      | None ->
        Inference_utils.error_unbound_type loc name
      end
    end in
    (result, ctx)

  | TypeArrow (arg_ty, ret_ty) ->
    let arg, ctx = check_type_expression_impl ctx var_map arg_ty in
    let ret, ctx = check_type_expression_impl ctx var_map ret_ty in
    (TypeArrow (arg, ret), ctx)

  | TypeTuple tys ->
    let types, ctx =
      List.fold_left (fun (types, ctx) ty ->
        let ty, ctx = check_type_expression_impl ctx var_map ty in
        (types @ [ty], ctx)
      ) ([], ctx) tys
    in
    (TypeTuple types, ctx)

  | TypeRecord (fields, is_open) ->
    let row_fields, ctx =
      List.fold_left (fun (fields, ctx) field ->
        let ty, ctx = check_type_expression_impl ctx var_map field.type_field_type in
        (fields @ [(field.type_field_name, RowFieldPresent ty)], ctx)
      ) ([], ctx) fields
    in
    let row_more, ctx =
      if is_open then Typing_context.new_type_variable ctx
      else (TypeRowEmpty, ctx)
    in
    (TypeRecord { row_fields; row_more }, ctx)

(** Check a type expression from a signature and convert to semantic type.
    Note: This does NOT preserve type variable sharing - use check_type_expression_with_params
    for type declarations with parameters.

    @param ctx The typing context
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
let check_type_expression ctx ty_expr =
  check_type_expression_impl ctx [] ty_expr

(** Check a type expression with declared type parameters.
    The param_names and param_vars must have the same length.

    @param ctx The typing context
    @param param_names List of type parameter names (e.g., ["'a"; "'b"])
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
let check_type_expression_with_params ctx param_names param_vars ty_expr =
  let var_map = List.combine param_names param_vars in
  check_type_expression_impl ctx var_map ty_expr

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
        let manifest_type, ctx' = check_type_expression !ctx_ref type_expr in
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
    let ty, ctx = check_type_expression ctx type_expr in
    (* Use base level for signature type generalization *)
    let scheme = Type_scheme.generalize ~level:1 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name_str, val_desc)), ctx)

  | SignatureType type_decls ->
    (* For now, just record the type name - full type declaration processing
       would require access to process_type_declaration which is defined later *)
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
            (* new_type_variable always returns a TypeVariable *)
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
      } in
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
    let ty, ctx = check_type_expression ctx ext_decl.external_type in
    (* Use base level for signature type generalization *)
    let scheme = Type_scheme.generalize ~level:1 ty in
    let val_desc = Module_types.{
      value_type = scheme;
      value_location = loc;
    } in
    (Some (Module_types.SigValue (name, val_desc)), ctx)
