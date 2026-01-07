(** Module type checking.

    Handles conversion of syntactic module types to semantic module types,
    including signature checking and with-constraints. *)

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
    | None -> Compiler_error.type_error loc (Printf.sprintf "Unbound module: %s" name)
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
            Module_types.{ mod_name = name; mod_id = id; mod_type = mty; mod_alias = None }
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
      let final_binding = follow_path base_binding.mod_type rest in
      (base_binding, final_binding)
    | None -> Compiler_error.type_error loc (Printf.sprintf "Unbound module: %s" first)
    end

(** Check a type expression from a signature and convert to semantic type *)
let rec check_type_expression env (ty_expr : Parsing.Syntax_tree.type_expression) : type_expression =
  let loc = ty_expr.Location.location in
  match ty_expr.Location.value with
  | TypeVariable _name ->
    (* TODO: Track type variable names for consistent instantiation within signatures.
       Currently, each type variable in a signature gets a fresh unification variable,
       so `'a -> 'a` becomes `'x -> 'y` instead of preserving the sharing. *)
    new_type_variable ()

  | TypeConstructor (name, args) ->
    let arg_types = List.map (check_type_expression env) args in
    begin match name with
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
        Compiler_error.type_error loc (Printf.sprintf "Unbound type: %s" name)
      end
    end

  | TypeArrow (arg_ty, ret_ty) ->
    let arg = check_type_expression env arg_ty in
    let ret = check_type_expression env ret_ty in
    TypeArrow (arg, ret)

  | TypeTuple tys ->
    let types = List.map (check_type_expression env) tys in
    TypeTuple types

  | TypeRecord (fields, is_open) ->
    let row_fields = List.map (fun field ->
      (field.type_field_name, RowFieldPresent (check_type_expression env field.type_field_type))
    ) fields in
    let row_more = if is_open then new_type_variable () else TypeRowEmpty in
    TypeRecord { row_fields; row_more }

(** Check a syntax module type and convert to semantic module type *)
let rec check_module_type env (mty : module_type) : Module_types.module_type =
  let loc = mty.Location.location in
  match mty.Location.value with
  | ModuleTypePath module_path ->
    (* Module type path like S or M.S *)
    let path_modules = module_path.Location.value in
    begin match path_modules with
    | [name] ->
      (* Simple module type name *)
      begin match Environment.find_module_type name env with
      | Some (Some resolved_mty) -> resolved_mty
      | Some None ->
        Compiler_error.type_error loc (Printf.sprintf "Module type %s is abstract" name)
      | None ->
        Compiler_error.type_error loc (Printf.sprintf "Unbound module type: %s" name)
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
        begin match final_binding.mod_type with
        | Module_types.ModTypeSig sig_ ->
          begin match Module_types.find_module_type_in_sig type_name sig_ with
          | Some (Some resolved) -> resolved
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
    let semantic_sig = check_signature env sig_items in
    Module_types.ModTypeSig semantic_sig

  | ModuleTypeFunctor (params, result) ->
    (* Functor type: functor (X : S) -> MT *)
    begin match params with
    | [] ->
      Compiler_error.type_error loc "Functor type must have at least one parameter"
    | [param] ->
      let param_name = param.functor_param_name.Location.value in
      let param_mty = check_module_type env param.functor_param_type in
      let param_id = Identifier.create param_name in
      let param_binding = Module_types.{
        mod_name = param_name;
        mod_id = param_id;
        mod_type = param_mty;
        mod_alias = None;
      } in
      (* Add parameter to environment for checking result type *)
      let result_env = Environment.add_module param_name param_binding env in
      let result_mty = check_module_type result_env result in
      let semantic_param = Module_types.{
        param_name;
        param_id;
        param_type = param_mty;
      } in
      Module_types.ModTypeFunctor (semantic_param, result_mty)
    | param :: rest ->
      (* Multiple parameters - desugar to nested functor type *)
      let inner_type = {
        Location.value = ModuleTypeFunctor (rest, result);
        location = loc;
      } in
      let single_param_type = {
        Location.value = ModuleTypeFunctor ([param], inner_type);
        location = loc;
      } in
      check_module_type env single_param_type
    end

  | ModuleTypeWith (base_mty, constraints) ->
    (* Check the base module type *)
    let base = check_module_type env base_mty in
    (* Apply each constraint *)
    List.fold_left (fun mty constraint_ ->
      apply_with_constraint env mty constraint_ loc
    ) base constraints

(** Apply a single with-constraint to a module type *)
and apply_with_constraint env mty (constraint_ : Parsing.Syntax_tree.with_constraint) loc =
  match constraint_ with
  | WithType (path, params, type_expr) ->
    apply_type_constraint env mty path params type_expr loc
  | WithModule (path, target) ->
    apply_module_constraint env mty path target loc

(** Apply a module constraint: with module M = N *)
and apply_module_constraint env mty path target loc =
  (* Look up the target module in the environment *)
  let target_modules = target.Location.value in
  let (_, target_binding) = lookup_module_path env target_modules loc in
  let target_mty = target_binding.Module_types.mod_type in
  (* Apply the constraint to the signature *)
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig = refine_module_in_sig env sig_ path target_mty loc in
    Module_types.ModTypeSig refined_sig
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to functor type"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to abstract module type"

(** Refine a module in a signature by path *)
and refine_module_in_sig _env sig_ (path : Parsing.Syntax_tree.longident) target_mty loc =
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
    refine_nested_module_in_sig sig_ prefix name target_mty loc

(** Refine a nested module in a signature *)
and refine_nested_module_in_sig sig_ (prefix : Parsing.Syntax_tree.longident) mod_name target_mty loc =
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
          let refined_inner_sig = refine_module_in_sig Environment.empty inner_sig inner_path target_mty loc in
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
          let refined_inner_sig = refine_module_in_sig Environment.empty inner_sig remaining_path target_mty loc in
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
and apply_type_constraint env mty path params type_expr loc =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let refined_sig = refine_type_in_sig env sig_ path params type_expr loc in
    Module_types.ModTypeSig refined_sig
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to functor type"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot apply with-constraint to abstract module type"

(** Refine a type in a signature by path *)
and refine_type_in_sig env sig_ (path : Parsing.Syntax_tree.longident) params type_expr loc =
  match path.Location.value with
  | Lident name ->
    (* Direct type: with type t = ... *)
    let found = ref false in
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
        let manifest_type = check_type_expression env type_expr in
        let refined_decl = { decl with declaration_manifest = Some manifest_type } in
        Module_types.SigType (n, refined_decl)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Type %s not found in signature" name);
    refined_sig
  | Ldot (prefix, name) ->
    (* Nested: with type M.t = ... - find module M, then refine type t inside *)
    refine_type_in_nested_sig env sig_ prefix name params type_expr loc

(** Refine a type in a nested module signature *)
and refine_type_in_nested_sig env sig_ (prefix : Parsing.Syntax_tree.longident) type_name params type_expr loc =
  (* Get the module name from the prefix *)
  match prefix.Location.value with
  | Lident mod_name ->
    (* Simple module prefix: with type M.t = ... *)
    let found = ref false in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let type_path = { Location.value = Parsing.Syntax_tree.Lident type_name; location = loc } in
        let refined_inner = apply_type_constraint env inner_mty type_path params type_expr loc in
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    refined_sig
  | Ldot (deeper_prefix, mod_name) ->
    (* Deeper nesting: with type M.N.t = ... - recursively find M, then N.t *)
    let found = ref false in
    let refined_sig = List.map (fun item ->
      match item with
      | Module_types.SigModule (n, inner_mty) when n = mod_name ->
        found := true;
        let remaining_path = { Location.value = Parsing.Syntax_tree.Ldot (deeper_prefix, type_name); location = loc } in
        let refined_inner = apply_type_constraint env inner_mty remaining_path params type_expr loc in
        Module_types.SigModule (n, refined_inner)
      | _ -> item
    ) sig_ in
    if not !found then
      Compiler_error.type_error loc (Printf.sprintf "Module %s not found in signature" mod_name);
    refined_sig

(** Check a syntax signature and convert to semantic signature *)
and check_signature env (sig_items : signature) : Module_types.signature =
  List.filter_map (fun item -> check_signature_item env item) sig_items

(** Check a signature item *)
and check_signature_item env (item : signature_item) : Module_types.signature_item option =
  let loc = item.Location.location in
  match item.Location.value with
  | SignatureValue (name, type_expr) ->
    let name_str = name.Location.value in
    (* Convert syntax type to semantic type *)
    let ty = check_type_expression env type_expr in
    let scheme = Types.generalize ty in
    let val_desc = Module_types.{
      val_type = scheme;
      val_location = loc;
    } in
    Some (Module_types.SigValue (name_str, val_desc))

  | SignatureType type_decls ->
    (* For now, just record the type name - full type declaration processing
       would require access to process_type_declaration which is defined later *)
    begin match type_decls with
    | decl :: _ ->
      let name = decl.type_name.Location.value in
      (* Create fresh type variables for each parameter *)
      let type_params =
        List.map (fun _ ->
          match Types.new_type_variable () with
          | Types.TypeVariable tv -> tv
          | _ -> assert false
        ) decl.type_parameters
      in
      (* Determine the kind based on the syntax *)
      let kind = match decl.type_kind with
        | Parsing.Syntax_tree.TypeAbstract -> Types.DeclarationAbstract
        | Parsing.Syntax_tree.TypeVariant _ -> Types.DeclarationAbstract (* Simplified for signatures *)
        | Parsing.Syntax_tree.TypeAlias _ -> Types.DeclarationAbstract (* Aliases are abstract in signatures *)
      in
      (* Create a type declaration for the signature *)
      let sig_decl = Types.{
        declaration_name = name;
        declaration_parameters = type_params;
        declaration_manifest = None;
        declaration_kind = kind;
      } in
      Some (Module_types.SigType (name, sig_decl))
    | [] -> None
    end

  | SignatureModule (name, mty) ->
    let name_str = name.Location.value in
    let semantic_mty = check_module_type env mty in
    Some (Module_types.SigModule (name_str, semantic_mty))

  | SignatureModuleType (name, mty_opt) ->
    let name_str = name.Location.value in
    let semantic_mty_opt = Option.map (check_module_type env) mty_opt in
    Some (Module_types.SigModuleType (name_str, semantic_mty_opt))

  | SignatureOpen _path ->
    (* Open in signature - skip for now *)
    None

  | SignatureInclude _mty ->
    (* Include in signature - skip for now *)
    None

  | SignatureExternal ext_decl ->
    (* External declarations in signatures contribute value bindings *)
    let name = ext_decl.external_name.Location.value in
    let ty = check_type_expression env ext_decl.external_type in
    (* For signatures, we don't validate FFI attributes - just record the type *)
    let scheme = Types.generalize ty in
    let val_desc = Module_types.{
      val_type = scheme;
      val_location = loc;
    } in
    Some (Module_types.SigValue (name, val_desc))
