open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** Unify types with alias expansion support.
    Sets up the type lookup function from the environment before unifying. *)
let unify env loc ty1 ty2 =
  Unification.set_type_lookup (fun path -> Environment.find_type_by_path path env);
  Unification.unify loc ty1 ty2

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

let type_of_constant = function
  | ConstantInteger _ -> type_int
  | ConstantFloat _ -> type_float
  | ConstantString _ -> type_string
  | ConstantBoolean _ -> type_bool
  | ConstantUnit -> type_unit

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
    let scheme = generalize ty in
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

(** Check a type expression from a signature and convert to semantic type *)
and check_type_expression env (ty_expr : Parsing.Syntax_tree.type_expression) : type_expression =
  let loc = ty_expr.Location.location in
  match ty_expr.Location.value with
  | TypeVariable name ->
    (* For now, create a fresh type variable *)
    let _ = name in
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

let rec infer_pattern env (pattern : pattern) =
  let loc = pattern.Location.location in
  match pattern.Location.value with
  | PatternVariable name ->
    let ty = new_type_variable () in
    let id = Identifier.create name in
    let env = Environment.add_value name id (trivial_scheme ty) env in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternWildcard ->
    let ty = new_type_variable () in
    let typed_pattern = {
      pattern_desc = TypedPatternWildcard;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstant const ->
    let ty = type_of_constant const in
    let typed_pattern = {
      pattern_desc = TypedPatternConstant const;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternTuple patterns ->
    let typed_patterns, types, env =
      List.fold_left (fun (pats, tys, env) p ->
        let pat, ty, env = infer_pattern env p in
        (pat :: pats, ty :: tys, env)
      ) ([], [], env) patterns
    in
    let typed_patterns = List.rev typed_patterns in
    let types = List.rev types in
    let ty = TypeTuple types in
    let typed_pattern = {
      pattern_desc = TypedPatternTuple typed_patterns;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstructor (name, arg_pattern) ->
    begin match Environment.find_constructor name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound constructor: %s" name)
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        let fresh_params =
          List.map (fun _ -> new_type_variable ())
            constructor_info.constructor_type_parameters
        in
        let subst =
          List.combine
            (List.map (fun tv -> tv.id) constructor_info.constructor_type_parameters)
            fresh_params
        in
        let rec substitute type_to_substitute =
          match representative type_to_substitute with
          | TypeVariable type_var ->
            begin match List.assoc_opt type_var.id subst with
            | Some fresh_type -> fresh_type
            | None -> type_to_substitute
            end
          | TypeConstructor (path, type_arguments) ->
            TypeConstructor (path, List.map substitute type_arguments)
          | TypeTuple element_types ->
            TypeTuple (List.map substitute element_types)
          | TypeArrow (argument_type, result_type) ->
            TypeArrow (substitute argument_type, substitute result_type)
          | TypeRecord row ->
            TypeRecord (substitute_row row)
          | TypeRowEmpty ->
            TypeRowEmpty
        and substitute_row row = {
          row_fields = List.map (fun (field_name, field) ->
            (field_name, match field with
              | RowFieldPresent field_type -> RowFieldPresent (substitute field_type))
          ) row.row_fields;
          row_more = substitute row.row_more;
        }
        in
        (Option.map substitute constructor_info.constructor_argument_type,
         substitute constructor_info.constructor_result_type)
      in
      let typed_arg, env = match arg_pattern, expected_arg_ty with
        | None, None -> (None, env)
        | Some p, Some expected_ty ->
          let typed_p, actual_ty, env = infer_pattern env p in
          unify env loc expected_ty actual_ty;
          (Some typed_p, env)
        | Some _, None ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s does not take an argument" name)
        | None, Some _ ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s requires an argument" name)
      in
      let typed_pattern = {
        pattern_desc = TypedPatternConstructor (constructor_info, typed_arg);
        pattern_type = result_ty;
        pattern_location = loc;
      } in
      (typed_pattern, result_ty, env)
    end

  | PatternAlias (inner_pattern, name) ->
    let typed_inner, ty, env = infer_pattern env inner_pattern in
    let id = Identifier.create name in
    let env = Environment.add_value name id (trivial_scheme ty) env in
    let _ = typed_inner in
    let typed_pattern = {
      pattern_desc = TypedPatternVariable id;
      pattern_type = ty;
      pattern_location = loc;
    } in
    (typed_pattern, ty, env)

  | PatternConstraint (inner_pattern, _type_expr) ->
    infer_pattern env inner_pattern

  | PatternRecord (pattern_fields, is_open) ->
    (* Infer types for each field pattern and collect bindings *)
    let typed_field_patterns, field_types, updated_environment =
      List.fold_left (fun (typed_fields_accumulator, field_types_accumulator, current_environment) pattern_field ->
        let field_name = pattern_field.pattern_field_name.Location.value in
        let inner_pattern = match pattern_field.pattern_field_pattern with
          | Some pattern -> pattern
          | None ->
            (* Punning: { x } means { x = x } - create a variable pattern *)
            Location.{ value = PatternVariable field_name;
                       location = pattern_field.pattern_field_name.Location.location }
        in
        let typed_inner_pattern, inner_pattern_type, updated_env = infer_pattern current_environment inner_pattern in
        let typed_field = {
          Typed_tree.typed_pattern_field_name = field_name;
          typed_pattern_field_pattern = typed_inner_pattern;
        } in
        (typed_field :: typed_fields_accumulator,
         (field_name, RowFieldPresent inner_pattern_type) :: field_types_accumulator,
         updated_env)
      ) ([], [], env) pattern_fields
    in
    let typed_field_patterns = List.rev typed_field_patterns in
    let field_types = List.sort compare (List.rev field_types) in
    let row_more = if is_open then new_type_variable () else TypeRowEmpty in
    let record_type = TypeRecord {
      row_fields = field_types;
      row_more;
    } in
    let typed_pattern = {
      pattern_desc = TypedPatternRecord (typed_field_patterns, is_open);
      pattern_type = record_type;
      pattern_location = loc;
    } in
    (typed_pattern, record_type, updated_environment)

let rec infer_expression env (expr : expression) =
  let loc = expr.Location.location in
  match expr.Location.value with
  | ExpressionVariable name ->
    begin match Environment.find_value name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound variable: %s" name)
    | Some (id, scheme) ->
      let ty = instantiate scheme in
      {
        expression_desc = TypedExpressionVariable id;
        expression_type = ty;
        expression_location = loc;
      }
    end

  | ExpressionConstant const ->
    let ty = type_of_constant const in
    {
      expression_desc = TypedExpressionConstant const;
      expression_type = ty;
      expression_location = loc;
    }

  | ExpressionTuple exprs ->
    let typed_exprs = List.map (infer_expression env) exprs in
    let types = List.map (fun e -> e.expression_type) typed_exprs in
    {
      expression_desc = TypedExpressionTuple typed_exprs;
      expression_type = TypeTuple types;
      expression_location = loc;
    }

  | ExpressionConstructor (name, arg_expr) ->
    begin match Environment.find_constructor name env with
    | None ->
      Compiler_error.type_error loc
        (Printf.sprintf "Unbound constructor: %s" name)
    | Some constructor_info ->
      let expected_arg_ty, result_ty =
        let fresh_params =
          List.map (fun _ -> new_type_variable ())
            constructor_info.constructor_type_parameters
        in
        let subst =
          List.combine
            (List.map (fun tv -> tv.id) constructor_info.constructor_type_parameters)
            fresh_params
        in
        let rec substitute type_to_substitute =
          match representative type_to_substitute with
          | TypeVariable type_var ->
            begin match List.assoc_opt type_var.id subst with
            | Some fresh_type -> fresh_type
            | None -> type_to_substitute
            end
          | TypeConstructor (path, type_arguments) ->
            TypeConstructor (path, List.map substitute type_arguments)
          | TypeTuple element_types ->
            TypeTuple (List.map substitute element_types)
          | TypeArrow (argument_type, result_type) ->
            TypeArrow (substitute argument_type, substitute result_type)
          | TypeRecord row ->
            TypeRecord (substitute_row row)
          | TypeRowEmpty ->
            TypeRowEmpty
        and substitute_row row = {
          row_fields = List.map (fun (field_name, field) ->
            (field_name, match field with
              | RowFieldPresent field_type -> RowFieldPresent (substitute field_type))
          ) row.row_fields;
          row_more = substitute row.row_more;
        }
        in
        (Option.map substitute constructor_info.constructor_argument_type,
         substitute constructor_info.constructor_result_type)
      in
      let typed_arg = match arg_expr, expected_arg_ty with
        | None, None -> None
        | Some e, Some expected_ty ->
          let typed_e = infer_expression env e in
          unify env loc expected_ty typed_e.expression_type;
          Some typed_e
        | Some _, None ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s does not take an argument" name)
        | None, Some _ ->
          Compiler_error.type_error loc
            (Printf.sprintf "Constructor %s requires an argument" name)
      in
      {
        expression_desc = TypedExpressionConstructor (constructor_info, typed_arg);
        expression_type = result_ty;
        expression_location = loc;
      }
    end

  | ExpressionApply (func_expr, arg_exprs) ->
    let typed_func = infer_expression env func_expr in
    let typed_args = List.map (infer_expression env) arg_exprs in
    let result_ty = new_type_variable () in
    let expected_func_ty =
      List.fold_right (fun arg acc ->
        TypeArrow (arg.expression_type, acc)
      ) typed_args result_ty
    in
    unify env loc expected_func_ty typed_func.expression_type;
    {
      expression_desc = TypedExpressionApply (typed_func, typed_args);
      expression_type = result_ty;
      expression_location = loc;
    }

  | ExpressionFunction (param_patterns, body_expr) ->
    enter_level ();
    let typed_params, param_types, inner_env =
      List.fold_left (fun (pats, tys, env) p ->
        let pat, ty, env = infer_pattern env p in
        (pat :: pats, ty :: tys, env)
      ) ([], [], env) param_patterns
    in
    let typed_params = List.rev typed_params in
    let param_types = List.rev param_types in
    let typed_body = infer_expression inner_env body_expr in
    leave_level ();
    let func_ty =
      List.fold_right (fun param_ty acc ->
        TypeArrow (param_ty, acc)
      ) param_types typed_body.expression_type
    in
    {
      expression_desc = TypedExpressionFunction (typed_params, typed_body);
      expression_type = func_ty;
      expression_location = loc;
    }

  | ExpressionLet (rec_flag, bindings, body_expr) ->
    let typed_bindings, inner_env = infer_bindings env rec_flag bindings in
    let typed_body = infer_expression inner_env body_expr in
    {
      expression_desc = TypedExpressionLet (rec_flag, typed_bindings, typed_body);
      expression_type = typed_body.expression_type;
      expression_location = loc;
    }

  | ExpressionIf (cond_expr, then_expr, else_expr_opt) ->
    let typed_cond = infer_expression env cond_expr in
    unify env loc type_bool typed_cond.expression_type;
    let typed_then = infer_expression env then_expr in
    let typed_else = match else_expr_opt with
      | Some else_expr ->
        let typed_else = infer_expression env else_expr in
        unify env loc typed_then.expression_type typed_else.expression_type;
        Some typed_else
      | None ->
        unify env loc type_unit typed_then.expression_type;
        None
    in
    {
      expression_desc = TypedExpressionIf (typed_cond, typed_then, typed_else);
      expression_type = typed_then.expression_type;
      expression_location = loc;
    }

  | ExpressionSequence (first_expr, second_expr) ->
    let typed_first = infer_expression env first_expr in
    let typed_second = infer_expression env second_expr in
    {
      expression_desc = TypedExpressionSequence (typed_first, typed_second);
      expression_type = typed_second.expression_type;
      expression_location = loc;
    }

  | ExpressionConstraint (inner_expr, _type_expr) ->
    infer_expression env inner_expr

  | ExpressionRecord record_fields ->
    (* Infer types for each field value *)
    let typed_record_fields = List.map (fun record_field ->
      let field_name = record_field.field_name.Location.value in
      let typed_field_value = infer_expression env record_field.field_value in
      {
        Typed_tree.typed_field_name = field_name;
        typed_field_value;
      }
    ) record_fields in
    (* Build row type from inferred field types *)
    let row_field_types = List.map (fun typed_field ->
      (typed_field.Typed_tree.typed_field_name,
       RowFieldPresent typed_field.typed_field_value.expression_type)
    ) typed_record_fields in
    let record_type = TypeRecord {
      row_fields = List.sort compare row_field_types;
      row_more = TypeRowEmpty;  (* Record literals are closed *)
    } in
    {
      expression_desc = TypedExpressionRecord typed_record_fields;
      expression_type = record_type;
      expression_location = loc;
    }

  | ExpressionRecordAccess (record_expression, field_name) ->
    (* Try to extract a module path from nested record accesses *)
    let rec extract_module_path expr =
      match expr.Location.value with
      | ExpressionConstructor (name, None) ->
        (* Check if this is a module *)
        begin match Environment.find_module name env with
        | Some _ -> Some [name]
        | None -> None
        end
      | ExpressionRecordAccess (inner, component) ->
        (* Recursively check if inner is a module path *)
        begin match extract_module_path inner with
        | Some path -> Some (path @ [component])
        | None -> None
        end
      | _ -> None
    in
    (* Check if record_expression is a module path *)
    begin match extract_module_path record_expression with
    | Some path_components ->
      (* This is module access: look up field in the module at path *)
      let (base_binding, mod_binding) = lookup_module_path env path_components loc in
      (* Build internal path from the ROOT module's name *)
      let internal_path = module_path_to_internal_path base_binding.mod_id path_components in
      begin match mod_binding.Module_types.mod_type with
      | Module_types.ModTypeSig sig_ ->
        (* First check if field_name is a submodule *)
        begin match Module_types.find_module_in_sig field_name sig_ with
        | Some _submod_type ->
          (* field_name is a submodule - return a module access to it *)
          let extended_path = Types.PathDot (internal_path, field_name) in
          (* Return a module access expression *)
          (* The type will be the submodule type, but we represent it as unit for now *)
          (* since we don't have proper module values at the expression level *)
          {
            expression_desc = TypedExpressionModuleAccess (extended_path, field_name);
            expression_type = TypeConstructor (PathBuiltin BuiltinUnit, []);
            expression_location = loc;
          }
        | None ->
          (* Check if field_name is a value *)
          begin match Module_types.find_value_in_sig field_name sig_ with
          | Some val_desc ->
            let ty = Types.instantiate val_desc.val_type in
            {
              expression_desc = TypedExpressionModuleAccess (internal_path, field_name);
              expression_type = ty;
              expression_location = loc;
            }
          | None ->
            let path_str = String.concat "." path_components in
            Compiler_error.type_error loc
              (Printf.sprintf "Value %s not found in module %s" field_name path_str)
          end
        end
      | Module_types.ModTypeFunctor _ ->
        Compiler_error.type_error loc "Cannot access value in a functor"
      | Module_types.ModTypeIdent _ ->
        Compiler_error.type_error loc "Cannot access value in an abstract module type"
      end
    | None ->
      (* Normal record access *)
      let typed_record_expression = infer_expression env record_expression in
      let field_type = new_type_variable () in
      let row_tail = new_type_variable () in
      let expected_record_type = TypeRecord {
        row_fields = [(field_name, RowFieldPresent field_type)];
        row_more = row_tail;
      } in
      unify env loc expected_record_type typed_record_expression.expression_type;
      {
        expression_desc = TypedExpressionRecordAccess (typed_record_expression, field_name);
        expression_type = field_type;
        expression_location = loc;
      }
    end

  | ExpressionRecordUpdate (base_expression, update_fields) ->
    (* Infer the base record expression type *)
    let typed_base_expression = infer_expression env base_expression in
    (* Infer types for each update field *)
    let typed_update_fields = List.map (fun update_field ->
      let field_name = update_field.field_name.Location.value in
      let typed_field_value = infer_expression env update_field.field_value in
      {
        Typed_tree.typed_field_name = field_name;
        typed_field_value;
      }
    ) update_fields in
    (* Build expected row type from update fields *)
    let update_field_types = List.map (fun typed_field ->
      (typed_field.Typed_tree.typed_field_name,
       RowFieldPresent typed_field.typed_field_value.expression_type)
    ) typed_update_fields in
    let row_tail = new_type_variable () in
    let expected_base_type = TypeRecord {
      row_fields = List.sort compare update_field_types;
      row_more = row_tail;
    } in
    unify env loc expected_base_type typed_base_expression.expression_type;
    (* Result type is same as base type *)
    {
      expression_desc = TypedExpressionRecordUpdate (typed_base_expression, typed_update_fields);
      expression_type = typed_base_expression.expression_type;
      expression_location = loc;
    }

  | ExpressionMatch (scrutinee_expression, match_arms) ->
    (* Infer type of the scrutinee *)
    let typed_scrutinee = infer_expression env scrutinee_expression in
    let scrutinee_type = typed_scrutinee.expression_type in
    (* Create fresh type variable for the result *)
    let result_type = new_type_variable () in
    (* Type-check each match arm *)
    let typed_match_arms = List.map (fun match_arm ->
      (* Infer pattern type and get bindings *)
      let typed_pattern, pattern_type, arm_environment = infer_pattern env match_arm.arm_pattern in
      (* Unify pattern type with scrutinee type *)
      unify env match_arm.arm_location scrutinee_type pattern_type;
      (* Type-check guard if present - must be bool *)
      let typed_guard = match match_arm.arm_guard with
        | None -> None
        | Some guard_expression ->
          let typed_guard_expression = infer_expression arm_environment guard_expression in
          unify env guard_expression.Location.location type_bool typed_guard_expression.expression_type;
          Some typed_guard_expression
      in
      (* Type-check arm body with pattern bindings *)
      let typed_arm_expression = infer_expression arm_environment match_arm.arm_expression in
      (* Unify arm result with overall result type *)
      unify env match_arm.arm_location result_type typed_arm_expression.expression_type;
      {
        Typed_tree.typed_arm_pattern = typed_pattern;
        typed_arm_guard = typed_guard;
        typed_arm_expression;
        typed_arm_location = match_arm.arm_location;
      }
    ) match_arms in
    (* Check exhaustiveness and redundancy *)
    Pattern_check.check_match env loc scrutinee_type typed_match_arms;
    {
      expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
      expression_type = result_type;
      expression_location = loc;
    }

  | ExpressionModuleAccess (module_path, value_name) ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = lookup_module_path env path_modules loc in
    begin match mod_binding.Module_types.mod_type with
    | Module_types.ModTypeSig sig_ ->
      begin match Module_types.find_value_in_sig value_name sig_ with
      | Some val_desc ->
        let ty = Types.instantiate val_desc.val_type in
        let internal_path = module_path_to_internal_path base_binding.mod_id path_modules in
        {
          expression_desc = TypedExpressionModuleAccess (internal_path, value_name);
          expression_type = ty;
          expression_location = loc;
        }
      | None ->
        Compiler_error.type_error loc
          (Printf.sprintf "Value %s not found in module" value_name)
      end
    | Module_types.ModTypeFunctor _ ->
      Compiler_error.type_error loc "Cannot access value in a functor"
    | Module_types.ModTypeIdent _ ->
      Compiler_error.type_error loc "Cannot access value in an abstract module type"
    end

and infer_bindings env rec_flag bindings =
  match rec_flag with
  | Nonrecursive ->
    enter_level ();
    let typed_bindings, env =
      List.fold_left (fun (bs, env) (binding : binding) ->
        let typed_expr = infer_expression env binding.binding_expression in
        leave_level ();
        let scheme = generalize typed_expr.expression_type in
        enter_level ();
        let typed_pat, pat_ty, env = infer_pattern env binding.binding_pattern in
        unify env binding.binding_location pat_ty typed_expr.expression_type;
        let env = match binding.binding_pattern.Location.value, typed_pat.pattern_desc with
          | PatternVariable name, TypedPatternVariable id ->
            Environment.add_value name id scheme env
          | _ -> env
        in
        let typed_binding = {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        } in
        (typed_binding :: bs, env)
      ) ([], env) bindings
    in
    leave_level ();
    (List.rev typed_bindings, env)

  | Recursive ->
    enter_level ();
    let env, temp_info =
      List.fold_left (fun (env, temps) (binding : binding) ->
        match binding.binding_pattern.Location.value with
        | PatternVariable name ->
          let ty = new_type_variable () in
          let id = Identifier.create name in
          let env = Environment.add_value name id (trivial_scheme ty) env in
          (env, (name, id, ty) :: temps)
        | _ ->
          Compiler_error.type_error binding.binding_location
            "Recursive bindings must be simple variables"
      ) (env, []) bindings
    in
    let typed_bindings =
      List.map2 (fun (binding : binding) (_name, id, expected_ty) ->
        let typed_expr = infer_expression env binding.binding_expression in
        unify env binding.binding_location expected_ty typed_expr.expression_type;
        let typed_pat = {
          pattern_desc = TypedPatternVariable id;
          pattern_type = expected_ty;
          pattern_location = binding.binding_pattern.Location.location;
        } in
        {
          Typed_tree.binding_pattern = typed_pat;
          binding_expression = typed_expr;
          binding_location = binding.binding_location;
        }
      ) bindings (List.rev temp_info)
    in
    leave_level ();
    let env =
      List.fold_left2 (fun env (_binding : binding) (name, id, ty) ->
        let scheme = generalize ty in
        Environment.add_value name id scheme env
      ) env bindings (List.rev temp_info)
    in
    (typed_bindings, env)

let process_type_declaration env (type_decl : Parsing.Syntax_tree.type_declaration) =
  let type_params =
    List.map (fun _ ->
      match new_type_variable () with
      | TypeVariable tv -> tv
      | _ -> assert false
    ) type_decl.type_parameters
  in
  let result_type =
    TypeConstructor (PathLocal type_decl.type_name.Location.value,
      List.map (fun tv -> TypeVariable tv) type_params)
  in
  let (declaration_kind, manifest) = match type_decl.type_kind with
    | TypeAbstract -> (DeclarationAbstract, None)
    | TypeVariant constructors ->
      let constructor_infos =
        List.mapi (fun tag_index (ctor : constructor_declaration) ->
          let arg_type = Option.map (fun _ -> new_type_variable ()) ctor.constructor_argument in
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
      (* Convert syntax type expression to semantic type *)
      let manifest_type = check_type_expression env ty_expr in
      (DeclarationAbstract, Some manifest_type)
  in
  let type_declaration = {
    declaration_name = type_decl.type_name.Location.value;
    declaration_parameters = type_params;
    declaration_manifest = manifest;
    declaration_kind;
  } in
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

let rec infer_structure_item env (item : structure_item) =
  match item.Location.value with
  | StructureValue (rec_flag, bindings) ->
    let typed_bindings, env = infer_bindings env rec_flag bindings in
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
        let spec_mty = check_module_type env constraint_mty in
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
    let resolved_mty = check_module_type env mty in
    (* Add the module type to the environment *)
    let env = Environment.add_module_type name (Some resolved_mty) env in
    let typed_item = {
      structure_item_desc = TypedStructureModuleType (name, resolved_mty);
      structure_item_location = item.Location.location;
    } in
    (typed_item, env)

  | StructureOpen module_path ->
    let path_modules = module_path.Location.value in
    let (base_binding, mod_binding) = lookup_module_path env path_modules item.Location.location in
    begin match mod_binding.Module_types.mod_type with
    | Module_types.ModTypeSig sig_ ->
      let (env, opened_bindings) = Environment.open_module sig_ env in
      let path = module_path_to_internal_path base_binding.mod_id path_modules in
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

(** Infer the type of a module expression *)
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
    let (base_binding, mod_binding) = lookup_module_path env path_modules loc in
    let internal_path = module_path_to_internal_path base_binding.mod_id path_modules in
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
      let param_mty = check_module_type env param.functor_param_type in
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
    let spec_mty = check_module_type env constraint_mty in
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

and infer_structure env structure =
  let typed_items, env =
    List.fold_left (fun (items, env) item ->
      let typed_item, env = infer_structure_item env item in
      (typed_item :: items, env)
    ) ([], env) structure
  in
  (List.rev typed_items, env)
