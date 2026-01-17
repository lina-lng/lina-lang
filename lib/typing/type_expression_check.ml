(** Type expression checking.

    Converts syntactic type expressions to semantic types with proper
    type variable sharing for type parameters.

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable]. *)

open Common
open Parsing.Syntax_tree
open Types

(** Check a type expression with a mapping from type variable names to semantic variables.
    This preserves sharing: multiple occurrences of ['a] map to the same variable.
    The [accumulate_vars] parameter controls whether fresh variables should be added
    to the var_map (used for GADT constructor return types where the same variable
    name should map to the same variable throughout the type).

    @param ctx The typing context
    @param var_map Mapping from type parameter names to their semantic type variables
    @param accumulate_vars If true, fresh variables are added to var_map for reuse
    @param ty_expr The syntactic type expression
    @return A triple [(type_expr, updated_ctx, updated_var_map)] *)
let rec check_type_expression_impl ctx (var_map : (string * type_variable) list)
    ~(accumulate_vars : bool)
    (ty_expr : Parsing.Syntax_tree.type_expression) : type_expression * Typing_context.t * (string * type_variable) list =
  let env = Typing_context.environment ctx in
  let loc = ty_expr.Location.location in
  match ty_expr.Location.value with
  | TypeVariable name ->
    (* Look up in the variable mapping first *)
    begin match List.assoc_opt name var_map with
    | Some tv -> (TypeVariable tv, ctx, var_map)
    | None ->
      (* Not a declared parameter - create fresh variable *)
      let ty, ctx = Typing_context.new_type_variable ctx in
      let new_var_map =
        if accumulate_vars then
          match ty with
          | TypeVariable tv -> (name, tv) :: var_map
          | _ -> var_map
        else var_map
      in
      (ty, ctx, new_var_map)
    end

  | TypeConstructor (longident, args) ->
    (* Process type arguments first, threading var_map through.
       Note: We accumulate in reverse order using cons for O(n) complexity. *)
    let rev_arg_types, ctx, var_map =
      List.fold_left (fun (rev_types, ctx, var_map) arg ->
        let ty, ctx, var_map = check_type_expression_impl ctx var_map ~accumulate_vars arg in
        (ty :: rev_types, ctx, var_map)
      ) ([], ctx, var_map) args
    in
    let arg_types = List.rev rev_arg_types in

    (* Helper to look up a local type by name (handles builtins, var_map, and environment) *)
    let lookup_local_type name =
      (* First check if this is a nullary reference to a forall-bound variable *)
      match arg_types, List.assoc_opt name var_map with
      | [], Some tv -> Some (TypeVariable tv)
      | _ ->
        (* Check for builtin types *)
        match name with
        | "int" -> Some type_int
        | "float" -> Some type_float
        | "string" -> Some type_string
        | "bool" -> Some type_bool
        | "unit" -> Some type_unit
        | "ref" ->
          begin match arg_types with
          | [content_ty] -> Some (type_ref content_ty)
          | _ -> Compiler_error.type_error loc
                   (Printf.sprintf "Type constructor ref expects 1 argument, got %d"
                      (List.length arg_types))
          end
        | _ ->
          (* Look up the type in environment *)
          begin match Environment.find_type name env with
          | Some decl ->
            (* Apply type parameter constraints if present *)
            if decl.declaration_constraints <> [] && arg_types <> [] then begin
              List.iter (fun (constraint_ : Types.type_constraint) ->
                let var_type = Type_utils.substitute_type_params
                  decl.declaration_parameters arg_types
                  (TypeVariable constraint_.constraint_variable) in
                let constraint_type = Type_utils.substitute_type_params
                  decl.declaration_parameters arg_types
                  constraint_.constraint_type in
                let fresh_var () = Types.new_type_variable_at_level 0 in
                let constraint_type = Type_scheme.instantiate_all_fresh ~fresh_var constraint_type in
                Inference_utils.unify_with_env env loc var_type constraint_type
              ) decl.declaration_constraints
            end;
            begin match decl.declaration_manifest with
            | Some manifest when arg_types = [] -> Some manifest
            | Some manifest ->
                Some (Type_utils.substitute_type_params decl.declaration_parameters arg_types manifest)
            | None ->
                Some (TypeConstructor (PathLocal decl.declaration_name, arg_types))
            end
          | None -> None
          end
    in

    (* Convert longident to path and look up type *)
    let rec longident_to_path_and_name (lid : longident_desc) : (string list * string) =
      match lid with
      | Lident name -> ([], name)
      | Ldot (prefix, name) ->
        let module_parts, _ = longident_to_path (prefix.Location.value) in
        (module_parts, name)
    and longident_to_path (lid : longident_desc) : (string list * string) =
      match lid with
      | Lident name -> ([name], name)
      | Ldot (prefix, name) ->
        let parts, _ = longident_to_path (prefix.Location.value) in
        (parts @ [name], name)
    in

    (* Helper to look up a module by path for qualified type access *)
    let lookup_module_for_type path_modules =
      match path_modules with
      | [] -> Compiler_error.type_error loc "Empty module path"
      | [name] ->
        begin match Environment.find_module name env with
        | Some binding -> binding
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
          follow_path base_binding.binding_type rest
        | None -> Inference_utils.error_unbound_module loc first
        end
    in

    let result =
      match longident.Location.value with
      | Lident name ->
        begin match lookup_local_type name with
        | Some ty -> ty
        | None -> Inference_utils.error_unbound_type loc name
        end
      | Ldot _ ->
        (* Qualified path like M.t or M.N.t *)
        let module_parts, type_name = longident_to_path_and_name longident.Location.value in
        begin match module_parts with
        | [] ->
          (* Should not happen, but fall back to local lookup *)
          begin match lookup_local_type type_name with
          | Some ty -> ty
          | None -> Inference_utils.error_unbound_type loc type_name
          end
        | _ ->
          (* Look up the module path *)
          let final_binding = lookup_module_for_type module_parts in
          let module_type = final_binding.Module_types.binding_type in
          (* Find type in module's signature *)
          begin match module_type with
          | Module_types.ModTypeSig sig_ ->
            begin match Module_types.find_type_in_sig type_name sig_ with
            | Some decl ->
              (* Build the path to this type *)
              let base_path = Types.PathIdent final_binding.Module_types.binding_id in
              let type_path = Types.PathDot (base_path, type_name) in
              begin match decl.declaration_manifest with
              | Some manifest when arg_types = [] -> manifest
              | Some manifest ->
                  Type_utils.substitute_type_params decl.declaration_parameters arg_types manifest
              | None ->
                  TypeConstructor (type_path, arg_types)
              end
            | None ->
              Compiler_error.type_error loc
                (Printf.sprintf "Unbound type %s in module" type_name)
            end
          | Module_types.ModTypeFunctor _ ->
            Compiler_error.type_error loc
              "Cannot access types in functor"
          | Module_types.ModTypeIdent _ ->
            Compiler_error.type_error loc
              "Cannot access types through abstract module type"
          end
        end
    in
    (result, ctx, var_map)

  | TypeArrow (label, arg_ty, ret_ty) ->
    let arg, ctx, var_map = check_type_expression_impl ctx var_map ~accumulate_vars arg_ty in
    let ret, ctx, var_map = check_type_expression_impl ctx var_map ~accumulate_vars ret_ty in
    let types_label = Inference_utils.convert_syntax_label label in
    (Types.TypeArrow (types_label, arg, ret), ctx, var_map)

  | TypeTuple tys ->
    (* Note: We accumulate in reverse order using cons for O(n) complexity. *)
    let rev_types, ctx, var_map =
      List.fold_left (fun (rev_types, ctx, var_map) ty ->
        let ty, ctx, var_map = check_type_expression_impl ctx var_map ~accumulate_vars ty in
        (ty :: rev_types, ctx, var_map)
      ) ([], ctx, var_map) tys
    in
    (TypeTuple (List.rev rev_types), ctx, var_map)

  | TypeRecord (fields, is_open) ->
    (* Note: We accumulate in reverse order using cons for O(n) complexity. *)
    let rev_row_fields, ctx, var_map =
      List.fold_left (fun (rev_fields, ctx, var_map) field ->
        let ty, ctx, var_map = check_type_expression_impl ctx var_map ~accumulate_vars field.type_field_type in
        ((field.type_field_name, RowFieldPresent ty) :: rev_fields, ctx, var_map)
      ) ([], ctx, var_map) fields
    in
    let row_fields = List.rev rev_row_fields in
    let row_more, ctx =
      if is_open then Typing_context.new_type_variable ctx
      else (TypeRowEmpty, ctx)
    in
    (TypeRecord { row_fields; row_more }, ctx, var_map)

  | TypePolyVariant _row ->
    (* Polymorphic variant types - not yet implemented *)
    Compiler_error.type_error ty_expr.location "Polymorphic variant types are not yet implemented"

  | TypeForall _ ->
    (* TypeForall is only valid at the top level of a let binding annotation
       for polymorphic recursion. It's handled specially in expression_infer.ml *)
    Compiler_error.type_error ty_expr.location
      "'type a.' syntax can only appear at the top level of a let binding type annotation"

  | TypePackage path ->
    (* Package type: (module S) *)
    (* For now, create a package type with just the path and empty signature.
       The actual signature will be resolved during module type checking. *)
    let package_ty = Types.TypePackage {
      package_path = Types.PathLocal (String.concat "." path.value);
      package_signature = [];
    } in
    (package_ty, ctx, var_map)

(** Check a type expression from a signature and convert to semantic type.
    Note: This does NOT preserve type variable sharing - use check_type_expression_with_params
    for type declarations with parameters.

    @param ctx The typing context
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
let check_type_expression ctx ty_expr =
  let ty, ctx, _var_map = check_type_expression_impl ctx [] ~accumulate_vars:false ty_expr in
  (ty, ctx)

(** Check a type expression with declared type parameters.
    The param_names and param_vars must have the same length.
    Does NOT accumulate fresh variables - use check_gadt_return_type for that.

    @param ctx The typing context
    @param param_names List of type parameter names (e.g., ["'a"; "'b"])
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
let check_type_expression_with_params ctx param_names param_vars ty_expr =
  let var_map = List.combine param_names param_vars in
  let ty, ctx, _var_map = check_type_expression_impl ctx var_map ~accumulate_vars:false ty_expr in
  (ty, ctx)

(** Check a GADT constructor return type, accumulating fresh type variables.
    This ensures that multiple occurrences of the same type variable name
    (like 'a in ('a, 'a) eq) map to the same semantic variable.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic return type expression
    @return A tuple [(type_expr, updated_ctx, gadt_params)] where gadt_params
            are the fresh type variables introduced in the return type *)
let check_gadt_return_type ctx param_names param_vars ty_expr =
  let var_map = List.combine param_names param_vars in
  let ty, ctx, final_var_map = check_type_expression_impl ctx var_map ~accumulate_vars:true ty_expr in
  (* Extract the new variables that were added (not in original param_vars) *)
  let original_ids = List.map (fun tv -> tv.id) param_vars in
  let gadt_params =
    final_var_map
    |> List.filter (fun (_, tv) -> not (List.mem tv.id original_ids))
    |> List.map snd
  in
  (ty, ctx, gadt_params)

(** Check a GADT constructor (both argument and return types), sharing type variables.
    For a constructor like [Pair : ('a expr * 'b expr) -> ('a * 'b) expr],
    this ensures 'a and 'b refer to the same type variables in both positions.

    This function parses the return type first (accumulating fresh type variables),
    then parses the argument type using the same variable mapping. This ensures that
    type variables with the same name in both positions refer to the same semantic
    type variable.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param arg_ty_expr_opt Optional syntactic argument type expression
    @param ret_ty_expr The syntactic return type expression
    @return A tuple [(arg_type, ret_type, gadt_params, ctx)] where gadt_params
            are the fresh type variables introduced (e.g., 'a and 'b in Pair) *)
let check_gadt_constructor ctx param_names param_vars arg_ty_expr_opt ret_ty_expr =
  let var_map = List.combine param_names param_vars in
  (* First parse the return type, accumulating fresh variables *)
  let ret_ty, ctx, var_map_with_gadt = check_type_expression_impl ctx var_map ~accumulate_vars:true ret_ty_expr in
  (* Then parse the argument type using the same var_map (with GADT vars included) *)
  let arg_ty, ctx = match arg_ty_expr_opt with
    | Some arg_ty_expr ->
      let ty, ctx, _var_map = check_type_expression_impl ctx var_map_with_gadt ~accumulate_vars:false arg_ty_expr in
      (Some ty, ctx)
    | None -> (None, ctx)
  in
  (* Extract the GADT params (new variables not in original param_vars) *)
  let original_ids = List.map (fun tv -> tv.id) param_vars in
  let gadt_params =
    var_map_with_gadt
    |> List.filter (fun (_, tv) -> not (List.mem tv.id original_ids))
    |> List.map snd
  in
  (arg_ty, ret_ty, gadt_params, ctx)
