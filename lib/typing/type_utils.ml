(** Type manipulation utilities.

    Consolidates type parameter substitution and path substitution
    operations used throughout the type checker and module system. *)

open Types

(** {1 Row Utilities} *)

let map_row_types f row = {
  row_fields = List.map (fun (name, field) ->
    (name, match field with
      | RowFieldPresent ty -> RowFieldPresent (f ty))
  ) row.row_fields;
  row_more = f row.row_more;
}

(** {1 Type Parameter Substitution} *)

let substitute_type_params params args body =
  if params = [] then body
  else
    let substitution = List.map2 (fun tv arg -> (tv.id, arg)) params args in
    Type_traversal.map (function
      | TypeVariable tv ->
        begin match List.assoc_opt tv.id substitution with
        | Some replacement -> replacement
        | None -> TypeVariable tv
        end
      | ty -> ty
    ) body

(** {1 Constructor Instantiation} *)

(** [instantiate_constructor ~fresh_var ctor] creates fresh type variables
    for constructor type parameters and substitutes them.

    @param fresh_var Function to create fresh type variables
    @param ctor The constructor info to instantiate
    @return Tuple of (argument_type option, result_type) with fresh variables *)
let instantiate_constructor ~fresh_var (ctor : constructor_info) =
  (* Create fresh type variables for each type parameter *)
  let fresh_params =
    List.map (fun _ -> fresh_var ()) ctor.constructor_type_parameters
  in
  (* Build substitution from old variable IDs to fresh types *)
  let substitution =
    List.combine
      (List.map (fun tv -> tv.id) ctor.constructor_type_parameters)
      fresh_params
  in
  let substitute ty =
    Type_traversal.map (function
      | TypeVariable tv ->
        begin match List.assoc_opt tv.id substitution with
        | Some fresh_type -> fresh_type
        | None -> TypeVariable tv
        end
      | ty -> ty
    ) ty
  in
  (Option.map substitute ctor.constructor_argument_type,
   substitute ctor.constructor_result_type)

(** {1 Path Substitution} *)

let rec substitute_path_prefix ~old_path ~new_path path =
  if path_equal path old_path then
    new_path
  else match path with
  | PathDot (base, name) ->
    let new_base = substitute_path_prefix ~old_path ~new_path base in
    if new_base == base then path
    else PathDot (new_base, name)
  | PathApply (func, arg) ->
    let new_func = substitute_path_prefix ~old_path ~new_path func in
    let new_arg = substitute_path_prefix ~old_path ~new_path arg in
    if new_func == func && new_arg == arg then path
    else PathApply (new_func, new_arg)
  | _ -> path

(** [substitute_path_in_type ~old_path ~new_path ty] substitutes [old_path]
    with [new_path] in all type constructor paths within [ty].

    Uses {!Type_traversal.map} for recursive traversal through all type structures
    including records, poly-variants, tuples, and arrows. *)
let substitute_path_in_type ~old_path ~new_path ty =
  Type_traversal.map (fun t ->
    match t with
    | TypeConstructor (path, args) ->
      let new_path = substitute_path_prefix ~old_path ~new_path path in
      if new_path == path then t
      else TypeConstructor (new_path, args)
    | _ -> t
  ) ty

let substitute_path_in_scheme ~old_path ~new_path scheme =
  let new_body = substitute_path_in_type ~old_path ~new_path scheme.body in
  if new_body == scheme.body then scheme
  else { scheme with body = new_body }

let substitute_path_in_type_decl ~old_path ~new_path decl =
  let new_manifest = match decl.declaration_manifest with
    | None -> None
    | Some ty -> Some (substitute_path_in_type ~old_path ~new_path ty)
  in
  if new_manifest == decl.declaration_manifest then decl
  else { decl with declaration_manifest = new_manifest }

let rec substitute_path_in_signature ~old_path ~new_path sig_ =
  List.map (fun item ->
    match item with
    | Module_types.SigValue (name, desc) ->
      let new_type = substitute_path_in_scheme ~old_path ~new_path desc.Module_types.value_type in
      if new_type == desc.Module_types.value_type then item
      else Module_types.SigValue (name, { desc with value_type = new_type })
    | Module_types.SigType (name, decl) ->
      let new_decl = substitute_path_in_type_decl ~old_path ~new_path decl in
      if new_decl == decl then item
      else Module_types.SigType (name, new_decl)
    | Module_types.SigModule (name, mty) ->
      let new_mty = substitute_path_in_module_type ~old_path ~new_path mty in
      if new_mty == mty then item
      else Module_types.SigModule (name, new_mty)
    | Module_types.SigModuleType (name, mty_opt) ->
      let new_mty_opt = match mty_opt with
        | None -> None
        | Some mty -> Some (substitute_path_in_module_type ~old_path ~new_path mty)
      in
      if new_mty_opt == mty_opt then item
      else Module_types.SigModuleType (name, new_mty_opt)
    | Module_types.SigExtensionConstructor _ ->
      (* Extension constructors don't need path substitution - they reference
         types by name which is already resolved *)
      item
  ) sig_

and substitute_path_in_module_type ~old_path ~new_path mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let new_sig = substitute_path_in_signature ~old_path ~new_path sig_ in
    if new_sig == sig_ then mty
    else Module_types.ModTypeSig new_sig
  | Module_types.ModTypeFunctor (param, result) ->
      let new_param, param_changed = match param with
        | Module_types.FunctorParamNamed { parameter_name; parameter_id; parameter_type } ->
            let new_type = substitute_path_in_module_type ~old_path ~new_path parameter_type in
            if new_type == parameter_type then (param, false)
            else (Module_types.FunctorParamNamed {
              parameter_name;
              parameter_id;
              parameter_type = new_type;
            }, true)
        | Module_types.FunctorParamUnit -> (param, false)
      in
      let new_result = substitute_path_in_module_type ~old_path ~new_path result in
      if not param_changed && new_result == result then mty
      else Module_types.ModTypeFunctor (new_param, new_result)
  | Module_types.ModTypeIdent path ->
    let new_path = substitute_path_prefix ~old_path ~new_path path in
    if new_path == path then mty
    else Module_types.ModTypeIdent new_path
