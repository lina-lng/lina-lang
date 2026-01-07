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
    let rec subst ty =
      match representative ty with
      | TypeVariable tv ->
        begin match List.assoc_opt tv.id substitution with
        | Some replacement -> replacement
        | None -> ty
        end
      | TypeConstructor (path, type_args) ->
        TypeConstructor (path, List.map subst type_args)
      | TypeTuple elements ->
        TypeTuple (List.map subst elements)
      | TypeArrow (arg, result) ->
        TypeArrow (subst arg, subst result)
      | TypeRecord row ->
        TypeRecord (map_row_types subst row)
      | TypeRowEmpty -> TypeRowEmpty
    in
    subst body

(** {1 Constructor Instantiation} *)

let instantiate_constructor (ctor : constructor_info) =
  (* Create fresh type variables for each type parameter *)
  let fresh_params =
    List.map (fun _ -> new_type_variable ()) ctor.constructor_type_parameters
  in
  (* Build substitution from old variable IDs to fresh types *)
  let substitution =
    List.combine
      (List.map (fun tv -> tv.id) ctor.constructor_type_parameters)
      fresh_params
  in
  let rec substitute ty =
    match representative ty with
    | TypeVariable tv ->
      begin match List.assoc_opt tv.id substitution with
      | Some fresh_type -> fresh_type
      | None -> ty
      end
    | TypeConstructor (path, type_args) ->
      TypeConstructor (path, List.map substitute type_args)
    | TypeTuple elements ->
      TypeTuple (List.map substitute elements)
    | TypeArrow (arg, result) ->
      TypeArrow (substitute arg, substitute result)
    | TypeRecord row ->
      TypeRecord (map_row_types substitute row)
    | TypeRowEmpty -> TypeRowEmpty
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

let rec substitute_path_in_row_field ~old_path ~new_path (field : row_field) : row_field =
  match field with
  | RowFieldPresent ty ->
    let new_ty = substitute_path_in_type ~old_path ~new_path ty in
    if new_ty == ty then field
    else RowFieldPresent new_ty

and substitute_path_in_row ~old_path ~new_path (row : row) : row =
  let new_fields = List.map (fun (name, field) ->
    (name, substitute_path_in_row_field ~old_path ~new_path field)
  ) row.row_fields in
  let new_more = substitute_path_in_type ~old_path ~new_path row.row_more in
  { row_fields = new_fields; row_more = new_more }

and substitute_path_in_type ~old_path ~new_path ty =
  match ty with
  | TypeConstructor (path, args) ->
    let new_type_path = substitute_path_prefix ~old_path ~new_path path in
    let new_args = List.map (substitute_path_in_type ~old_path ~new_path) args in
    if new_type_path == path && new_args == args then ty
    else TypeConstructor (new_type_path, new_args)
  | TypeArrow (arg, result) ->
    let new_arg = substitute_path_in_type ~old_path ~new_path arg in
    let new_result = substitute_path_in_type ~old_path ~new_path result in
    if new_arg == arg && new_result == result then ty
    else TypeArrow (new_arg, new_result)
  | TypeTuple elements ->
    let new_elements = List.map (substitute_path_in_type ~old_path ~new_path) elements in
    if new_elements == elements then ty
    else TypeTuple new_elements
  | TypeRecord row ->
    let new_row = substitute_path_in_row ~old_path ~new_path row in
    TypeRecord new_row
  | TypeVariable _ | TypeRowEmpty -> ty

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
      let new_type = substitute_path_in_scheme ~old_path ~new_path desc.Module_types.val_type in
      if new_type == desc.Module_types.val_type then item
      else Module_types.SigValue (name, { desc with val_type = new_type })
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
  ) sig_

and substitute_path_in_module_type ~old_path ~new_path mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let new_sig = substitute_path_in_signature ~old_path ~new_path sig_ in
    if new_sig == sig_ then mty
    else Module_types.ModTypeSig new_sig
  | Module_types.ModTypeFunctor (param, result) ->
    let new_param_type = substitute_path_in_module_type ~old_path ~new_path param.Module_types.param_type in
    let new_result = substitute_path_in_module_type ~old_path ~new_path result in
    if new_param_type == param.Module_types.param_type && new_result == result then mty
    else Module_types.ModTypeFunctor (
      { param with param_type = new_param_type },
      new_result)
  | Module_types.ModTypeIdent path ->
    let new_path = substitute_path_prefix ~old_path ~new_path path in
    if new_path == path then mty
    else Module_types.ModTypeIdent new_path
