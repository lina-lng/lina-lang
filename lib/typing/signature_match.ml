(** Signature matching for modules.

    Implements the algorithm to check if a module implementation
    satisfies a module signature specification.
*)

open Common
open Types

(** Module type lookup function reference.
    Set this before calling match functions to enable ModTypeIdent expansion. *)
let module_type_lookup_ref : (path -> Module_types.module_type option) ref = ref (fun _ -> None)

(** Set the module type lookup function *)
let set_module_type_lookup f = module_type_lookup_ref := f

(** Expand a ModTypeIdent to its definition if available *)
let rec expand_module_type mty =
  match mty with
  | Module_types.ModTypeIdent path ->
    begin match !module_type_lookup_ref path with
    | Some expanded_mty -> expand_module_type expanded_mty
    | None -> mty  (* Can't expand - leave as is *)
    end
  | _ -> mty

(** Module strengthening: Makes abstract types concrete by binding them
    to their path. This ensures that `module N = M` makes N.t = M.t. *)

let rec strengthen_signature path sig_ =
  List.map (fun item ->
    match item with
    | Module_types.SigType (name, decl) ->
      (* Make abstract type concrete: t becomes bound to path.t *)
      let type_path = Types.PathDot (path, name) in
      let manifest = Types.TypeConstructor (type_path,
        List.map (fun tv -> Types.TypeVariable tv) decl.declaration_parameters) in
      Module_types.SigType (name, { decl with declaration_manifest = Some manifest })
    | Module_types.SigModule (name, mty) ->
      (* Recursively strengthen nested modules *)
      Module_types.SigModule (name, strengthen_module_type (Types.PathDot (path, name)) mty)
    | other -> other
  ) sig_

and strengthen_module_type path mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.ModTypeSig (strengthen_signature path sig_)
  | Module_types.ModTypeFunctor _ ->
    (* Don't strengthen functor bodies - they're not yet applied *)
    mty
  | Module_types.ModTypeIdent _ ->
    (* Named module types - could expand and strengthen *)
    mty

(** Path substitution: Replace occurrences of old_path with new_path.
    This is used when applying functors to substitute the parameter path
    with the argument path. *)

(** Substitute path prefix: if path starts with old_path, replace with new_path *)
let rec substitute_path_prefix old_path new_path path =
  if Types.path_equal path old_path then
    new_path
  else match path with
  | Types.PathDot (base, name) ->
    let new_base = substitute_path_prefix old_path new_path base in
    if new_base == base then path
    else Types.PathDot (new_base, name)
  | Types.PathApply (func, arg) ->
    let new_func = substitute_path_prefix old_path new_path func in
    let new_arg = substitute_path_prefix old_path new_path arg in
    if new_func == func && new_arg == arg then path
    else Types.PathApply (new_func, new_arg)
  | _ -> path

(** Substitute paths in a row field *)
let rec substitute_path_in_row_field old_path new_path (field : Types.row_field) : Types.row_field =
  match field with
  | Types.RowFieldPresent ty ->
    let new_ty = substitute_path_in_type old_path new_path ty in
    if new_ty == ty then field
    else Types.RowFieldPresent new_ty

(** Substitute paths in a row *)
and substitute_path_in_row old_path new_path (row : Types.row) : Types.row =
  let new_fields = List.map (fun (name, field) ->
    (name, substitute_path_in_row_field old_path new_path field)
  ) row.Types.row_fields in
  let new_more = substitute_path_in_type old_path new_path row.Types.row_more in
  { Types.row_fields = new_fields; row_more = new_more }

(** Substitute paths in a type expression *)
and substitute_path_in_type old_path new_path ty =
  match ty with
  | Types.TypeConstructor (path, args) ->
    let new_type_path = substitute_path_prefix old_path new_path path in
    let new_args = List.map (substitute_path_in_type old_path new_path) args in
    if new_type_path == path && new_args == args then ty
    else Types.TypeConstructor (new_type_path, new_args)
  | Types.TypeArrow (arg, result) ->
    let new_arg = substitute_path_in_type old_path new_path arg in
    let new_result = substitute_path_in_type old_path new_path result in
    if new_arg == arg && new_result == result then ty
    else Types.TypeArrow (new_arg, new_result)
  | Types.TypeTuple elements ->
    let new_elements = List.map (substitute_path_in_type old_path new_path) elements in
    if new_elements == elements then ty
    else Types.TypeTuple new_elements
  | Types.TypeRecord row ->
    let new_row = substitute_path_in_row old_path new_path row in
    Types.TypeRecord new_row
  | Types.TypeVariable _ | Types.TypeRowEmpty -> ty

(** Substitute paths in a type scheme *)
let substitute_path_in_scheme old_path new_path scheme =
  let new_body = substitute_path_in_type old_path new_path scheme.Types.body in
  if new_body == scheme.Types.body then scheme
  else { scheme with Types.body = new_body }

(** Substitute paths in a type declaration *)
let substitute_path_in_type_decl old_path new_path decl =
  let new_manifest = match decl.Types.declaration_manifest with
    | None -> None
    | Some ty -> Some (substitute_path_in_type old_path new_path ty)
  in
  if new_manifest == decl.Types.declaration_manifest then decl
  else { decl with Types.declaration_manifest = new_manifest }

(** Substitute paths in a signature *)
let rec substitute_path_in_signature old_path new_path sig_ =
  List.map (fun item ->
    match item with
    | Module_types.SigValue (name, desc) ->
      let new_type = substitute_path_in_scheme old_path new_path desc.val_type in
      if new_type == desc.val_type then item
      else Module_types.SigValue (name, { desc with val_type = new_type })
    | Module_types.SigType (name, decl) ->
      let new_decl = substitute_path_in_type_decl old_path new_path decl in
      if new_decl == decl then item
      else Module_types.SigType (name, new_decl)
    | Module_types.SigModule (name, mty) ->
      let new_mty = substitute_path_in_module_type old_path new_path mty in
      if new_mty == mty then item
      else Module_types.SigModule (name, new_mty)
    | Module_types.SigModuleType (name, mty_opt) ->
      let new_mty_opt = match mty_opt with
        | None -> None
        | Some mty -> Some (substitute_path_in_module_type old_path new_path mty)
      in
      if new_mty_opt == mty_opt then item
      else Module_types.SigModuleType (name, new_mty_opt)
  ) sig_

(** Substitute paths in a module type *)
and substitute_path_in_module_type old_path new_path mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    let new_sig = substitute_path_in_signature old_path new_path sig_ in
    if new_sig == sig_ then mty
    else Module_types.ModTypeSig new_sig
  | Module_types.ModTypeFunctor (param, result) ->
    let new_param_type = substitute_path_in_module_type old_path new_path param.param_type in
    let new_result = substitute_path_in_module_type old_path new_path result in
    if new_param_type == param.param_type && new_result == result then mty
    else Module_types.ModTypeFunctor (
      { param with param_type = new_param_type },
      new_result)
  | Module_types.ModTypeIdent path ->
    let new_path = substitute_path_prefix old_path new_path path in
    if new_path == path then mty
    else Module_types.ModTypeIdent new_path

(** Result of signature matching *)
type match_error =
  | MissingValue of string
  | MissingType of string
  | MissingModule of string
  | TypeMismatch of string * type_expression * type_expression
  | ModuleTypeMismatch of string * string

type match_result = (unit, match_error) result

(** Check if an implementation type is compatible with a specification type.
    The implementation type must be at least as general as the spec type. *)
let check_type_compatibility _loc impl_scheme spec_scheme =
  (* For now, do a simple structural comparison after instantiation *)
  (* A proper implementation would use subsumption checking *)
  let impl_ty = instantiate impl_scheme in
  let spec_ty = instantiate spec_scheme in
  (* Try to unify - if it succeeds, implementation is compatible *)
  try
    Unification.unify Location.none impl_ty spec_ty;
    Ok ()
  with
  | Compiler_error.Error _ ->
    Error (TypeMismatch ("", impl_ty, spec_ty))

(** Check if two type declarations are compatible.
    - Parameter counts must match
    - If spec has manifest, impl must have compatible manifest
    - If spec is abstract, impl can be anything *)
let check_type_declaration_compatibility name impl_decl spec_decl =
  (* Check parameter count *)
  let impl_params = List.length impl_decl.declaration_parameters in
  let spec_params = List.length spec_decl.declaration_parameters in
  if impl_params <> spec_params then
    Error (TypeMismatch (name,
      TypeConstructor (PathLocal name, []),
      TypeConstructor (PathLocal name, [])))
  else
    (* Check manifest compatibility *)
    match spec_decl.declaration_manifest with
    | None ->
      (* Spec is abstract - impl can be anything *)
      Ok ()
    | Some spec_manifest ->
      (* Spec has manifest - impl must have compatible manifest *)
      begin match impl_decl.declaration_manifest with
      | None ->
        (* Impl is abstract but spec has manifest - error *)
        Error (TypeMismatch (name,
          TypeConstructor (PathLocal name, []),
          spec_manifest))
      | Some impl_manifest ->
        (* Both have manifests - check they're compatible *)
        (* Create type schemes for comparison *)
        let impl_scheme = { quantified_variables = impl_decl.declaration_parameters; body = impl_manifest } in
        let spec_scheme = { quantified_variables = spec_decl.declaration_parameters; body = spec_manifest } in
        begin try
          let impl_ty = instantiate impl_scheme in
          let spec_ty = instantiate spec_scheme in
          Unification.unify Location.none impl_ty spec_ty;
          Ok ()
        with
        | Compiler_error.Error _ ->
          Error (TypeMismatch (name, impl_manifest, spec_manifest))
        end
      end

(** Check if an implementation signature satisfies a specification signature *)
let rec match_signature loc impl_sig spec_sig : match_result =
  (* For each item in the specification, check there's a matching item in implementation *)
  let check_item (spec_item : Module_types.signature_item) =
    match spec_item with
    | Module_types.SigValue (name, spec_val) ->
      begin match Module_types.find_value_in_sig name impl_sig with
      | Some impl_val ->
        begin match check_type_compatibility loc impl_val.val_type spec_val.val_type with
        | Ok () -> Ok ()
        | Error (TypeMismatch (_, impl_ty, spec_ty)) ->
          Error (TypeMismatch (name, impl_ty, spec_ty))
        | Error e -> Error e
        end
      | None ->
        Error (MissingValue name)
      end

    | Module_types.SigType (name, spec_decl) ->
      (* Check type exists and is compatible *)
      begin match Module_types.find_type_in_sig name impl_sig with
      | Some impl_decl ->
        check_type_declaration_compatibility name impl_decl spec_decl
      | None -> Error (MissingType name)
      end

    | Module_types.SigModule (name, spec_mty) ->
      begin match Module_types.find_module_in_sig name impl_sig with
      | Some impl_mty ->
        begin match match_module_type loc impl_mty spec_mty with
        | Ok () -> Ok ()
        | Error e -> Error e
        end
      | None ->
        Error (MissingModule name)
      end

    | Module_types.SigModuleType (_name, _spec_mty_opt) ->
      (* Module type declarations - skip for now *)
      Ok ()
  in
  (* Check all specification items *)
  List.fold_left (fun acc item ->
    match acc with
    | Error _ -> acc  (* Stop on first error *)
    | Ok () -> check_item item
  ) (Ok ()) spec_sig

(** Check if an implementation module type matches a specification module type *)
and match_module_type loc impl_mty spec_mty : match_result =
  (* First, expand any ModTypeIdent to their definitions *)
  let impl_mty = expand_module_type impl_mty in
  let spec_mty = expand_module_type spec_mty in
  match impl_mty, spec_mty with
  | Module_types.ModTypeSig impl_sig, Module_types.ModTypeSig spec_sig ->
    match_signature loc impl_sig spec_sig

  | Module_types.ModTypeFunctor (param_impl, result_impl),
    Module_types.ModTypeFunctor (param_spec, result_spec) ->
    (* Functor matching:
       - Parameters match contravariantly (spec param against impl param)
       - Results match covariantly (impl result against spec result) *)
    begin match match_module_type loc param_spec.param_type param_impl.param_type with
    | Error e -> Error e
    | Ok () ->
      (* Covariant: impl result must match spec result *)
      match_module_type loc result_impl result_spec
    end

  | Module_types.ModTypeIdent path1, Module_types.ModTypeIdent path2 ->
    (* Both are unexpandable named module types - check path equality *)
    if Types.path_equal path1 path2 then Ok ()
    else Error (ModuleTypeMismatch (
      Printf.sprintf "module type %s is not compatible with %s"
        (Types.path_to_string path1) (Types.path_to_string path2), ""))

  | Module_types.ModTypeIdent path, _ ->
    (* Couldn't expand impl - error *)
    Error (ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), ""))

  | _, Module_types.ModTypeIdent path ->
    (* Couldn't expand spec - error *)
    Error (ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), ""))

  | _ ->
    Error (ModuleTypeMismatch ("module type mismatch", ""))

(** Format a match error for display *)
let format_match_error = function
  | MissingValue name ->
    Printf.sprintf "Missing value: %s" name
  | MissingType name ->
    Printf.sprintf "Missing type: %s" name
  | MissingModule name ->
    Printf.sprintf "Missing module: %s" name
  | TypeMismatch (name, impl_ty, spec_ty) ->
    Printf.sprintf "Type mismatch for %s: implementation has %s, but signature expects %s"
      name (type_expression_to_string impl_ty) (type_expression_to_string spec_ty)
  | ModuleTypeMismatch (msg, _) ->
    msg
