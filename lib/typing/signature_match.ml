(** Signature matching for modules.

    Implements the algorithm to check if a module implementation
    satisfies a module signature specification.
*)

open Common
open Types

(** {1 Match Context}

    Context for signature matching operations, containing lookup functions
    for type aliases and module types. *)

(** Context for signature matching operations. *)
type match_context = {
  type_lookup : Unification.type_lookup;
  module_type_lookup : path -> Module_types.module_type option;
}

(** Create a match context with the given lookup functions. *)
let create_context ~type_lookup ~module_type_lookup =
  { type_lookup; module_type_lookup }

(** Create an expansion state from a match context. *)
let create_expansion_state ctx =
  Module_expansion.create_state ~env_lookup:ctx.module_type_lookup

(** Expand a ModTypeIdent to its definition if available.

    Uses the robust Module_expansion module for cycle detection and
    better error handling. Falls back to the unexpanded type if expansion fails. *)
let expand_module_type ctx mty =
  let state = create_expansion_state ctx in
  match Module_expansion.try_expand state mty with
  | Some expanded -> expanded
  | None -> mty  (* Can't expand - leave as is *)

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

(** Re-export path substitution functions from Type_utils for public API *)
let substitute_path_in_module_type old_path new_path mty =
  Type_utils.substitute_path_in_module_type ~old_path ~new_path mty

(** Result of signature matching *)
type match_error =
  | MissingValue of string
  | MissingType of string
  | MissingModule of string
  | TypeMismatch of string * type_expression * type_expression
  | ModuleTypeMismatch of string * string
  | WeakTypeEscape of string * type_expression  (** Weak type cannot satisfy polymorphic signature *)

type match_result = (unit, match_error) result

(** List of all match errors. Used by accumulating match functions. *)
type match_errors = match_error list

(** Create a fresh type variable for signature matching.
    Uses generic_level since these are free variables for structural comparison. *)
let fresh_match_var () =
  Types.new_type_variable_at_level Types.generic_level

(** Check if a type contains any weak type variables.
    Weak variables are non-generalizable and cannot satisfy polymorphic signatures. *)
let rec has_weak_variables ty =
  match Types.representative ty with
  | Types.TypeVariable tv -> tv.weak
  | Types.TypeArrow (arg, result) ->
    has_weak_variables arg || has_weak_variables result
  | Types.TypeTuple types ->
    List.exists has_weak_variables types
  | Types.TypeConstructor (_, args) ->
    List.exists has_weak_variables args
  | Types.TypeRecord row ->
    let has_weak_in_row_field (_, field) =
      match field with
      | Types.RowFieldPresent ty -> has_weak_variables ty
    in
    List.exists has_weak_in_row_field row.row_fields ||
    has_weak_variables row.row_more
  | Types.TypeRowEmpty -> false
  | Types.TypePolyVariant pv_row ->
    let has_weak_in_field (_, field) =
      match field with
      | Types.PVFieldPresent (Some ty) -> has_weak_variables ty
      | Types.PVFieldPresent None | Types.PVFieldAbsent -> false
    in
    List.exists has_weak_in_field pv_row.pv_fields ||
    has_weak_variables pv_row.pv_more

(** Check if a type scheme has weak variables in its body.
    This is used to detect weak type escape at module boundaries. *)
let scheme_has_weak_variables scheme =
  has_weak_variables scheme.Types.body

(** Check if an implementation type is compatible with a specification type.
    The implementation type must be at least as general as the spec type.
    Also checks that weak types don't escape to polymorphic positions. *)
let check_type_compatibility ctx _loc impl_scheme spec_scheme =
  (* Check for weak type escape: if spec is polymorphic but impl has weak vars *)
  let spec_is_polymorphic = spec_scheme.Types.quantified_variables <> [] in
  if spec_is_polymorphic && scheme_has_weak_variables impl_scheme then
    Error (WeakTypeEscape ("", impl_scheme.Types.body))
  else begin
    (* Do structural comparison after instantiation *)
    let impl_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var impl_scheme in
    let spec_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var spec_scheme in
    (* Try to unify - if it succeeds, implementation is compatible *)
    try
      Unification.unify ~type_lookup:ctx.type_lookup Location.none impl_ty spec_ty;
      Ok ()
    with
    | Compiler_error.Error _ ->
      Error (TypeMismatch ("", impl_ty, spec_ty))
  end

(** Check if two type declarations are compatible.
    - Parameter counts must match
    - If spec has manifest, impl must have compatible manifest
    - If spec is abstract, impl can be anything *)
let check_type_declaration_compatibility ctx name impl_decl spec_decl =
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
          let impl_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var impl_scheme in
          let spec_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var spec_scheme in
          Unification.unify ~type_lookup:ctx.type_lookup Location.none impl_ty spec_ty;
          Ok ()
        with
        | Compiler_error.Error _ ->
          Error (TypeMismatch (name, impl_manifest, spec_manifest))
        end
      end

(** Check if an implementation signature satisfies a specification signature *)
let rec match_signature ctx loc impl_sig spec_sig : match_result =
  (* For each item in the specification, check there's a matching item in implementation *)
  let check_item (spec_item : Module_types.signature_item) =
    match spec_item with
    | Module_types.SigValue (name, spec_val) ->
      begin match Module_types.find_value_in_sig name impl_sig with
      | Some impl_val ->
        begin match check_type_compatibility ctx loc impl_val.value_type spec_val.value_type with
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
        check_type_declaration_compatibility ctx name impl_decl spec_decl
      | None -> Error (MissingType name)
      end

    | Module_types.SigModule (name, spec_mty) ->
      begin match Module_types.find_module_in_sig name impl_sig with
      | Some impl_mty ->
        begin match match_module_type ctx loc impl_mty spec_mty with
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
and match_module_type ctx loc impl_mty spec_mty : match_result =
  (* First, expand any ModTypeIdent to their definitions *)
  let impl_mty = expand_module_type ctx impl_mty in
  let spec_mty = expand_module_type ctx spec_mty in
  match impl_mty, spec_mty with
  | Module_types.ModTypeSig impl_sig, Module_types.ModTypeSig spec_sig ->
    match_signature ctx loc impl_sig spec_sig

  | Module_types.ModTypeFunctor (param_impl, result_impl),
    Module_types.ModTypeFunctor (param_spec, result_spec) ->
    (* Functor matching:
       - Parameters match contravariantly (spec param against impl param)
       - Results match covariantly (impl result against spec result) *)
    begin match match_module_type ctx loc param_spec.parameter_type param_impl.parameter_type with
    | Error e -> Error e
    | Ok () ->
      (* Covariant: impl result must match spec result *)
      match_module_type ctx loc result_impl result_spec
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
  | WeakTypeEscape (name, ty) ->
    let prefix = if name = "" then "" else Printf.sprintf "for %s: " name in
    Printf.sprintf "Weak type escape %s%s cannot satisfy polymorphic signature"
      prefix (type_expression_to_string ty)

(** {1 Accumulating Match Functions}

    These functions collect all errors instead of stopping at the first one.
    Useful for LSP diagnostics where showing all issues at once is preferred. *)

(** Check if an implementation signature satisfies a specification signature,
    accumulating all errors instead of stopping at the first. *)
let rec match_signature_all ctx loc impl_sig spec_sig : match_errors =
  let check_item (spec_item : Module_types.signature_item) =
    match spec_item with
    | Module_types.SigValue (name, spec_val) ->
      begin match Module_types.find_value_in_sig name impl_sig with
      | Some impl_val ->
        begin match check_type_compatibility ctx loc impl_val.value_type spec_val.value_type with
        | Ok () -> []
        | Error (TypeMismatch (_, impl_ty, spec_ty)) ->
          [TypeMismatch (name, impl_ty, spec_ty)]
        | Error e -> [e]
        end
      | None ->
        [MissingValue name]
      end

    | Module_types.SigType (name, spec_decl) ->
      begin match Module_types.find_type_in_sig name impl_sig with
      | Some impl_decl ->
        begin match check_type_declaration_compatibility ctx name impl_decl spec_decl with
        | Ok () -> []
        | Error e -> [e]
        end
      | None -> [MissingType name]
      end

    | Module_types.SigModule (name, spec_mty) ->
      begin match Module_types.find_module_in_sig name impl_sig with
      | Some impl_mty ->
        match_module_type_all ctx loc impl_mty spec_mty
      | None ->
        [MissingModule name]
      end

    | Module_types.SigModuleType (_name, _spec_mty_opt) ->
      (* Module type declarations - skip for now *)
      []
  in
  (* Check all specification items and collect all errors *)
  List.fold_left (fun errors item ->
    errors @ check_item item
  ) [] spec_sig

(** Check if an implementation module type matches a specification module type,
    accumulating all errors instead of stopping at the first. *)
and match_module_type_all ctx loc impl_mty spec_mty : match_errors =
  let impl_mty = expand_module_type ctx impl_mty in
  let spec_mty = expand_module_type ctx spec_mty in
  match impl_mty, spec_mty with
  | Module_types.ModTypeSig impl_sig, Module_types.ModTypeSig spec_sig ->
    match_signature_all ctx loc impl_sig spec_sig

  | Module_types.ModTypeFunctor (param_impl, result_impl),
    Module_types.ModTypeFunctor (param_spec, result_spec) ->
    (* Collect errors from both parameter and result matching *)
    let param_errors = match_module_type_all ctx loc param_spec.parameter_type param_impl.parameter_type in
    let result_errors = match_module_type_all ctx loc result_impl result_spec in
    param_errors @ result_errors

  | Module_types.ModTypeIdent path1, Module_types.ModTypeIdent path2 ->
    if Types.path_equal path1 path2 then []
    else [ModuleTypeMismatch (
      Printf.sprintf "module type %s is not compatible with %s"
        (Types.path_to_string path1) (Types.path_to_string path2), "")]

  | Module_types.ModTypeIdent path, _ ->
    [ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), "")]

  | _, Module_types.ModTypeIdent path ->
    [ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), "")]

  | _ ->
    [ModuleTypeMismatch ("module type mismatch", "")]

(** Convert accumulated errors to a single match_result for backward compatibility *)
let errors_to_result errors =
  match errors with
  | [] -> Ok ()
  | first_error :: _ -> Error first_error
