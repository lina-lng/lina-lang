(** Signature matching for modules.

    Implements the algorithm to check if a module implementation
    satisfies a module signature specification.
*)

open Common
open Types

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

    | Module_types.SigType (name, _spec_decl) ->
      (* For now, just check the type exists *)
      (* A full implementation would check type definitions are compatible *)
      begin match Module_types.find_type_in_sig name impl_sig with
      | Some _impl_decl -> Ok ()
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

  | Module_types.ModTypeIdent _, _ | _, Module_types.ModTypeIdent _ ->
    (* Named module types - need to expand *)
    (* For now, just accept *)
    Ok ()

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
