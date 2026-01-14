(** Robust module type expansion with cycle detection.

    See {!module:Module_expansion.mli} for detailed documentation. *)

open Types

(** Errors that can occur during module type expansion. *)
type expansion_error =
  | ModuleTypeNotFound of path
  | CyclicModuleType of path list
  | PathResolutionFailed of path * string

exception Expansion_error of expansion_error

(** Set of paths for tracking visited module types. *)
module PathSet = Set.Make(struct
  type t = path
  let compare = compare
end)

(** Expansion state with cycle detection. *)
type state = {
  visiting : PathSet.t;  (** Paths currently being expanded (for cycle detection) *)
  env_lookup : path -> Module_types.module_type option;  (** Environment lookup *)
}

let create_state ~env_lookup = {
  visiting = PathSet.empty;
  env_lookup;
}

(** Check if we're in a cycle and raise if so. *)
let check_cycle state path =
  if PathSet.mem path state.visiting then
    raise (Expansion_error (CyclicModuleType (path :: PathSet.elements state.visiting)))

(** Push a path onto the visiting set. *)
let push_visiting state path =
  { state with visiting = PathSet.add path state.visiting }

(** Fully expand a module type, resolving all ModTypeIdent references. *)
let rec expand state mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
      Module_types.ModTypeSig (expand_signature state sig_)

  | Module_types.ModTypeFunctor (param, result) ->
      let expanded_parameter_type = expand state param.Module_types.parameter_type in
      let expanded_result = expand state result in
      Module_types.ModTypeFunctor (
        { param with Module_types.parameter_type = expanded_parameter_type },
        expanded_result
      )

  | Module_types.ModTypeIdent path ->
      check_cycle state path;
      begin match state.env_lookup path with
      | Some resolved_mty ->
          let state' = push_visiting state path in
          expand state' resolved_mty
      | None ->
          raise (Expansion_error (ModuleTypeNotFound path))
      end

and expand_signature state sig_ =
  List.map (expand_signature_item state) sig_

and expand_signature_item state item =
  match item with
  | Module_types.SigValue (name, desc) ->
      Module_types.SigValue (name, desc)
  | Module_types.SigType (name, decl) ->
      Module_types.SigType (name, decl)
  | Module_types.SigModule (name, mty) ->
      Module_types.SigModule (name, expand state mty)
  | Module_types.SigModuleType (name, mty_opt) ->
      Module_types.SigModuleType (name, Option.map (expand state) mty_opt)

(** Try to expand, returning None on failure instead of raising. *)
let try_expand state mty =
  try Some (expand state mty)
  with Expansion_error _ -> None

(** Expand only the outermost ModTypeIdent, don't recurse into nested types. *)
let expand_shallow state mty =
  match mty with
  | Module_types.ModTypeIdent path ->
    begin match state.env_lookup path with
    | Some resolved -> resolved
    | None -> mty  (* Can't expand, return as-is *)
    end
  | _ -> mty

(** Format expansion error for display. *)
let format_error = function
  | ModuleTypeNotFound path ->
    Printf.sprintf "Module type not found: %s" (path_to_string path)

  | CyclicModuleType paths ->
    let path_strs = List.map path_to_string paths in
    let cycle_str = String.concat " -> " path_strs in
    Printf.sprintf
      "Cyclic module type definition detected: %s\n\n\
       Module type definitions cannot reference themselves, directly or indirectly."
      cycle_str

  | PathResolutionFailed (path, reason) ->
    Printf.sprintf "Cannot resolve module path %s: %s"
      (path_to_string path) reason
