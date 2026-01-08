(** Go-to-definition support for the LSP server. *)

(** Definition location result. *)
type definition_result = {
  uri : string;
  range : Lsp_types.range;
}

(** Find definition at position.

    This is a stub implementation. Full implementation requires:
    - Tracking definition locations in the environment
    - Resolving identifiers at cursor position *)
let find_definition store uri (pos : Lsp_types.position) =
  (* Stub: return None for now *)
  let _ = store in
  let _ = uri in
  let _ = pos in
  None
