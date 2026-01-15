(** LSP document lookup pipeline.

    This module centralizes the common pattern of getting a document and
    converting positions to offsets, eliminating duplication across LSP
    feature modules. *)

let ( let* ) = Option.bind

(** Context available during document operations. *)
type document_context = {
  store : Document_store.t;
  uri : string;
  doc : Document_store.document;
  offset : int;
}

(** Run an operation with document context at a specific position.

    Returns [None] if document doesn't exist or position is invalid. *)
let with_document_at_position store uri (pos : Lsp_types.position) f =
  let* doc = Document_store.get_document store uri in
  let* offset = Document_store.lsp_position_to_offset doc pos in
  f { store; uri; doc; offset }

(** Run an operation with document context, returning a default on failure.

    Useful for operations that return lists instead of options. *)
let with_document_at_position_or store uri pos ~default f =
  match with_document_at_position store uri pos (fun ctx -> Some (f ctx)) with
  | Some result -> result
  | None -> default
