(** LSP document lookup pipeline.

    This module centralizes the common pattern of getting a document and
    converting positions to offsets, eliminating duplication across LSP
    feature modules. *)

(** {1 Context Type} *)

(** Context available during document operations. *)
type document_context = {
  store : Document_store.t;
  uri : string;
  doc : Document_store.document;
  offset : int;
}

(** {1 Pipeline Functions} *)

(** Run an operation with document context at a specific position.

    Returns [None] if document doesn't exist or position is invalid. *)
val with_document_at_position :
  Document_store.t ->
  string ->
  Lsp_types.position ->
  (document_context -> 'a option) ->
  'a option

(** Run an operation with document context, returning a default on failure.

    Useful for operations that return lists instead of options. *)
val with_document_at_position_or :
  Document_store.t ->
  string ->
  Lsp_types.position ->
  default:'a ->
  (document_context -> 'a) ->
  'a
