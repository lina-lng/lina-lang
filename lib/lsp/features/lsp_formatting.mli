(** LSP formatting support.

    This module provides document formatting functionality using the
    CST-based formatter which preserves comments and trivia. *)

module Lsp = Linol.Lsp

(** [format_document store uri options] formats an entire document.

    Returns a list of text edits to apply, or [None] if the document
    is not found. Returns an empty list if formatting fails (to avoid
    breaking the document) or if the document is already formatted. *)
val format_document :
  Document_store.t ->
  string ->
  Lsp.Types.FormattingOptions.t ->
  Lsp.Types.TextEdit.t list option

(** [format_range store uri range options] formats a range within a document.

    The range is extended to complete syntactic boundaries to ensure
    valid formatting. Returns [None] if the document is not found,
    or an empty list if formatting fails. *)
val format_range :
  Document_store.t ->
  string ->
  Lsp.Types.Range.t ->
  Lsp.Types.FormattingOptions.t ->
  Lsp.Types.TextEdit.t list option
