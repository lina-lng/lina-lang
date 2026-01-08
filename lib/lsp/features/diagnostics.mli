(** Diagnostic computation for the LSP server.

    This module computes diagnostics (errors and warnings) for documents
    by running the parse and type checking phases. *)

(** {1 Diagnostic Computation} *)

(** [compute_diagnostics store uri] computes all diagnostics for a document.

    This runs parsing and type checking, caching results in the document store.
    Returns parse errors, type errors, and warnings combined. *)
val compute_diagnostics : Document_store.t -> string -> Lsp_types.diagnostic list

(** [parse_document store uri] parses a document and returns parse errors.

    Results are cached in the document store. *)
val parse_document :
  Document_store.t ->
  string ->
  Parsing.Syntax_tree.structure option * Lsp_types.diagnostic list

(** [type_check_document store uri] type checks a document.

    Requires parsing to succeed first. Returns typed AST, environment,
    type errors, and warnings. Results are cached. *)
val type_check_document :
  Document_store.t ->
  string ->
  Typing.Typed_tree.typed_structure option
  * Typing.Environment.t
  * Lsp_types.diagnostic list

(** {1 Error Conversion} *)

(** [diagnostic_of_accumulated_error err] converts an accumulated error. *)
val diagnostic_of_accumulated_error :
  Typing.Error_accumulator.error -> Lsp_types.diagnostic

(** [diagnostic_of_warning info] converts a compiler warning. *)
val diagnostic_of_warning : Common.Compiler_error.warning_info -> Lsp_types.diagnostic
