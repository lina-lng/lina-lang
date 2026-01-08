(** Document symbols for the LSP server.

    This module provides document symbol information for the outline view. *)

(** Symbol kinds. *)
type symbol_kind =
  | Function
  | Variable
  | Module
  | Type
  | Constructor
  | Field

(** A document symbol. *)
type document_symbol = {
  name : string;
  kind : symbol_kind;
  range : Lsp_types.range;
  detail : string option;  (** Type signature *)
  children : document_symbol list;
}

(** [get_document_symbols store uri] returns all symbols in a document.

    Returns top-level definitions including:
    - Let bindings
    - Type definitions
    - Module definitions *)
val get_document_symbols :
  Document_store.t -> string -> document_symbol list
