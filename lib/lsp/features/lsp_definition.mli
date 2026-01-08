(** Go-to-definition support for the LSP server.

    This module provides definition location lookup for identifiers. *)

(** Definition location result. *)
type definition_result = {
  uri : string;
  range : Lsp_types.range;
}

(** [find_definition store uri position] finds the definition of symbol at position.

    Returns [None] if no definition can be found or the position doesn't
    contain an identifier. *)
val find_definition :
  Document_store.t -> string -> Lsp_types.position -> definition_result option
