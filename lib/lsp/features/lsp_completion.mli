(** Completion support for the LSP server.

    This module provides auto-completion suggestions based on the
    current context and available bindings. *)

(** Completion item kinds. *)
type completion_kind =
  | Function
  | Variable
  | Module
  | Type
  | Constructor
  | Field
  | Keyword

(** A completion item. *)
type completion_item = {
  label : string;
  kind : completion_kind;
  detail : string option;  (** Type signature *)
  documentation : string option;
}

(** [get_completions store uri position] returns completion items at position.

    Provides completions based on:
    - In-scope values and functions
    - Module members (after '.')
    - Type names
    - Constructors
    - Keywords *)
val get_completions :
  Document_store.t -> string -> Lsp_types.position -> completion_item list
