(** Hover information for the LSP server.

    This module provides type information when hovering over code elements. *)

(** Hover result with markdown content and optional range. *)
type hover_result = {
  contents : string;  (** Markdown content *)
  range : Lsp_types.range option;  (** Range of the hovered element *)
}

(** [get_hover store uri position] returns hover information at position.

    Returns type information for expressions, patterns, and bindings.
    Returns [None] if no meaningful hover info is available. *)
val get_hover :
  Document_store.t -> string -> Lsp_types.position -> hover_result option
