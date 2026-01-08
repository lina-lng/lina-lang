(** Document state management for the LSP server.

    This module manages open documents in the editor, tracking their
    content, version, and cached analysis results. It handles
    incremental text updates and provides efficient position lookups. *)

(** {1 Document State} *)

(** Cached parse result for a document. *)
type parse_cache = {
  parse_version : int;
  ast : Parsing.Syntax_tree.structure option;
  parse_errors : Lsp_types.diagnostic list;
}

(** Cached type checking result for a document. *)
type typing_cache = {
  typing_version : int;
  typed_ast : Typing.Typed_tree.typed_structure option;
  environment : Typing.Environment.t;
  type_errors : Lsp_types.diagnostic list;
  warnings : Lsp_types.diagnostic list;
}

(** State of a single open document. *)
type document = {
  uri : string;
  version : int;
  content : string;
  line_offsets : int array;  (** Byte offset of each line start *)
  mutable parse_cache : parse_cache option;
  mutable typing_cache : typing_cache option;
}

(** The document store holding all open documents. *)
type t

(** {1 Creation} *)

(** Create an empty document store. *)
val create : unit -> t

(** {1 Document Lifecycle} *)

(** [open_document store ~uri ~content ~version] opens a new document.
    If a document with this URI already exists, it is replaced. *)
val open_document : t -> uri:string -> content:string -> version:int -> unit

(** [close_document store uri] closes and removes a document. *)
val close_document : t -> uri:string -> unit

(** [update_document store ~uri ~content ~version] replaces document content. *)
val update_document : t -> uri:string -> content:string -> version:int -> unit

(** {1 Document Access} *)

(** [get_document store uri] returns the document if it exists. *)
val get_document : t -> string -> document option

(** [get_all_uris store] returns all open document URIs. *)
val get_all_uris : t -> string list

(** [document_content store uri] returns document content if it exists. *)
val document_content : t -> string -> string option

(** {1 Position Conversions} *)

(** [position_to_offset doc line character] converts LSP position to byte offset.
    Returns [None] if position is out of bounds. *)
val position_to_offset : document -> int -> int -> int option

(** [offset_to_position doc offset] converts byte offset to LSP position.
    Returns [None] if offset is out of bounds. *)
val offset_to_position : document -> int -> (int * int) option

(** [lsp_position_to_offset doc pos] converts LSP position type to byte offset. *)
val lsp_position_to_offset : document -> Lsp_types.position -> int option

(** [offset_to_lsp_position doc offset] converts byte offset to LSP position type. *)
val offset_to_lsp_position : document -> int -> Lsp_types.position option

(** {1 Cache Management} *)

(** [invalidate_caches store uri] clears cached analysis for a document. *)
val invalidate_caches : t -> uri:string -> unit

(** [set_parse_cache store uri cache] updates the parse cache. *)
val set_parse_cache : t -> uri:string -> parse_cache -> unit

(** [set_typing_cache store uri cache] updates the typing cache. *)
val set_typing_cache : t -> uri:string -> typing_cache -> unit

(** [get_parse_cache store uri] returns the parse cache if valid. *)
val get_parse_cache : t -> string -> parse_cache option

(** [get_typing_cache store uri] returns the typing cache if valid. *)
val get_typing_cache : t -> string -> typing_cache option

(** {1 Utilities} *)

(** [filename_of_uri uri] extracts the file path from a file:// URI. *)
val filename_of_uri : string -> string

(** [uri_of_filename filename] creates a file:// URI from a path. *)
val uri_of_filename : string -> string
