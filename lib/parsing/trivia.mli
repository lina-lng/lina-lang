(** Trivia represents non-semantic source content: whitespace and comments.

    Trivia is attached to tokens to preserve formatting information during
    code transformation operations like formatting and refactoring.

    Attachment rules:
    - Leading trivia: content from the previous token's trailing newline
      up to (not including) the current token
    - Trailing trivia: content on the same line after the token,
      up to (but not including) the newline *)

(** The kind of trivia piece. *)
type trivia_kind =
  | Whitespace of string
      (** Horizontal whitespace (spaces, tabs). Does not include newlines. *)
  | Newline
      (** A line break. Stored separately to track line boundaries. *)
  | LineComment of string
      (** A line comment including the [--] prefix and content up to newline. *)
  | BlockComment of string
      (** A block comment including the [(*] and [*)] delimiters. *)
[@@deriving show, eq]

(** A single piece of trivia with its source location. *)
type trivia_piece = {
  kind : trivia_kind;
  location : Common.Location.t;
}
[@@deriving show, eq]

(** Trivia attached to a token.

    A token can have both leading and trailing trivia:
    - Leading trivia appears before the token (possibly including newlines
      and comments from previous lines)
    - Trailing trivia appears after the token on the same line *)
type t = {
  leading : trivia_piece list;
  trailing : trivia_piece list;
}
[@@deriving show, eq]

(** {1 Construction} *)

(** Empty trivia with no leading or trailing content. *)
val empty : t

(** Create trivia with only leading content. *)
val with_leading : trivia_piece list -> t

(** Create trivia with only trailing content. *)
val with_trailing : trivia_piece list -> t

(** Create trivia with both leading and trailing content. *)
val create : leading:trivia_piece list -> trailing:trivia_piece list -> t

(** {1 Queries} *)

(** Check if trivia contains any newlines. *)
val has_newline : t -> bool

(** Check if trivia contains any comments. *)
val has_comment : t -> bool

(** Check if trivia is empty (no leading or trailing content). *)
val is_empty : t -> bool

(** Count the number of newlines in trivia (useful for blank line detection). *)
val newline_count : t -> int

(** Extract all comments from trivia (both leading and trailing). *)
val comments : t -> trivia_piece list

(** {1 Conversion} *)

(** Get the text content of a trivia piece. *)
val trivia_piece_text : trivia_piece -> string

(** Reconstruct the original source text from trivia. *)
val to_string : t -> string
