(** Trivia represents non-semantic source content: whitespace and comments.

    Trivia is attached to tokens to preserve formatting information during
    code transformation operations like formatting and refactoring.

    Attachment rules:
    - Leading trivia: content from the previous token's trailing newline
      up to (not including) the current token
    - Trailing trivia: content on the same line after the token,
      up to (but not including) the newline *)

open Common

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
  location : Location.t;
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

(** Empty trivia with no leading or trailing content. *)
let empty = { leading = []; trailing = [] }

(** Create trivia with only leading content. *)
let with_leading leading = { leading; trailing = [] }

(** Create trivia with only trailing content. *)
let with_trailing trailing = { leading = []; trailing }

(** Create trivia with both leading and trailing content. *)
let create ~leading ~trailing = { leading; trailing }

(** {1 Trivia Piece Predicates} *)

(** Check if a trivia piece is a newline. *)
let is_newline piece =
  match piece.kind with
  | Newline -> true
  | _ -> false

(** Check if a trivia piece is a comment (line or block). *)
let is_comment piece =
  match piece.kind with
  | LineComment _ | BlockComment _ -> true
  | _ -> false

(** {1 Trivia Queries} *)

(** Check if trivia contains any newlines. *)
let has_newline trivia =
  List.exists is_newline trivia.leading || List.exists is_newline trivia.trailing

(** Check if trivia contains any comments. *)
let has_comment trivia =
  List.exists is_comment trivia.leading || List.exists is_comment trivia.trailing

(** Check if trivia is empty (no leading or trailing content). *)
let is_empty trivia =
  trivia.leading = [] && trivia.trailing = []

(** Count the number of newlines in trivia (useful for blank line detection). *)
let newline_count trivia =
  let count pieces = List.length (List.filter is_newline pieces) in
  count trivia.leading + count trivia.trailing

(** Extract all comments from trivia (both leading and trailing). *)
let comments trivia =
  List.filter is_comment trivia.leading @ List.filter is_comment trivia.trailing

(** Get the text content of a trivia piece. *)
let trivia_piece_text piece =
  match piece.kind with
  | Whitespace s -> s
  | Newline -> "\n"
  | LineComment s -> s
  | BlockComment s -> s

(** Reconstruct the original source text from trivia. *)
let to_string trivia =
  let pieces_to_string pieces =
    pieces |> List.map trivia_piece_text |> String.concat ""
  in
  pieces_to_string trivia.leading ^ pieces_to_string trivia.trailing
