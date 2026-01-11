(** Common utilities for CST formatting.

    This module provides shared utilities for formatting CST nodes:
    - Trivia handling (whitespace, comments, newlines)
    - Token formatting
    - Blank line detection
    - List formatting helpers *)

open Cst
open Doc

(** {1 Trivia Formatting} *)

(** Format a single trivia piece to a doc.

    - [TK_WHITESPACE] is normalized to a single space
    - [TK_NEWLINE] uses [literalline] to preserve blank lines
    - Line comments are emitted as text
    - Block comments handle multi-line content *)
val format_trivia_piece : Green_tree.trivia_piece -> doc

(** Format leading trivia, skipping whitespace (indentation handled by Doc).

    Leading trivia appears before a token and may include newlines and comments.
    Whitespace is skipped because the Doc system handles indentation. *)
val format_leading_trivia : Green_tree.trivia_piece list -> doc

(** Format trailing trivia.

    Trailing trivia appears after a token on the same line.
    Line comments use [line_suffix] to stay at end of line. *)
val format_trailing_trivia : Green_tree.trivia_piece list -> doc

(** {1 Token Formatting} *)

(** Wrap a doc with token trivia.

    @param token The token providing trivia
    @param d The formatted content to wrap *)
val format_token_trivia : Red_tree.syntax_token -> doc -> doc

(** Format a token including its trivia. *)
val format_token : Red_tree.syntax_token -> doc

(** Format just the token text without trivia. *)
val format_token_text : Red_tree.syntax_token -> doc

(** {1 Blank Line Detection} *)

(** Check if there's a blank line before this token.

    Returns [true] if there are 2 or more newlines in leading trivia. *)
val has_blank_line_before : Red_tree.syntax_token -> bool

(** Count blank lines before this token.

    Returns the number of newlines minus 1 (0 if no blank lines). *)
val blank_lines_before : Red_tree.syntax_token -> int

(** {1 List Formatting} *)

(** Format a separated list of docs.

    [format_separated sep docs] joins [docs] with [sep] between each pair. *)
val format_separated : doc -> doc list -> doc

(** Format a separated list with optional trailing separator.

    [format_separated_trailing sep ~trailing docs] is like [format_separated]
    but optionally adds a trailing separator after the last element. *)
val format_separated_trailing : doc -> trailing:bool -> doc list -> doc

(** Format a list with a separator that includes a soft break.

    Useful for comma-separated lists that should break together. *)
val format_soft_separated : doc -> doc list -> doc

(** {1 Generic Formatting} *)

(** Format all children of a node by concatenating them.

    This is the fallback formatter when semantic formatting isn't available. *)
val format_children : Red_tree.syntax_node -> doc

(** Forward reference for format_node to enable mutual recursion.

    Must be set before using [format_children]. *)
val format_node_ref : (Red_tree.syntax_node -> doc) ref
