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

(** {1 Forward References for Mutual Recursion}

    These references enable cross-module formatting calls between
    expression, pattern, type, and module formatters. They are
    initialized in format_cst.ml before any formatting occurs. *)

(** Forward reference for format_node. *)
val format_node_ref : (Red_tree.syntax_node -> doc) ref

(** Forward reference for expression formatting. *)
val format_expression_ref : (Red_tree.syntax_node -> doc) ref

(** Forward reference for pattern formatting. *)
val format_pattern_ref : (Red_tree.syntax_node -> doc) ref

(** Forward reference for type formatting. *)
val format_type_ref : (Red_tree.syntax_node -> doc) ref

(** Format an expression using the forward reference. *)
val format_expression : Red_tree.syntax_node -> doc

(** Format a pattern using the forward reference. *)
val format_pattern : Red_tree.syntax_node -> doc

(** Format a type using the forward reference. *)
val format_type : Red_tree.syntax_node -> doc

(** {1 Generic Formatting} *)

(** Format all children of a node by concatenating them.

    This is the fallback formatter when semantic formatting isn't available. *)
val format_children : Red_tree.syntax_node -> doc

(** {1 Optional Formatting Helpers} *)

(** Format an optional token, returning empty if None. *)
val format_optional_token : Red_tree.syntax_token option -> doc

(** Format a list of nodes with a formatter, returning empty for empty list. *)
val format_node_list : (Red_tree.syntax_node -> doc) -> Red_tree.syntax_node list -> doc

(** Format a list of nodes with a custom separator, returning empty for empty list. *)
val format_node_list_sep :
  sep:doc -> (Red_tree.syntax_node -> doc) -> Red_tree.syntax_node list -> doc

(** {1 Required Accessor Combinators}

    These combinators handle the common pattern of extracting multiple
    optional values from a node and formatting them if all are present. *)

(** [with_required2 opt_a opt_b ~fallback ~success] calls [success a b] if both
    options are Some, otherwise calls [fallback ()]. *)
val with_required2 :
  'a option -> 'b option ->
  fallback:(unit -> 'result) ->
  success:('a -> 'b -> 'result) ->
  'result

(** [with_required3 opt_a opt_b opt_c ~fallback ~success] calls [success a b c]
    if all options are Some, otherwise calls [fallback ()]. *)
val with_required3 :
  'a option -> 'b option -> 'c option ->
  fallback:(unit -> 'result) ->
  success:('a -> 'b -> 'c -> 'result) ->
  'result

(** [with_required4 opt_a opt_b opt_c opt_d ~fallback ~success] calls
    [success a b c d] if all options are Some, otherwise calls [fallback ()]. *)
val with_required4 :
  'a option -> 'b option -> 'c option -> 'd option ->
  fallback:(unit -> 'result) ->
  success:('a -> 'b -> 'c -> 'd -> 'result) ->
  'result

(** [with_required5 opt_a opt_b opt_c opt_d opt_e ~fallback ~success] calls
    [success a b c d e] if all options are Some, otherwise calls [fallback ()]. *)
val with_required5 :
  'a option -> 'b option -> 'c option -> 'd option -> 'e option ->
  fallback:(unit -> 'result) ->
  success:('a -> 'b -> 'c -> 'd -> 'e -> 'result) ->
  'result

(** [format_token_or fallback opt_token] formats the token if present,
    otherwise returns [fallback]. Useful for optional delimiters. *)
val format_token_or : doc -> Red_tree.syntax_token option -> doc

(** Check if there's a blank line before a node (via its first token). *)
val has_blank_before : Red_tree.syntax_node -> bool

(** Format items with blank line preservation between them.

    [format_items_with_blank_sep ~formatter items] formats each item
    with [formatter] and separates them with hardlines, doubling the
    hardline when the original source had a blank line between items. *)
val format_items_with_blank_sep :
  formatter:(Red_tree.syntax_node -> doc) ->
  Red_tree.syntax_node list ->
  doc

(** {1 Format Combinators} *)

(** Format a parenthesized construct: (inner)

    [format_parens ~lparen_tok ~rparen_tok ~inner ~formatter node] formats
    a construct with optional parens around a single inner node. Falls back
    to [format_children] if any accessor returns [None]. *)
val format_parens :
  lparen_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  rparen_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  inner:(Red_tree.syntax_node -> Red_tree.syntax_node option) ->
  formatter:(Red_tree.syntax_node -> doc) ->
  Red_tree.syntax_node ->
  doc

(** Format a tuple-like construct: (elem, elem, ...)

    [format_tuple_like ~lparen_tok ~rparen_tok ~elements ~formatter node]
    formats a parenthesized list of elements separated by commas.
    Uses fallback delimiters when tokens are missing. *)
val format_tuple_like :
  lparen_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  rparen_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  elements:(Red_tree.syntax_node -> Red_tree.syntax_node list) ->
  formatter:(Red_tree.syntax_node -> doc) ->
  Red_tree.syntax_node ->
  doc

(** Format a braced container: \{ field; field; ... \}

    [format_braced ~lbrace_tok ~rbrace_tok ~fields ~format_field node]
    formats a braced container with semicolon-separated fields.
    Uses block formatting for multi-element containers. *)
val format_braced :
  lbrace_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  rbrace_tok:(Red_tree.syntax_node -> Red_tree.syntax_token option) ->
  fields:(Red_tree.syntax_node -> Red_tree.syntax_node list) ->
  format_field:(Red_tree.syntax_node -> doc) ->
  Red_tree.syntax_node ->
  doc

(** {1 Error Recovery} *)

(** [with_error_recovery ~formatter node] applies error-safe formatting.

    For error nodes, preserves original text.
    For nodes with errors in children, uses conservative formatting.
    Otherwise, calls [formatter kind node] with the node's syntax kind. *)
val with_error_recovery :
  formatter:(Syntax_kind.t -> Red_tree.syntax_node -> doc) ->
  Red_tree.syntax_node ->
  doc
