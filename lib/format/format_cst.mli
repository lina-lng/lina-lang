(** CST-based formatter: formats CST nodes while preserving trivia.

    This module provides formatting functions that work directly on the CST,
    preserving all comments and whitespace in their original positions.
    Unlike the AST-based formatter, this ensures lossless formatting. *)

open Cst

(** {1 Trivia Formatting} *)

(** [format_leading_trivia trivia] formats leading trivia.
    Leading trivia appears before a token and may include newlines and comments. *)
val format_leading_trivia : Green_tree.trivia_piece list -> Doc.doc

(** [format_trailing_trivia trivia] formats trailing trivia.
    Trailing trivia appears on the same line after a token. *)
val format_trailing_trivia : Green_tree.trivia_piece list -> Doc.doc

(** {1 Token Formatting} *)

(** [format_token token] formats a token including its trivia. *)
val format_token : Red_tree.syntax_token -> Doc.doc

(** [format_token_text token] formats just the token text without trivia. *)
val format_token_text : Red_tree.syntax_token -> Doc.doc

(** {1 Node Formatting} *)

(** [format_node node] formats any CST node to a Doc.

    This is the main formatting function. It dispatches to specialized
    formatters based on the node kind. *)
val format_node : Red_tree.syntax_node -> Doc.doc

(** [format_expression node] formats an expression node. *)
val format_expression : Red_tree.syntax_node -> Doc.doc

(** [format_pattern node] formats a pattern node. *)
val format_pattern : Red_tree.syntax_node -> Doc.doc

(** [format_type node] formats a type node. *)
val format_type : Red_tree.syntax_node -> Doc.doc

(** [format_binding node] formats a binding. *)
val format_binding : Red_tree.syntax_node -> Doc.doc

(** [format_structure_item node] formats a structure item. *)
val format_structure_item : Red_tree.syntax_node -> Doc.doc

(** {1 Main Entry Points} *)

(** [format_source_file node] formats a source file CST node.

    @param node The root node (should be NK_SOURCE_FILE)
    @return A Doc document representing the formatted source *)
val format_source_file : Red_tree.syntax_node -> Doc.doc

(** [format_string ~width content] parses and formats a source string.

    @param width Target line width (default 80)
    @param content The source code to format
    @return The formatted source code as a string *)
val format_string : ?width:int -> string -> string

(** [format_green_node node] formats a green node by wrapping it in a red tree.

    @param node The green node to format
    @return A Doc document *)
val format_green_node : Green_tree.green_node -> Doc.doc
