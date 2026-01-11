(** CST-based formatter: formats CST nodes while preserving trivia.

    This module provides formatting functions that work directly on the CST,
    preserving all comments and whitespace in their original positions.
    Unlike the AST-based formatter, this ensures lossless formatting.

    {2 Architecture}

    The formatter is organized into specialized modules:
    - {!Format_common} - Trivia handling, token formatting, blank line detection
    - {!Format_accessors} - Typed child accessors for CST nodes
    - {!Format_expr} - Expression formatting
    - {!Format_pattern} - Pattern formatting
    - {!Format_type} - Type expression formatting
    - {!Format_module} - Module-level formatting (structures, signatures) *)

open Cst

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

(** [format_module_expr node] formats a module expression. *)
val format_module_expr : Red_tree.syntax_node -> Doc.doc

(** [format_signature_item node] formats a signature item. *)
val format_signature_item : Red_tree.syntax_node -> Doc.doc

(** {1 Main Entry Points} *)

(** [format_source_file node] formats a source file CST node.

    @param node The root node (should be NK_SOURCE_FILE)
    @return A Doc document representing the formatted source *)
val format_source_file : Red_tree.syntax_node -> Doc.doc

(** [format_string ~width ~indent content] parses and formats a source string.

    @param width Target line width (default 80)
    @param indent Spaces per indentation level (default 2)
    @param content The source code to format
    @return The formatted source code as a string *)
val format_string : ?width:int -> ?indent:int -> string -> string

(** [format_green_node node] formats a green node by wrapping it in a red tree.

    @param node The green node to format
    @return A Doc document *)
val format_green_node : Green_tree.green_node -> Doc.doc
