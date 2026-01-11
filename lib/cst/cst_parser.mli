(** CST parser: builds concrete syntax trees from token streams.

    This module provides a recursive descent parser that constructs CST nodes
    directly from tokens, preserving all trivia. The parser is designed to be
    error-tolerant and always produces a tree, even for invalid input. *)

(** {1 Main Entry Points} *)

(** [parse_structure filename content] parses a complete source file.

    Returns the root green node of the CST. The green tree is immutable
    and position-independent, suitable for caching and sharing.

    @param filename The name of the source file (for error messages)
    @param content The source code to parse
    @return The root green node representing the source file *)
val parse_structure : string -> string -> Green_tree.green_node

(** [parse filename content] parses source code and returns a red tree root.

    This is the main entry point for parsing with full navigation support.
    The red tree provides parent pointers and absolute positions.

    @param filename The name of the source file (for error messages)
    @param content The source code to parse
    @return A red tree root node with full navigation capabilities *)
val parse : string -> string -> Red_tree.syntax_node

(** [parse_expression_string filename content] parses a single expression.

    Useful for testing and REPL-like interfaces. Returns a source file
    node containing the expression.

    @param filename The name of the source file (for error messages)
    @param content The expression to parse
    @return The root green node containing the expression *)
val parse_expression_string : string -> string -> Green_tree.green_node
