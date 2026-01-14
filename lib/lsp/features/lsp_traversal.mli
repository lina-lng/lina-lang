(** Shared AST traversal utilities for LSP features.

    This module provides common functionality for finding nodes in the typed AST
    at a given source offset. Used by hover, go-to-definition, and other features
    that need to locate the AST node under the cursor. *)

(** A node found at a position - either an expression or a pattern. *)
type node_at_position =
  | ExpressionNode of Typing.Typed_tree.typed_expression
  | PatternNode of Typing.Typed_tree.typed_pattern

(** [find_pattern_at offset pattern] finds the innermost pattern containing [offset].

    Returns [Some pattern] if offset falls within the pattern's location,
    preferring child patterns over parents for precision. *)
val find_pattern_at :
  int -> Typing.Typed_tree.typed_pattern -> Typing.Typed_tree.typed_pattern option

(** [find_node_at offset expression] finds the innermost node containing [offset].

    Traverses the expression tree depth-first, checking patterns in binding positions
    (function parameters, let bindings, match arms) before their corresponding bodies.

    Returns [Some node] if offset falls within the expression's location,
    preferring deeper nodes over ancestors for precision. *)
val find_node_at :
  int -> Typing.Typed_tree.typed_expression -> node_at_position option

(** [find_node_in_structure offset structure] finds a node in a top-level structure.

    Searches through structure items (value bindings, modules) to find the
    innermost node containing [offset]. *)
val find_node_in_structure :
  int -> Typing.Typed_tree.typed_structure -> node_at_position option
