(** Pattern formatting with semantic awareness.

    This module provides semantic formatters for pattern nodes. *)

open Cst
open Doc

(** {1 Forward References}

    This reference enables cross-module recursion with type formatters.
    It is initialized by {!Format_cst}. *)

val format_type_ref : (Red_tree.syntax_node -> doc) ref

(** {1 Main Formatter} *)

(** Format any pattern node.

    Dispatches to the appropriate semantic formatter based on node kind. *)
val format_pattern : Red_tree.syntax_node -> doc

(** {1 Specific Formatters} *)

(** Format a tuple pattern: [(a, b, c)] *)
val format_tuple_pattern : Red_tree.syntax_node -> doc

(** Format a constructor pattern: [Some x] *)
val format_constructor_pattern : Red_tree.syntax_node -> doc

(** Format a record pattern: [{ x; y }] *)
val format_record_pattern : Red_tree.syntax_node -> doc

(** Format a record pattern field. *)
val format_record_pattern_field : Red_tree.syntax_node -> doc

(** Format an alias pattern: [pat as name] *)
val format_alias_pattern : Red_tree.syntax_node -> doc

(** Format a constraint pattern: [(pat : type)] *)
val format_constraint_pattern : Red_tree.syntax_node -> doc

(** Format a parenthesized pattern: [(pat)] *)
val format_paren_pattern : Red_tree.syntax_node -> doc
