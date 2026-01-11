(** Type expression formatting with semantic awareness.

    This module provides semantic formatters for type expression nodes. *)

open Cst
open Doc

(** {1 Main Formatter} *)

(** Format any type expression node.

    Dispatches to the appropriate semantic formatter based on node kind. *)
val format_type : Red_tree.syntax_node -> doc

(** {1 Specific Formatters} *)

(** Format an arrow type: ['a -> 'b] *)
val format_type_arrow : Red_tree.syntax_node -> doc

(** Format a tuple type: ['a * 'b * 'c] *)
val format_type_tuple : Red_tree.syntax_node -> doc

(** Format a type constructor: ['a option] or [('a, 'b) result] *)
val format_type_constructor : Red_tree.syntax_node -> doc

(** Format a record type: [{ x : int; y : string }] *)
val format_type_record : Red_tree.syntax_node -> doc

(** Format a record type field. *)
val format_type_record_field : Red_tree.syntax_node -> doc

(** Format a parenthesized type: [(type)] *)
val format_paren_type : Red_tree.syntax_node -> doc
