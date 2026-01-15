(** Module-level formatting with semantic awareness.

    This module provides semantic formatters for structure items, module
    expressions, signatures, and module-level declarations.

    {2 Key Features}

    - Blank line preservation between definitions
    - Proper indentation of module bodies
    - Consistent formatting of signatures and structures *)

open Cst
open Doc

(** {1 Main Formatters} *)

(** Format any structure item. *)
val format_structure_item : Red_tree.syntax_node -> doc

(** Format a list of structure items with blank line preservation. *)
val format_structure_items : Red_tree.syntax_node list -> doc

(** Format any module expression. *)
val format_module_expr : Red_tree.syntax_node -> doc

(** Format any signature item. *)
val format_signature_item : Red_tree.syntax_node -> doc

(** {1 Structure Item Formatters} *)

(** Format a value definition: [let x = ...] *)
val format_value_definition : Red_tree.syntax_node -> doc

(** Format a type definition: [type t = ...] *)
val format_type_definition : Red_tree.syntax_node -> doc

(** Format a type declaration (within a type definition). *)
val format_type_declaration : Red_tree.syntax_node -> doc

(** Format a constructor declaration. *)
val format_constructor_declaration : Red_tree.syntax_node -> doc

(** Format a module definition: [module M = ...] *)
val format_module_definition : Red_tree.syntax_node -> doc

(** Format an open declaration: [open M] *)
val format_open_declaration : Red_tree.syntax_node -> doc

(** Format an include declaration: [include M] *)
val format_include_declaration : Red_tree.syntax_node -> doc

(** Format an external declaration: [external ...] *)
val format_external_declaration : Red_tree.syntax_node -> doc

(** {1 Module Expression Formatters} *)

(** Format a structure: [struct ... end] *)
val format_structure : Red_tree.syntax_node -> doc

(** Format a functor expression. *)
val format_functor_expr : Red_tree.syntax_node -> doc

(** Format a functor parameter. *)
val format_functor_parameter : Red_tree.syntax_node -> doc

(** Format a functor application. *)
val format_module_apply : Red_tree.syntax_node -> doc

(** Format a module constraint: [(M : S)] *)
val format_module_constraint : Red_tree.syntax_node -> doc

(** {1 Signature Formatters} *)

(** Format a signature: [sig ... end] *)
val format_signature : Red_tree.syntax_node -> doc

(** Format a value specification: [val x : t] *)
val format_value_specification : Red_tree.syntax_node -> doc

(** {1 Binding Formatters} *)

(** Format a let binding. *)
val format_binding : Red_tree.syntax_node -> doc
