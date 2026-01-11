(** Expression formatting with semantic awareness.

    This module provides semantic formatters for expression nodes, using
    proper Doc combinators for intelligent line breaking and indentation.

    {2 Formatting Philosophy}

    - Short expressions stay on one line
    - Long expressions break at semantic boundaries
    - Operator expressions break before the operator
    - Nested structures use consistent indentation *)

open Cst
open Doc

(** {1 Forward References}

    These references enable cross-module recursion between expression,
    pattern, and type formatters. They are initialized by {!Format_cst}. *)

val format_pattern_ref : (Red_tree.syntax_node -> doc) ref
val format_type_ref : (Red_tree.syntax_node -> doc) ref

(** {1 Main Formatter} *)

(** Format any expression node.

    Dispatches to the appropriate semantic formatter based on node kind.
    Falls back to [Format_common.format_children] for unknown kinds. *)
val format_expression : Red_tree.syntax_node -> doc

(** {1 Specific Formatters} *)

(** Format a let expression.

    {[
      (* Short form *)
      let x = 1 in x + 1

      (* Long form *)
      let very_long_binding =
        complex_expression
      in
      body_expression
    ]} *)
val format_let_expr : Red_tree.syntax_node -> doc

(** Format an if expression.

    {[
      (* Single line *)
      if cond then a else b

      (* Multi-line *)
      if condition then
        true_branch
      else
        false_branch
    ]} *)
val format_if_expr : Red_tree.syntax_node -> doc

(** Format a match expression.

    {[
      match scrutinee with
      | Pattern1 -> body1
      | Pattern2 -> body2
    ]} *)
val format_match_expr : Red_tree.syntax_node -> doc

(** Format a match arm. *)
val format_match_arm : Red_tree.syntax_node -> doc

(** Format a function (lambda) expression.

    {[
      fun x y -> x + y
    ]} *)
val format_function_expr : Red_tree.syntax_node -> doc

(** Format a function application.

    {[
      (* Short *)
      f x y z

      (* Long - arguments break *)
      very_long_function_name
        argument1
        argument2
    ]} *)
val format_apply_expr : Red_tree.syntax_node -> doc

(** Format an infix (binary) expression.

    {[
      (* Short *)
      a + b + c

      (* Long - break before operator *)
      long_left_operand
        + long_right_operand
    ]} *)
val format_infix_expr : Red_tree.syntax_node -> doc

(** Format a tuple expression.

    {[
      (a, b, c)
    ]} *)
val format_tuple_expr : Red_tree.syntax_node -> doc

(** Format a record expression.

    {[
      (* Compact *)
      { x = 1; y = 2 }

      (* Expanded *)
      {
        long_field = long_value;
        another = value
      }
    ]} *)
val format_record_expr : Red_tree.syntax_node -> doc

(** Format a record field. *)
val format_record_field : Red_tree.syntax_node -> doc

(** Format a record update expression.

    {[
      { record with field = new_value }
    ]} *)
val format_record_update : Red_tree.syntax_node -> doc

(** Format a parenthesized expression. *)
val format_paren_expr : Red_tree.syntax_node -> doc

(** Format a constraint expression.

    {[
      (expr : type)
    ]} *)
val format_constraint_expr : Red_tree.syntax_node -> doc

(** Format a sequence expression.

    {[
      expr1;
      expr2
    ]} *)
val format_sequence_expr : Red_tree.syntax_node -> doc

(** Format a record access expression.

    {[
      record.field
    ]} *)
val format_record_access : Red_tree.syntax_node -> doc

(** Format a module access expression.

    {[
      Module.value
    ]} *)
val format_module_access : Red_tree.syntax_node -> doc

(** Format a constructor expression.

    {[
      Some value
    ]} *)
val format_constructor_expr : Red_tree.syntax_node -> doc
