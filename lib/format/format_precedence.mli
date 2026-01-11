(** Operator precedence for formatting decisions.

    This module provides precedence information for operators, which is used
    to determine when parentheses are needed in formatted output. *)

(** {1 Types} *)

(** Associativity of operators. *)
type associativity =
  | Left   (** Left-associative *)
  | Right  (** Right-associative *)
  | None   (** Non-associative *)

(** Position of a subexpression relative to its parent operator. *)
type position =
  | LeftOperand
  | RightOperand

(** {1 Precedence Functions} *)

(** [operator_precedence op] returns the precedence level of [op].
    Higher numbers bind tighter. Returns 0 for unknown operators. *)
val operator_precedence : string -> int

(** [operator_associativity op] returns the associativity of [op]. *)
val operator_associativity : string -> associativity

(** [needs_parens ~parent_op ~child_op ~position] determines if parentheses
    are needed for a child expression. *)
val needs_parens : parent_op:string -> child_op:string -> position:position -> bool

(** {1 Operator Classification} *)

(** [is_comparison op] returns true if [op] is a comparison operator. *)
val is_comparison : string -> bool

(** [is_arithmetic op] returns true if [op] is an arithmetic operator. *)
val is_arithmetic : string -> bool

(** [is_logical op] returns true if [op] is a logical operator. *)
val is_logical : string -> bool
