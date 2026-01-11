(** Operator precedence for formatting decisions.

    This module provides precedence information for operators, which is used
    to determine when parentheses are needed in formatted output. *)

(** Associativity of operators. *)
type associativity =
  | Left   (** Left-associative: a op b op c = (a op b) op c *)
  | Right  (** Right-associative: a op b op c = a op (b op c) *)
  | None   (** Non-associative: parentheses required for chaining *)

(** Position of a subexpression relative to its parent operator. *)
type position =
  | LeftOperand
  | RightOperand

(** Get the precedence level of an operator.
    Higher numbers bind tighter. *)
let operator_precedence op =
  match op with
  (* Sequencing - lowest precedence *)
  | ";" -> 1
  (* Tuple construction *)
  | "," -> 2
  (* Logical or *)
  | "||" -> 3
  (* Logical and *)
  | "&&" -> 4
  (* Comparison operators *)
  | "=" | "<>" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> 5
  (* Cons and append *)
  | "::" -> 6
  | "@" | "^" -> 6
  (* Additive operators *)
  | "+" | "-" -> 7
  (* Multiplicative operators *)
  | "*" | "/" | "mod" -> 8
  (* Exponentiation *)
  | "**" -> 9
  (* Application - highest precedence *)
  | _ -> 0

(** Get the associativity of an operator. *)
let operator_associativity op =
  match op with
  (* Right-associative operators *)
  | "::" | "**" | "^" | "@" -> Right
  (* Non-associative comparison operators *)
  | "=" | "<>" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> None
  (* Most operators are left-associative *)
  | _ -> Left

(** Determine if parentheses are needed for a child expression.

    @param parent_op The operator of the parent expression
    @param child_op The operator of the child expression (if any)
    @param position Whether the child is the left or right operand
    @return true if parentheses are needed *)
let needs_parens ~parent_op ~child_op ~position =
  let parent_prec = operator_precedence parent_op in
  let child_prec = operator_precedence child_op in
  let parent_assoc = operator_associativity parent_op in
  if child_prec < parent_prec then
    (* Child binds less tightly - needs parens *)
    true
  else if child_prec > parent_prec then
    (* Child binds more tightly - no parens needed *)
    false
  else
    (* Same precedence - check associativity *)
    match parent_assoc, position with
    | Left, RightOperand -> true   (* (a op b) op c, not a op (b op c) *)
    | Right, LeftOperand -> true   (* a op (b op c), not (a op b) op c *)
    | None, _ -> true              (* Non-associative - always needs parens *)
    | _ -> false

(** Check if an operator is a comparison operator.
    Comparison operators are non-associative. *)
let is_comparison op =
  match op with
  | "=" | "<>" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

(** Check if an operator is an arithmetic operator. *)
let is_arithmetic op =
  match op with
  | "+" | "-" | "*" | "/" | "mod" | "**" -> true
  | _ -> false

(** Check if an operator is a logical operator. *)
let is_logical op =
  match op with
  | "&&" | "||" -> true
  | _ -> false
