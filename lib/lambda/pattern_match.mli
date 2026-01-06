(** Pattern match compilation using Maranget's decision tree algorithm *)

(** Occurrence step - path element to access sub-values *)
type occurrence_step =
  | OccTupleField of int
  | OccConstructorArg
  | OccRecordField of string

(** Occurrence - path from scrutinee to sub-value *)
type occurrence = occurrence_step list

(** Head constructor for switching *)
type head_constructor =
  | HCWildcard
  | HCConstant of Parsing.Syntax_tree.constant
  | HCTuple of int
  | HCConstructor of string * int  (** name, tag_index *)
  | HCRecord of string list

(** Leaf information in decision tree *)
type leaf_info = {
  leaf_bindings : (Common.Identifier.t * occurrence) list;
  leaf_action : Typing.Typed_tree.typed_expression;
}

(** Switch information in decision tree *)
type switch_info = {
  switch_occurrence : occurrence;
  switch_cases : (head_constructor * decision_tree) list;
  switch_default : decision_tree option;
}

(** Guard information in decision tree *)
and guard_info = {
  guard_bindings : (Common.Identifier.t * occurrence) list;
  guard_condition : Typing.Typed_tree.typed_expression;
  guard_then : decision_tree;
  guard_else : decision_tree;
}

(** Compiled decision tree *)
and decision_tree =
  | DTFail
  | DTLeaf of leaf_info
  | DTSwitch of switch_info
  | DTGuard of guard_info

(** Compile match arms into a decision tree *)
val compile_match : Typing.Typed_tree.typed_match_arm list -> decision_tree
