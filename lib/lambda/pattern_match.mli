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
  | HCPolyVariant of string  (** polymorphic variant tag *)

(** Compiled decision tree.

    Uses inline records for cleaner pattern matching:
    - [DTFail]: Match failure (no clause matched)
    - [DTLeaf]: Successful match with bindings and action
    - [DTSwitch]: Branch on head constructor at an occurrence
    - [DTGuard]: Conditional with guard expression *)
type decision_tree =
  | DTFail
  | DTLeaf of {
      leaf_bindings : (Common.Identifier.t * occurrence) list;
      leaf_action : Typing.Typed_tree.typed_expression;
    }
  | DTSwitch of {
      switch_occurrence : occurrence;
      switch_cases : (head_constructor * decision_tree) list;
      switch_default : decision_tree option;
    }
  | DTGuard of {
      guard_bindings : (Common.Identifier.t * occurrence) list;
      guard_condition : Typing.Typed_tree.typed_expression;
      guard_then : decision_tree;
      guard_else : decision_tree;
    }

(** Compile match arms into a decision tree *)
val compile_match : Typing.Typed_tree.typed_match_arm list -> decision_tree

(** {1 List Split Utilities}

    These helpers perform single-pass list splitting, avoiding the O(n)
    double traversal of using List.filteri twice. *)

(** [split_at_remove idx lst] splits [lst] at index [idx], removing the element
    at that index.

    Returns [(before, after)] where [before] contains elements with index < [idx]
    and [after] contains elements with index > [idx].

    Example: [split_at_remove 2 [a;b;c;d;e]] returns [([a;b], [d;e])]

    Time complexity: O(n) single pass instead of O(2n) with two List.filteri calls. *)
val split_at_remove : int -> 'a list -> 'a list * 'a list

(** [remove_at idx lst] removes the element at index [idx] from [lst].

    Equivalent to [List.filteri (fun i _ -> i <> idx) lst] but single pass. *)
val remove_at : int -> 'a list -> 'a list

(** [head_constructor_key h] generates a unique string key for head constructor [h].

    Used for hash-based deduplication during head constructor collection.
    The key format ensures different constructors have different keys. *)
val head_constructor_key : head_constructor -> string
