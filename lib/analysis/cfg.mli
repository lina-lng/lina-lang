(** Control Flow Graph construction.

    This module builds a control flow graph from typed expressions to enable
    dead code detection. *)

(** A segment of code in the control flow graph. *)
type segment = {
  id : int;
  mutable reachable : bool;
  location : Common.Location.t;
  mutable successors : int list;
  mutable predecessors : int list;
  is_diverging : bool;
      (** True if this segment ends with a diverging call (raise, exit, etc.). *)
}

(** A control flow graph for an expression. *)
type t = {
  entry : int;
  exit : int list;
      (** May be empty if expression diverges. *)
  segments : (int, segment) Hashtbl.t;
}

(** [build expr] constructs a control flow graph for the given expression. *)
val build : Typing.Typed_tree.typed_expression -> t

(** [mark_reachable cfg] marks all reachable segments starting from entry. *)
val mark_reachable : t -> unit

(** [find_unreachable cfg] returns all unreachable segments. *)
val find_unreachable : t -> segment list
