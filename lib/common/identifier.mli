(** Unique identifiers for runtime values.

    Identifiers combine a user-visible name with a unique stamp (integer).
    This allows multiple bindings with the same name to coexist without
    confusion, which is essential for:
    - Variable shadowing
    - Generated temporaries
    - Module system (multiple modules may have the same internal names)

    {2 Example}

    {[
      let x = Identifier.create "foo"  (* foo/0 *)
      let y = Identifier.create "foo"  (* foo/1 *)
      assert (not (Identifier.equal x y))
    ]} *)

(** {1 Identifier Type} *)

(** An identifier with a name and unique stamp.

    The [private] modifier prevents direct construction; use {!create}
    or {!create_with_stamp} instead. *)
type t = private {
  name : string;  (** User-visible name *)
  stamp : int;    (** Unique integer for disambiguation *)
}
[@@deriving show, eq, ord]

(** {1 Creation} *)

(** [create name] creates a fresh identifier with a new unique stamp.

    Each call returns an identifier with a different stamp, even for
    the same name.

    @param name The user-visible name
    @return A fresh identifier *)
val create : string -> t

(** [create_with_stamp name stamp] creates an identifier with a specific stamp.

    Use this for serialization/deserialization or when you need deterministic
    identifiers. For normal use, prefer {!create}.

    @param name The user-visible name
    @param stamp The stamp value
    @return An identifier with the given stamp *)
val create_with_stamp : string -> int -> t

(** {1 Accessors} *)

(** [name id] returns the user-visible name of [id]. *)
val name : t -> string

(** [stamp id] returns the unique stamp of [id]. *)
val stamp : t -> int

(** {1 Comparison} *)

(** [equal id1 id2] returns [true] if both identifiers have the same
    name and stamp. *)
val equal : t -> t -> bool

(** [compare id1 id2] provides a total ordering on identifiers.
    Orders first by name, then by stamp. *)
val compare : t -> t -> int

(** {1 Collections} *)

(** Set of identifiers. *)
module Set : Set.S with type elt = t

(** Map with identifier keys. *)
module Map : Map.S with type key = t
