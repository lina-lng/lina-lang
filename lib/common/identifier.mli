type t = private {
  name : string;
  stamp : int;
}
[@@deriving show, eq, ord]

val create : string -> t
val create_with_stamp : string -> int -> t
val name : t -> string
val stamp : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
