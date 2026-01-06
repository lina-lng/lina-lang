type position = {
  filename : string;
  line : int;
  column : int;
  offset : int;
}
[@@deriving show, eq, ord]

type t = {
  start_pos : position;
  end_pos : position;
}
[@@deriving show, eq, ord]

val none : t
val is_none : t -> bool
val merge : t -> t -> t
val from_lexing_positions : Lexing.position -> Lexing.position -> t

type 'a located = {
  value : 'a;
  location : t;
}
[@@deriving show, eq]

val locate : 'a -> t -> 'a located
val map_located : ('a -> 'b) -> 'a located -> 'b located
val dummy_located : 'a -> 'a located
