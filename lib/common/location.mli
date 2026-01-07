(** Source code locations for error reporting.

    This module provides types and utilities for tracking positions in source
    files. Locations are used throughout the compiler for error messages,
    debugging, and source mapping.

    A location consists of a start and end position, representing a span
    of text in the source file. *)

(** {1 Positions} *)

(** A position in a source file.

    Positions are 1-indexed for lines and columns to match editor conventions.
    The offset is 0-indexed from the start of the file. *)
type position = {
  filename : string;  (** Source file name *)
  line : int;         (** Line number (1-indexed) *)
  column : int;       (** Column number (1-indexed) *)
  offset : int;       (** Byte offset from file start (0-indexed) *)
}
[@@deriving show, eq, ord]

(** {1 Locations} *)

(** A span of source code between two positions.

    The span is inclusive of start and exclusive of end,
    following common convention. *)
type t = {
  start_pos : position;  (** Start of the span *)
  end_pos : position;    (** End of the span (exclusive) *)
}
[@@deriving show, eq, ord]

(** A dummy location for generated code or unknown positions. *)
val none : t

(** [is_none loc] returns [true] if [loc] is a dummy location. *)
val is_none : t -> bool

(** [merge loc1 loc2] returns a location spanning from the start of [loc1]
    to the end of [loc2]. Used to combine locations of sub-expressions. *)
val merge : t -> t -> t

(** [from_lexing_positions start end_] creates a location from
    standard library lexer positions. *)
val from_lexing_positions : Lexing.position -> Lexing.position -> t

(** {1 Located Values} *)

(** A value paired with its source location. *)
type 'a located = {
  value : 'a;       (** The wrapped value *)
  location : t;     (** Source location of the value *)
}
[@@deriving show, eq]

(** [locate value loc] wraps a value with its location. *)
val locate : 'a -> t -> 'a located

(** [map_located f loc_val] applies [f] to the value, preserving location. *)
val map_located : ('a -> 'b) -> 'a located -> 'b located

(** [dummy_located value] wraps a value with a dummy location. *)
val dummy_located : 'a -> 'a located
