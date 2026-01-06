type kind =
  | LexerError of string
  | ParserError of string
  | TypeError of string
  | InternalError of string
[@@deriving show]

type t = {
  kind : kind;
  location : Location.t;
  hints : string list;
}
[@@deriving show]

exception Error of t

val make : kind -> Location.t -> t
val with_hints : t -> string list -> t

val lexer_error : Location.t -> string -> 'a
val parser_error : Location.t -> string -> 'a
val type_error : Location.t -> string -> 'a
val internal_error : string -> 'a

val report : Format.formatter -> t -> unit
val report_to_string : t -> string
