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

let make kind location = { kind; location; hints = [] }
let with_hints error hints = { error with hints }

let raise_error kind location =
  raise (Error (make kind location))

let lexer_error location msg = raise_error (LexerError msg) location
let parser_error location msg = raise_error (ParserError msg) location
let type_error location msg = raise_error (TypeError msg) location
let internal_error msg = raise_error (InternalError msg) Location.none

let kind_to_string = function
  | LexerError _ -> "Lexer error"
  | ParserError _ -> "Parse error"
  | TypeError _ -> "Type error"
  | InternalError _ -> "Internal error"

let kind_message = function
  | LexerError msg -> msg
  | ParserError msg -> msg
  | TypeError msg -> msg
  | InternalError msg -> msg

let report fmt error =
  let loc = error.location in
  if Location.is_none loc then
    Format.fprintf fmt "@[<v>%s: %s@]"
      (kind_to_string error.kind)
      (kind_message error.kind)
  else
    Format.fprintf fmt "@[<v>File \"%s\", line %d, characters %d-%d:@,%s: %s@]"
      loc.start_pos.filename
      loc.start_pos.line
      loc.start_pos.column
      loc.end_pos.column
      (kind_to_string error.kind)
      (kind_message error.kind);
  List.iter (fun hint ->
    Format.fprintf fmt "@,Hint: %s" hint
  ) error.hints

let report_to_string error =
  Format.asprintf "%a" report error

(* Warning infrastructure *)

type warning =
  | NonExhaustiveMatch of string  (* witness description *)
  | RedundantPattern

type warning_info = {
  warning : warning;
  warning_location : Location.t;
}

let warnings : warning_info list ref = ref []

let emit_warning warning warning_location =
  warnings := { warning; warning_location } :: !warnings

let get_warnings () =
  let ws = List.rev !warnings in
  warnings := [];
  ws

let clear_warnings () = warnings := []

let warning_to_string = function
  | NonExhaustiveMatch witness ->
    Printf.sprintf "Non-exhaustive pattern matching, missing case: %s" witness
  | RedundantPattern ->
    "Redundant pattern: this case will never be matched"

let report_warning fmt { warning; warning_location = loc } =
  if Location.is_none loc then
    Format.fprintf fmt "@[<v>Warning: %s@]" (warning_to_string warning)
  else
    Format.fprintf fmt "@[<v>File \"%s\", line %d, characters %d-%d:@,Warning: %s@]"
      loc.start_pos.filename
      loc.start_pos.line
      loc.start_pos.column
      loc.end_pos.column
      (warning_to_string warning)

let report_warning_to_string w =
  Format.asprintf "%a" report_warning w
