(** LSP type definitions and conversions. *)

open Common

(** LSP position (0-indexed line and character). *)
type position = {
  line : int;
  character : int;
}
[@@deriving show, eq]

(** LSP range (start and end positions). *)
type range = {
  start_pos : position;
  end_pos : position;
}
[@@deriving show, eq]

(** Diagnostic severity levels. *)
type diagnostic_severity =
  | Error
  | Warning
  | Information
  | Hint
[@@deriving show, eq]

(** Related diagnostic information. *)
type related_diagnostic_info = {
  location_uri : string;
  location_range : range;
  related_message : string;
}
[@@deriving show, eq]

(** A diagnostic message with location and severity. *)
type diagnostic = {
  range : range;
  severity : diagnostic_severity;
  message : string;
  code : string option;
  source : string option;
  related_information : related_diagnostic_info list;
}
[@@deriving show, eq]

(** Convert Lina position to LSP position.

    Lina uses 1-indexed lines (from Lexing convention) but 0-indexed columns
    (computed as pos_cnum - pos_bol). LSP uses 0-indexed for both. *)
let position_of_lina_position (pos : Location.position) : position =
  {
    line = pos.line - 1;
    character = pos.column;  (* column is already 0-indexed *)
  }

(** Convert Lina location to LSP range. *)
let range_of_location (loc : Location.t) : range =
  {
    start_pos = position_of_lina_position loc.start_pos;
    end_pos = position_of_lina_position loc.end_pos;
  }

(** Create a Lina position from LSP position.

    Converts from LSP's 0-indexed positions to Lina's convention
    (1-indexed lines, 0-indexed columns). *)
let lina_position_of_position filename (pos : position) offset : Location.position =
  {
    filename;
    line = pos.line + 1;
    column = pos.character;  (* both LSP and Lina use 0-indexed columns *)
    offset;
  }

(** Convert severity to LSP integer code. *)
let severity_to_int = function
  | Error -> 1
  | Warning -> 2
  | Information -> 3
  | Hint -> 4

(** Convert LSP integer code to severity. *)
let severity_of_int = function
  | 1 -> Error
  | 2 -> Warning
  | 3 -> Information
  | 4 -> Hint
  | _ -> Information

(** Create a diagnostic at location. *)
let make_diagnostic ~severity ~message ?code ?(hints = []) loc =
  let message_with_hints =
    if hints = [] then message
    else
      let hints_str = String.concat "\n" (List.map (fun h -> "Hint: " ^ h) hints) in
      message ^ "\n" ^ hints_str
  in
  {
    range = range_of_location loc;
    severity;
    message = message_with_hints;
    code;
    source = Some "lina";
    related_information = [];
  }

(** Convert compiler error to diagnostic. *)
let diagnostic_of_compiler_error (err : Compiler_error.t) : diagnostic =
  let severity, code, message =
    match err.kind with
    | Compiler_error.LexerError msg -> (Error, "lexer", msg)
    | Compiler_error.ParserError msg -> (Error, "parser", msg)
    | Compiler_error.TypeError msg -> (Error, "type", msg)
    | Compiler_error.InternalError msg -> (Error, "internal", msg)
  in
  make_diagnostic ~severity ~message ~code ~hints:err.hints err.location

(** Create range covering entire document content. *)
let full_document_range (content : string) : range =
  let lines = String.split_on_char '\n' content in
  let line_count = List.length lines in
  let last_line_length =
    match List.rev lines with
    | [] -> 0
    | last :: _ -> String.length last
  in
  {
    start_pos = { line = 0; character = 0 };
    end_pos = { line = line_count - 1; character = last_line_length };
  }
