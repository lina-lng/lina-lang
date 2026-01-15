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

(** Format a source location as "File "path", line N, characters M-P". *)
let format_location fmt (loc : Location.t) =
  Format.fprintf fmt "File \"%s\", line %d, characters %d-%d"
    loc.start_pos.filename
    loc.start_pos.line
    loc.start_pos.column
    loc.end_pos.column

(** Format a message with optional location prefix. *)
let format_with_location fmt loc prefix message =
  if Location.is_none loc then
    Format.fprintf fmt "@[<v>%s: %s@]" prefix message
  else
    Format.fprintf fmt "@[<v>%a:@,%s: %s@]" format_location loc prefix message

let report fmt error =
  format_with_location fmt error.location
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

let report_warning fmt { warning; warning_location } =
  format_with_location fmt warning_location "Warning" (warning_to_string warning)

let report_warning_to_string w =
  Format.asprintf "%a" report_warning w

(** Report a warning as an error (for warnings promoted to errors). *)
let report_warning_as_error fmt { warning; warning_location } =
  format_with_location fmt warning_location "Error" (warning_to_string warning)

let report_warning_as_error_to_string w =
  Format.asprintf "%a" report_warning_as_error w

(* Enhanced Diagnostic System *)

type severity =
  | Error
  | Warning
  | Info
  | Hint

type applicability =
  | MachineApplicable
  | MaybeIncorrect
  | HasPlaceholders
  | Unspecified

type suggestion = {
  suggestion_message : string;
  replacement : string;
  suggestion_span : Location.t;
  applicability : applicability;
}

type label = {
  label_span : Location.t;
  label_message : string option;
  is_primary : bool;
}

type diagnostic = {
  severity : severity;
  code : Error_code.t option;
  message : string;
  labels : label list;
  notes : string list;
  suggestions : suggestion list;
}

let make_diagnostic ~severity ~message ?code ?(labels = []) ?(notes = [])
    ?(suggestions = []) () =
  { severity; code; message; labels; notes; suggestions }

let code_of_kind = function
  | LexerError _ -> Error_code.e_lexer_error
  | ParserError _ -> Error_code.e_syntax_error
  | TypeError _ -> Error_code.e_type_mismatch
  | InternalError _ -> Error_code.e_type_mismatch

let diagnostic_of_error err =
  let label = {
    label_span = err.location;
    label_message = None;
    is_primary = true;
  } in
  let labels = if Location.is_none err.location then [] else [label] in
  {
    severity = Error;
    code = Some (code_of_kind err.kind);
    message = kind_message err.kind;
    labels;
    notes = err.hints;
    suggestions = [];
  }

let warning_code_of_warning = function
  | NonExhaustiveMatch _ -> Error_code.w_non_exhaustive
  | RedundantPattern -> Error_code.w_redundant_pattern

(** Get the error code for a warning_info. *)
let warning_code { warning; _ } = warning_code_of_warning warning

let diagnostic_of_warning { warning; warning_location } =
  let label = {
    label_span = warning_location;
    label_message = None;
    is_primary = true;
  } in
  let labels = if Location.is_none warning_location then [] else [label] in
  {
    severity = Warning;
    code = Some (warning_code_of_warning warning);
    message = warning_to_string warning;
    labels;
    notes = [];
    suggestions = [];
  }

let error ?code message =
  { severity = Error; code; message; labels = []; notes = []; suggestions = [] }

let warning ?code message =
  { severity = Warning; code; message; labels = []; notes = []; suggestions = [] }

let with_label ~span ?message ?primary diag =
  let is_primary = match primary with
    | Some primary -> primary
    | None -> diag.labels = []
  in
  let label = { label_span = span; label_message = message; is_primary } in
  { diag with labels = diag.labels @ [label] }

let with_primary_label ~span ?message diag =
  with_label ~span ?message ~primary:true diag

let with_secondary_label ~span ?message diag =
  with_label ~span ?message ~primary:false diag

let with_note note diag =
  { diag with notes = diag.notes @ [note] }

let with_suggestion ~span ~message ~replacement ?(applicability = Unspecified) diag =
  let suggestion = {
    suggestion_message = message;
    replacement;
    suggestion_span = span;
    applicability;
  } in
  { diag with suggestions = diag.suggestions @ [suggestion] }
