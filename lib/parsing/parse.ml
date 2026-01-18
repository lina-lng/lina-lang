open Common

type lexer_state = {
  state : Lexer.state;
  mutable last_location : Location.t;
}

let make_lexer_wrapper lexer_state =
  fun () ->
    let token, loc = Lexer.next_token lexer_state.state in
    lexer_state.last_location <- loc;
    let start_pos = Parsing_utils.location_to_lexing_position loc.start_pos in
    let end_pos = Parsing_utils.location_to_lexing_position loc.end_pos in
    (Parsing_utils.lexer_token_to_parser_token token, start_pos, end_pos)

let parse parser_entry filename content =
  let state = Lexer.create_state filename content in
  let lexer_state = { state; last_location = Location.none } in
  let lexer = make_lexer_wrapper lexer_state in
  try
    MenhirLib.Convert.Simplified.traditional2revised parser_entry lexer
  with
  | Parser.Error ->
    Compiler_error.parser_error lexer_state.last_location "Syntax error"

let expression_from_string content =
  parse Parser.expression_eof "<string>" content

let structure_from_string ?(filename = "<string>") content =
  parse Parser.structure filename content

let structure_from_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  parse Parser.structure filename content

(** {1 Tolerant Parsing with Error Recovery}

    These functions use Menhir's incremental API to parse with error recovery,
    producing partial ASTs with error nodes instead of failing on the first
    syntax error. *)

module I = Parser.MenhirInterpreter

(** Supplier function type for incremental parsing. *)
type supplier = unit -> Parser.token * Lexing.position * Lexing.position

(** Create a supplier from a lexer state. *)
let make_supplier (ls : Error_recovery.lexer_state) : supplier =
  fun () -> Error_recovery.read_token ls

(** Tracking state for error reporting during incremental parsing. *)
type tracking_state = {
  mutable current_token : Parser.token;
  mutable current_start : Lexing.position;
  mutable current_end : Lexing.position;
}

(** Create initial tracking state. *)
let make_tracking_state filename = {
  current_token = Parser.EOF;
  current_start = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  current_end = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
}

(** Wrap a supplier to track current token position for error reporting. *)
let wrap_tracking_supplier tracking supplier () =
  let token, start_pos, end_pos = supplier () in
  tracking.current_token <- token;
  tracking.current_start <- start_pos;
  tracking.current_end <- end_pos;
  (token, start_pos, end_pos)

(** Get current error location from tracking state. *)
let tracking_error_location tracking =
  Parsing_utils.location_from_positions tracking.current_start tracking.current_end

(** Helper to continue parsing from current position after error recovery. *)
let rec parse_from_lexer_state filename ls errors =
  let tracking = make_tracking_state filename in
  let tracking_supplier = wrap_tracking_supplier tracking (make_supplier ls) in

  let token, start_pos, end_pos = tracking_supplier () in
  let initial = Parser.Incremental.structure start_pos in

  let rec inner_loop checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
        let token, start_pos, end_pos = tracking_supplier () in
        inner_loop (I.offer checkpoint (token, start_pos, end_pos))

    | I.Shifting _ | I.AboutToReduce _ ->
        inner_loop (I.resume checkpoint)

    | I.HandlingError _env ->
        let error_loc = tracking_error_location tracking in
        errors := Printf.sprintf "Syntax error at line %d, column %d"
          error_loc.start_pos.line error_loc.start_pos.column :: !errors;

        let error_span = Error_recovery.synchronize_to_structure ls in
        let error_item = Error_recovery.make_structure_error error_span "Syntax error" in

        begin match tracking.current_token with
        | Parser.EOF -> [error_item]
        | _ -> error_item :: parse_from_lexer_state filename ls errors
        end

    | I.Accepted result -> result
    | I.Rejected -> []
  in

  inner_loop (I.offer initial (token, start_pos, end_pos))

(** Parse a structure with error recovery.
    Returns the AST and a list of error messages. *)
let structure_from_string_tolerant ?(filename = "<string>") content
    : Syntax_tree.structure * string list =
  let ls = Error_recovery.create_lexer_state filename content in
  let tracking = make_tracking_state filename in
  let tracking_supplier = wrap_tracking_supplier tracking (make_supplier ls) in
  let errors = ref [] in

  let initial_pos = tracking.current_start in
  let initial = Parser.Incremental.structure initial_pos in

  let rec loop checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
        let token, start_pos, end_pos = tracking_supplier () in
        loop (I.offer checkpoint (token, start_pos, end_pos))

    | I.Shifting _ | I.AboutToReduce _ ->
        loop (I.resume checkpoint)

    | I.HandlingError _env ->
        let error_loc = tracking_error_location tracking in
        errors := Printf.sprintf "Syntax error at line %d, column %d"
          error_loc.start_pos.line error_loc.start_pos.column :: !errors;

        let error_span = Error_recovery.synchronize_to_structure ls in
        let error_item = Error_recovery.make_structure_error error_span "Syntax error" in

        begin match tracking.current_token with
        | Parser.EOF ->
            ([error_item], List.rev !errors)
        | _ ->
            let remaining = parse_from_lexer_state filename ls errors in
            (error_item :: remaining, List.rev !errors)
        end

    | I.Accepted result ->
        (result, List.rev !errors)

    | I.Rejected ->
        let error_loc = tracking_error_location tracking in
        errors := Printf.sprintf "Parser rejected at line %d"
          error_loc.start_pos.line :: !errors;
        ([], List.rev !errors)
  in

  loop initial

(** Parse a structure from file with error recovery. *)
let structure_from_file_tolerant filename : Syntax_tree.structure * string list =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  structure_from_string_tolerant ~filename content
