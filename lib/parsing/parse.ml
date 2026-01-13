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

let structure_from_string content =
  parse Parser.structure "<string>" content

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

(** Helper to continue parsing from current position after error recovery.
    This is defined at module level to avoid issues with nested recursive functions. *)
let rec parse_from_lexer_state filename ls errors =
  let supplier = make_supplier ls in
  let current_token = ref Parser.EOF in
  let current_start = ref {
    Lexing.pos_fname = filename;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  let current_end = ref !current_start in

  let tracking_supplier () =
    let token, start_pos, end_pos = supplier () in
    current_token := token;
    current_start := start_pos;
    current_end := end_pos;
    (token, start_pos, end_pos)
  in

  (* Get the current position for the new parser *)
  let token, start_pos, end_pos = tracking_supplier () in
  let initial = Parser.Incremental.structure start_pos in

  let rec inner_loop checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
      let token, start_pos, end_pos = tracking_supplier () in
      let checkpoint = I.offer checkpoint (token, start_pos, end_pos) in
      inner_loop checkpoint

    | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      inner_loop checkpoint

    | I.HandlingError _env ->
      let error_loc = Parsing_utils.location_from_positions !current_start !current_end in
      errors := Printf.sprintf "Syntax error at line %d, column %d"
        error_loc.start_pos.line error_loc.start_pos.column :: !errors;
      let error_span = Error_recovery.synchronize_to_structure ls in
      let error_item = Error_recovery.make_structure_error error_span "Syntax error" in
      begin match !current_token with
      | Parser.EOF -> [error_item]
      | _ ->
        let rest = parse_from_lexer_state filename ls errors in
        error_item :: rest
      end

    | I.Accepted result -> result

    | I.Rejected -> []
  in
  (* Offer the first token we already read *)
  let checkpoint = I.offer initial (token, start_pos, end_pos) in
  inner_loop checkpoint

(** Parse a structure with error recovery.
    Returns the AST and a list of error messages. *)
let structure_from_string_tolerant ?(filename = "<string>") content
    : Syntax_tree.structure * string list =
  let ls = Error_recovery.create_lexer_state filename content in
  let supplier = make_supplier ls in

  (* Track current token for error reporting *)
  let current_token = ref Parser.EOF in
  let current_start = ref {
    Lexing.pos_fname = filename;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  let current_end = ref !current_start in

  (* Wrap supplier to track current token *)
  let tracking_supplier () =
    let token, start_pos, end_pos = supplier () in
    current_token := token;
    current_start := start_pos;
    current_end := end_pos;
    (token, start_pos, end_pos)
  in

  (* Errors collected during parsing *)
  let errors = ref [] in

  (* Initial checkpoint *)
  let initial = Parser.Incremental.structure {
    Lexing.pos_fname = filename;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  } in

  (* Run the incremental parser with error recovery *)
  let rec loop checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
      let token, start_pos, end_pos = tracking_supplier () in
      let checkpoint = I.offer checkpoint (token, start_pos, end_pos) in
      loop checkpoint

    | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop checkpoint

    | I.HandlingError _env ->
      (* Record the error location *)
      let error_loc = Parsing_utils.location_from_positions !current_start !current_end in
      errors := Printf.sprintf "Syntax error at line %d, column %d"
        error_loc.start_pos.line error_loc.start_pos.column :: !errors;

      (* Skip to next structure-level synchronization point *)
      let error_span = Error_recovery.synchronize_to_structure ls in

      (* Create error node *)
      let error_item = Error_recovery.make_structure_error error_span "Syntax error" in

      (* For structure-level recovery, we can't easily restart parsing from here
         with the incremental API. Instead, we'll parse the rest separately and
         concatenate. This is a simplified recovery strategy. *)

      (* Continue parsing from the sync point *)
      begin match !current_token with
      | Parser.EOF ->
        (* At EOF, just return what we have with the error node *)
        ([error_item], List.rev !errors)
      | _ ->
        (* Try to parse the rest of the file *)
        let remaining_result = parse_from_lexer_state filename ls errors in
        (error_item :: remaining_result, List.rev !errors)
      end

    | I.Accepted result ->
      (result, List.rev !errors)

    | I.Rejected ->
      (* Should not happen with proper error handling *)
      let error_loc = Parsing_utils.location_from_positions !current_start !current_end in
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
