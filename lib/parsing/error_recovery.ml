(** Error recovery for tolerant parsing.

    This module provides synchronization and recovery strategies for
    handling syntax errors during incremental parsing. The goal is to
    produce partial ASTs with error nodes rather than aborting on the
    first syntax error. *)

open Common

(** Tokens that mark the beginning of a new structure item.
    When we encounter a syntax error, we can skip to one of these
    to recover and continue parsing. *)
let is_structure_sync_token (token : Parser.token) : bool =
  match token with
  | Parser.LET
  | Parser.TYPE
  | Parser.MODULE
  | Parser.OPEN
  | Parser.INCLUDE
  | Parser.EXTERNAL
  | Parser.EOF -> true
  | _ -> false

(** Tokens that mark the end of an expression context.
    Used to recover within expressions. *)
let is_expression_sync_token (token : Parser.token) : bool =
  match token with
  | Parser.IN
  | Parser.END
  | Parser.THEN
  | Parser.ELSE
  | Parser.BAR
  | Parser.RPAREN
  | Parser.RBRACE
  | Parser.RBRACKET
  | Parser.SEMICOLON
  | Parser.EOF -> true
  | _ -> false

(** State for incremental lexing during error recovery. *)
type lexer_state = {
  state : Lexer.state;
  mutable last_token : Parser.token;
  mutable last_start : Lexing.position;
  mutable last_end : Lexing.position;
}

(** Create a lexer state for error recovery. *)
let create_lexer_state filename content : lexer_state =
  let initial_pos = {
    Lexing.pos_fname = filename;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  {
    state = Lexer.create_state filename content;
    last_token = Parser.EOF;
    last_start = initial_pos;
    last_end = initial_pos;
  }

(** Read the next token and update state. *)
let read_token (ls : lexer_state) : Parser.token * Lexing.position * Lexing.position =
  let lexer_token, loc = Lexer.next_token ls.state in
  let parser_token = Parsing_utils.lexer_token_to_parser_token lexer_token in
  let start_pos = Parsing_utils.location_to_lexing_position loc.start_pos in
  let end_pos = Parsing_utils.location_to_lexing_position loc.end_pos in
  ls.last_token <- parser_token;
  ls.last_start <- start_pos;
  ls.last_end <- end_pos;
  (parser_token, start_pos, end_pos)

(** Skip tokens until we find a synchronization point.
    Returns the location of skipped tokens. *)
let synchronize_to_structure (ls : lexer_state) : Location.t =
  let error_start = ls.last_start in
  let rec skip () =
    if is_structure_sync_token ls.last_token then
      Parsing_utils.location_from_positions error_start ls.last_end
    else begin
      let _ = read_token ls in
      skip ()
    end
  in
  skip ()

(** Create an error structure item from an error span. *)
let make_structure_error (error_span : Location.t) (message : string) : Syntax_tree.structure_item =
  let error_info = {
    Syntax_tree.error_message = message;
    error_span;
  } in
  Location.{
    value = Syntax_tree.StructureError error_info;
    location = error_span;
  }

(** Create an error expression from an error span. *)
let make_expression_error (error_span : Location.t) (message : string) : Syntax_tree.expression =
  let error_info = {
    Syntax_tree.error_message = message;
    error_span;
  } in
  Location.{
    value = Syntax_tree.ExpressionError error_info;
    location = error_span;
  }

(** Create an error pattern from an error span. *)
let make_pattern_error (error_span : Location.t) (message : string) : Syntax_tree.pattern =
  let error_info = {
    Syntax_tree.error_message = message;
    error_span;
  } in
  Location.{
    value = Syntax_tree.PatternError error_info;
    location = error_span;
  }
