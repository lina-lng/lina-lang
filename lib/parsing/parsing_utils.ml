(** Shared utilities for parsing.

    This module contains functions shared between the standard parser
    and the error-tolerant parser to avoid code duplication. *)

open Common

(** Convert Lexer token to Parser token. *)
let lexer_token_to_parser_token : Lexer.token -> Parser.token = function
  | Lexer.INTEGER n -> Parser.INTEGER n
  | Lexer.FLOAT f -> Parser.FLOAT f
  | Lexer.STRING s -> Parser.STRING s
  | Lexer.LOWERCASE_IDENTIFIER s -> Parser.LOWERCASE_IDENTIFIER s
  | Lexer.UPPERCASE_IDENTIFIER s -> Parser.UPPERCASE_IDENTIFIER s
  | Lexer.TYPE_VARIABLE s -> Parser.TYPE_VARIABLE s
  | Lexer.TRUE -> Parser.TRUE
  | Lexer.FALSE -> Parser.FALSE
  | Lexer.LET -> Parser.LET
  | Lexer.REC -> Parser.REC
  | Lexer.IN -> Parser.IN
  | Lexer.FUN -> Parser.FUN
  | Lexer.IF -> Parser.IF
  | Lexer.THEN -> Parser.THEN
  | Lexer.ELSE -> Parser.ELSE
  | Lexer.TYPE -> Parser.TYPE
  | Lexer.OF -> Parser.OF
  | Lexer.AND -> Parser.AND
  | Lexer.AS -> Parser.AS
  | Lexer.MATCH -> Parser.MATCH
  | Lexer.WITH -> Parser.WITH
  | Lexer.WHEN -> Parser.WHEN
  | Lexer.MODULE -> Parser.MODULE
  | Lexer.STRUCT -> Parser.STRUCT
  | Lexer.END -> Parser.END
  | Lexer.SIG -> Parser.SIG
  | Lexer.FUNCTOR -> Parser.FUNCTOR
  | Lexer.OPEN -> Parser.OPEN
  | Lexer.INCLUDE -> Parser.INCLUDE
  | Lexer.VAL -> Parser.VAL
  | Lexer.EXTERNAL -> Parser.EXTERNAL
  | Lexer.AT -> Parser.AT
  | Lexer.LPAREN -> Parser.LPAREN
  | Lexer.RPAREN -> Parser.RPAREN
  | Lexer.LBRACKET -> Parser.LBRACKET
  | Lexer.RBRACKET -> Parser.RBRACKET
  | Lexer.LBRACE -> Parser.LBRACE
  | Lexer.RBRACE -> Parser.RBRACE
  | Lexer.COMMA -> Parser.COMMA
  | Lexer.SEMICOLON -> Parser.SEMICOLON
  | Lexer.COLON -> Parser.COLON
  | Lexer.DOT -> Parser.DOT
  | Lexer.DOTDOT -> Parser.DOTDOT
  | Lexer.ARROW -> Parser.ARROW
  | Lexer.EQUAL -> Parser.EQUAL
  | Lexer.BAR -> Parser.BAR
  | Lexer.UNDERSCORE -> Parser.UNDERSCORE
  | Lexer.STAR -> Parser.STAR
  | Lexer.PLUS -> Parser.PLUS
  | Lexer.MINUS -> Parser.MINUS
  | Lexer.SLASH -> Parser.SLASH
  | Lexer.LESS -> Parser.LESS
  | Lexer.GREATER -> Parser.GREATER
  | Lexer.LESS_EQUAL -> Parser.LESS_EQUAL
  | Lexer.GREATER_EQUAL -> Parser.GREATER_EQUAL
  | Lexer.EQUAL_EQUAL -> Parser.EQUAL_EQUAL
  | Lexer.NOT_EQUAL -> Parser.NOT_EQUAL
  | Lexer.REF -> Parser.REF
  | Lexer.BANG -> Parser.BANG
  | Lexer.COLONEQUALS -> Parser.COLONEQUALS
  | Lexer.EOF -> Parser.EOF

(** Convert Lina position to Lexing.position. *)
let location_to_lexing_position (pos : Location.position) : Lexing.position =
  {
    Lexing.pos_fname = pos.filename;
    pos_lnum = pos.line;
    pos_bol = pos.offset - pos.column;
    pos_cnum = pos.offset;
  }

(** Convert Lexing.position to Lina position. *)
let position_of_lexing (lpos : Lexing.position) : Location.position =
  {
    Location.filename = lpos.pos_fname;
    line = lpos.pos_lnum;
    column = lpos.pos_cnum - lpos.pos_bol;
    offset = lpos.pos_cnum;
  }

(** Create a location from start and end Lexing positions. *)
let location_from_positions (start_pos : Lexing.position) (end_pos : Lexing.position) : Location.t =
  {
    Location.start_pos = position_of_lexing start_pos;
    end_pos = position_of_lexing end_pos;
  }
