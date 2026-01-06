open Common

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
  | Lexer.EOF -> Parser.EOF

type lexer_state = {
  state : Lexer.state;
  mutable last_location : Location.t;
}

let location_to_lexing_position (pos : Location.position) : Lexing.position =
  {
    pos_fname = pos.filename;
    pos_lnum = pos.line;
    pos_bol = pos.offset - pos.column;
    pos_cnum = pos.offset;
  }

let make_lexer_wrapper lexer_state =
  fun () ->
    let token, loc = Lexer.next_token lexer_state.state in
    lexer_state.last_location <- loc;
    let start_pos = location_to_lexing_position loc.start_pos in
    let end_pos = location_to_lexing_position loc.end_pos in
    (lexer_token_to_parser_token token, start_pos, end_pos)

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
