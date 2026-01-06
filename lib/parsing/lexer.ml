open Common

type token =
  | INTEGER of int
  | FLOAT of float
  | STRING of string
  | LOWERCASE_IDENTIFIER of string
  | UPPERCASE_IDENTIFIER of string
  | TYPE_VARIABLE of string
  | TRUE
  | FALSE
  | LET
  | REC
  | IN
  | FUN
  | IF
  | THEN
  | ELSE
  | TYPE
  | OF
  | AND
  | AS
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMICOLON
  | COLON
  | ARROW
  | EQUAL
  | BAR
  | UNDERSCORE
  | STAR
  | PLUS
  | MINUS
  | SLASH
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | EQUAL_EQUAL
  | NOT_EQUAL
  | EOF
[@@deriving show]

let digit = [%sedlex.regexp? '0' .. '9']
let lowercase_letter = [%sedlex.regexp? 'a' .. 'z']
let uppercase_letter = [%sedlex.regexp? 'A' .. 'Z']
let letter = [%sedlex.regexp? lowercase_letter | uppercase_letter]
let identifier_char = [%sedlex.regexp? letter | digit | '_' | '\'']
let whitespace = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r']

let keywords =
  [
    ("true", TRUE);
    ("false", FALSE);
    ("let", LET);
    ("rec", REC);
    ("in", IN);
    ("fun", FUN);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("type", TYPE);
    ("of", OF);
    ("and", AND);
    ("as", AS);
  ]

let keyword_or_identifier str =
  match List.assoc_opt str keywords with
  | Some token -> token
  | None -> LOWERCASE_IDENTIFIER str

type state = {
  lexbuf : Sedlexing.lexbuf;
  mutable current_location : Location.t;
}

let create_state filename content =
  let lexbuf = Sedlexing.Utf8.from_string content in
  Sedlexing.set_filename lexbuf filename;
  { lexbuf; current_location = Location.none }

let update_location state =
  let start_pos, end_pos = Sedlexing.lexing_positions state.lexbuf in
  state.current_location <- Location.from_lexing_positions start_pos end_pos

let current_lexeme state = Sedlexing.Utf8.lexeme state.lexbuf

let rec skip_line_comment lexbuf =
  match%sedlex lexbuf with
  | '\n' -> ()
  | eof -> ()
  | any -> skip_line_comment lexbuf
  | _ -> ()

let rec skip_block_comment depth lexbuf loc =
  match%sedlex lexbuf with
  | "*)" ->
    if depth = 0 then ()
    else skip_block_comment (depth - 1) lexbuf loc
  | "(*" -> skip_block_comment (depth + 1) lexbuf loc
  | eof -> Compiler_error.lexer_error loc "Unterminated block comment"
  | any -> skip_block_comment depth lexbuf loc
  | _ -> skip_block_comment depth lexbuf loc

let parse_string lexbuf loc =
  let buffer = Buffer.create 64 in
  let rec read_string () =
    match%sedlex lexbuf with
    | '"' -> Buffer.contents buffer
    | '\\', 'n' ->
      Buffer.add_char buffer '\n';
      read_string ()
    | '\\', 't' ->
      Buffer.add_char buffer '\t';
      read_string ()
    | '\\', 'r' ->
      Buffer.add_char buffer '\r';
      read_string ()
    | '\\', '\\' ->
      Buffer.add_char buffer '\\';
      read_string ()
    | '\\', '"' ->
      Buffer.add_char buffer '"';
      read_string ()
    | eof -> Compiler_error.lexer_error loc "Unterminated string literal"
    | any ->
      Buffer.add_string buffer (Sedlexing.Utf8.lexeme lexbuf);
      read_string ()
    | _ -> Compiler_error.lexer_error loc "Invalid character in string"
  in
  read_string ()

let rec next_token state =
  let lexbuf = state.lexbuf in
  match%sedlex lexbuf with
  | Plus whitespace -> next_token state
  | "--" ->
    skip_line_comment lexbuf;
    next_token state
  | "(*" ->
    update_location state;
    skip_block_comment 0 lexbuf state.current_location;
    next_token state
  | "()" ->
    update_location state;
    (LPAREN, state.current_location)
  | '(' ->
    update_location state;
    (LPAREN, state.current_location)
  | ')' ->
    update_location state;
    (RPAREN, state.current_location)
  | '[' ->
    update_location state;
    (LBRACKET, state.current_location)
  | ']' ->
    update_location state;
    (RBRACKET, state.current_location)
  | ',' ->
    update_location state;
    (COMMA, state.current_location)
  | ';' ->
    update_location state;
    (SEMICOLON, state.current_location)
  | ':' ->
    update_location state;
    (COLON, state.current_location)
  | "->" ->
    update_location state;
    (ARROW, state.current_location)
  | "==" ->
    update_location state;
    (EQUAL_EQUAL, state.current_location)
  | "!=" ->
    update_location state;
    (NOT_EQUAL, state.current_location)
  | "<=" ->
    update_location state;
    (LESS_EQUAL, state.current_location)
  | ">=" ->
    update_location state;
    (GREATER_EQUAL, state.current_location)
  | '=' ->
    update_location state;
    (EQUAL, state.current_location)
  | '|' ->
    update_location state;
    (BAR, state.current_location)
  | '_' ->
    update_location state;
    (UNDERSCORE, state.current_location)
  | '*' ->
    update_location state;
    (STAR, state.current_location)
  | '+' ->
    update_location state;
    (PLUS, state.current_location)
  | '-' ->
    update_location state;
    (MINUS, state.current_location)
  | '/' ->
    update_location state;
    (SLASH, state.current_location)
  | '<' ->
    update_location state;
    (LESS, state.current_location)
  | '>' ->
    update_location state;
    (GREATER, state.current_location)
  | '"' ->
    update_location state;
    let str = parse_string lexbuf state.current_location in
    update_location state;
    (STRING str, state.current_location)
  | '\'', lowercase_letter, Star identifier_char ->
    update_location state;
    let lexeme = current_lexeme state in
    let var_name = String.sub lexeme 1 (String.length lexeme - 1) in
    (TYPE_VARIABLE var_name, state.current_location)
  | lowercase_letter, Star identifier_char ->
    update_location state;
    (keyword_or_identifier (current_lexeme state), state.current_location)
  | uppercase_letter, Star identifier_char ->
    update_location state;
    (UPPERCASE_IDENTIFIER (current_lexeme state), state.current_location)
  | Plus digit, '.', Star digit ->
    update_location state;
    (FLOAT (float_of_string (current_lexeme state)), state.current_location)
  | Plus digit ->
    update_location state;
    (INTEGER (int_of_string (current_lexeme state)), state.current_location)
  | eof ->
    update_location state;
    (EOF, state.current_location)
  | _ ->
    update_location state;
    Compiler_error.lexer_error state.current_location
      (Printf.sprintf "Unexpected character: %s" (current_lexeme state))

let tokenize filename content =
  let state = create_state filename content in
  let rec collect_tokens acc =
    let token, loc = next_token state in
    let acc = (token, loc) :: acc in
    match token with
    | EOF -> List.rev acc
    | _ -> collect_tokens acc
  in
  collect_tokens []
