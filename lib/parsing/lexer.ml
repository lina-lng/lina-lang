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

(* Extended number literal patterns *)
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let binary_digit = [%sedlex.regexp? '0' | '1']
let digit_with_underscore = [%sedlex.regexp? digit | '_']
let hex_digit_with_underscore = [%sedlex.regexp? hex_digit | '_']
let binary_digit_with_underscore = [%sedlex.regexp? binary_digit | '_']
let exponent = [%sedlex.regexp? ('e' | 'E'), Opt ('+' | '-'), Plus digit_with_underscore]

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

(* Helper functions for string escapes *)

let hex_value c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let parse_hex_byte lexeme loc =
  if String.length lexeme < 4 then
    Compiler_error.lexer_error loc "Incomplete hex escape \\xHH"
  else
    let h1 = hex_value lexeme.[2] in
    let h2 = hex_value lexeme.[3] in
    if h1 < 0 || h2 < 0 then
      Compiler_error.lexer_error loc
        (Printf.sprintf "Invalid hex escape: %s" lexeme)
    else Char.chr (h1 * 16 + h2)

let encode_utf8 codepoint =
  if codepoint < 0 then None
  else if codepoint <= 0x7F then Some (String.make 1 (Char.chr codepoint))
  else if codepoint <= 0x7FF then
    let b1 = 0xC0 lor (codepoint lsr 6) in
    let b2 = 0x80 lor (codepoint land 0x3F) in
    Some (String.init 2 (fun i -> Char.chr (if i = 0 then b1 else b2)))
  else if codepoint <= 0xFFFF then
    let b1 = 0xE0 lor (codepoint lsr 12) in
    let b2 = 0x80 lor ((codepoint lsr 6) land 0x3F) in
    let b3 = 0x80 lor (codepoint land 0x3F) in
    Some
      (String.init 3 (fun i ->
           Char.chr (match i with 0 -> b1 | 1 -> b2 | _ -> b3)))
  else if codepoint <= 0x10FFFF then
    let b1 = 0xF0 lor (codepoint lsr 18) in
    let b2 = 0x80 lor ((codepoint lsr 12) land 0x3F) in
    let b3 = 0x80 lor ((codepoint lsr 6) land 0x3F) in
    let b4 = 0x80 lor (codepoint land 0x3F) in
    Some
      (String.init 4 (fun i ->
           Char.chr (match i with 0 -> b1 | 1 -> b2 | 2 -> b3 | _ -> b4)))
  else None

let parse_unicode_escape lexeme loc =
  let len = String.length lexeme in
  if len < 5 then Compiler_error.lexer_error loc "Invalid Unicode escape"
  else
    (* lexeme is "\u{HHHH}" - extract hex part between { and } *)
    let hex_start = 3 in
    let hex_end = len - 1 in
    if hex_end <= hex_start then
      Compiler_error.lexer_error loc "Empty Unicode escape \\u{}"
    else
      let hex_str = String.sub lexeme hex_start (hex_end - hex_start) in
      try
        let codepoint = int_of_string ("0x" ^ hex_str) in
        match encode_utf8 codepoint with
        | Some s -> s
        | None ->
            Compiler_error.lexer_error loc
              (Printf.sprintf "Invalid Unicode codepoint: U+%s" hex_str)
      with Failure _ ->
        Compiler_error.lexer_error loc
          (Printf.sprintf "Invalid hex in Unicode escape: \\u{%s}" hex_str)

(* Helper functions for number literals *)

let strip_underscores s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> if c <> '_' then Buffer.add_char buf c) s;
  Buffer.contents buf

let validate_underscores s loc =
  let len = String.length s in
  if len = 0 then ()
  else (
    (* Determine start position (skip prefix like 0x or 0b) *)
    let check_start =
      if
        len > 2 && s.[0] = '0'
        && (s.[1] = 'x' || s.[1] = 'X' || s.[1] = 'b' || s.[1] = 'B')
      then 2
      else 0
    in
    if check_start < len && s.[check_start] = '_' then
      Compiler_error.lexer_error loc
        "Number literal cannot have leading underscore after prefix";
    if s.[len - 1] = '_' then
      Compiler_error.lexer_error loc
        "Number literal cannot have trailing underscore";
    let rec check_consecutive i =
      if i >= len - 1 then ()
      else if s.[i] = '_' && s.[i + 1] = '_' then
        Compiler_error.lexer_error loc
          "Number literal cannot have consecutive underscores"
      else check_consecutive (i + 1)
    in
    check_consecutive check_start)

let parse_int_literal s loc =
  validate_underscores s loc;
  let clean = strip_underscores s in
  try int_of_string clean
  with Failure _ ->
    Compiler_error.lexer_error loc
      (Printf.sprintf "Invalid integer literal: %s" s)

let parse_float_literal s loc =
  validate_underscores s loc;
  let clean = strip_underscores s in
  try float_of_string clean
  with Failure _ ->
    Compiler_error.lexer_error loc
      (Printf.sprintf "Invalid float literal: %s" s)

let parse_string lexbuf loc =
  let buffer = Buffer.create 64 in
  let rec read_string () =
    match%sedlex lexbuf with
    | '"' -> Buffer.contents buffer
    (* Simple escapes *)
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
    (* Null character *)
    | '\\', '0' ->
        Buffer.add_char buffer '\x00';
        read_string ()
    (* Hex byte escape \xHH *)
    | '\\', 'x', hex_digit, hex_digit ->
        let lexeme = Sedlexing.Utf8.lexeme lexbuf in
        let c = parse_hex_byte lexeme loc in
        Buffer.add_char buffer c;
        read_string ()
    (* Unicode escape \u{H} to \u{HHHHHH} *)
    | '\\', 'u', '{', Plus hex_digit, '}' ->
        let lexeme = Sedlexing.Utf8.lexeme lexbuf in
        let utf8_str = parse_unicode_escape lexeme loc in
        Buffer.add_string buffer utf8_str;
        read_string ()
    (* Error cases for malformed escapes *)
    | '\\', 'x' ->
        Compiler_error.lexer_error loc
          "Invalid hex escape: expected \\xHH (two hex digits)"
    | '\\', 'u', '{' ->
        Compiler_error.lexer_error loc
          "Invalid Unicode escape: expected \\u{HHHH} (1-6 hex digits)"
    | '\\', 'u' ->
        Compiler_error.lexer_error loc
          "Invalid Unicode escape: expected \\u{HHHH}"
    | '\\', any ->
        let lexeme = Sedlexing.Utf8.lexeme lexbuf in
        Compiler_error.lexer_error loc
          (Printf.sprintf "Unknown escape sequence: %s" lexeme)
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
  (* Hex integers: 0x1A, 0XFF_FF *)
  | '0', ('x' | 'X'), Plus hex_digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
  (* Binary integers: 0b1010, 0B1111_0000 *)
  | '0', ('b' | 'B'), Plus binary_digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
  (* Float with decimal and exponent: 3.14e10, 1.5E-3 *)
  | Plus digit_with_underscore, '.', Star digit_with_underscore, exponent ->
      update_location state;
      let lexeme = current_lexeme state in
      (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with exponent only (no decimal): 1e10, 1_000E5 *)
  | Plus digit_with_underscore, exponent ->
      update_location state;
      let lexeme = current_lexeme state in
      (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with decimal: 3.14, 1_000.50 *)
  | Plus digit_with_underscore, '.', Plus digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with trailing dot: 3. (backwards compatibility) *)
  | Plus digit_with_underscore, '.', Star digit ->
      update_location state;
      let lexeme = current_lexeme state in
      (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Decimal integer with optional underscores: 123, 1_000_000 *)
  | Plus digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
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
