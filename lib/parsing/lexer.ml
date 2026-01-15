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
  | FUNCTION
  | IF
  | THEN
  | ELSE
  | TYPE
  | OF
  | AND
  | AS
  | MATCH
  | WITH
  | WHEN
  (* Module system tokens *)
  | MODULE
  | STRUCT
  | END
  | SIG
  | FUNCTOR
  | OPEN
  | INCLUDE
  | VAL
  (* Privacy token *)
  | PRIVATE
  (* Constraint token *)
  | CONSTRAINT
  (* FFI tokens *)
  | EXTERNAL
  | AT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | SEMICOLON
  | COLON
  | DOT
  | DOTDOT
  | ARROW
  | EQUAL
  | BAR
  | UNDERSCORE
  | STAR
  | PLUS
  | MINUS
  | SLASH
  | CARET                (** ^ string concatenation *)
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | EQUAL_EQUAL
  | NOT_EQUAL
  (* Reference operations *)
  | REF            (** ref keyword for creating references *)
  | BANG           (** ! for dereference *)
  | COLONEQUALS    (** := for assignment *)
  | COLONCOLON     (** :: for list cons *)
  (* Polymorphic variants *)
  | BACKTICK_TAG of string  (** `` `Tag `` for polymorphic variant constructors *)
  (* Labeled arguments *)
  | TILDE          (** ~ for labeled arguments *)
  | QUESTION       (** ? for optional arguments *)
  (* Extensible variants *)
  | PLUSEQUAL      (** += for type extension *)
  | EOF
[@@deriving show, eq]

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

(** Keyword lookup table for O(1) keyword identification instead of O(n) list search. *)
let keywords_table : (string, token) Hashtbl.t =
  let table = Hashtbl.create 32 in
  List.iter (fun (keyword, token) -> Hashtbl.add table keyword token)
    [
      ("true", TRUE);
      ("false", FALSE);
      ("let", LET);
      ("rec", REC);
      ("in", IN);
      ("fun", FUN);
      ("function", FUNCTION);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("type", TYPE);
      ("of", OF);
      ("and", AND);
      ("as", AS);
      ("match", MATCH);
      ("with", WITH);
      ("when", WHEN);
      (* Module system keywords *)
      ("module", MODULE);
      ("struct", STRUCT);
      ("end", END);
      ("sig", SIG);
      ("functor", FUNCTOR);
      ("open", OPEN);
      ("include", INCLUDE);
      ("val", VAL);
      (* Privacy keyword *)
      ("private", PRIVATE);
      (* Constraint keyword *)
      ("constraint", CONSTRAINT);
      (* FFI keywords *)
      ("external", EXTERNAL);
      (* Reference keyword *)
      ("ref", REF);
    ];
  table

let keyword_or_identifier str =
  match Hashtbl.find_opt keywords_table str with
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

(** {1 Token Recognition}

    The core token matching logic is in [lex_real_token]. This function
    handles actual token recognition for both simple tokenization and
    trivia-aware lexing. *)

(** Create a token result, updating location first. *)
let make_token tok state =
  update_location state;
  Some (tok, state.current_location)

(** Lex the next real token (not whitespace or comment).
    Returns [None] if the current position doesn't match any token. *)
let lex_real_token state =
  let lexbuf = state.lexbuf in
  match%sedlex lexbuf with
  (* Delimiters *)
  | '(' -> make_token LPAREN state
  | ')' -> make_token RPAREN state
  | '[' -> make_token LBRACKET state
  | ']' -> make_token RBRACKET state
  | '{' -> make_token LBRACE state
  | '}' -> make_token RBRACE state
  (* Punctuation - multi-char first for longest match *)
  | ".." -> make_token DOTDOT state
  | '.' -> make_token DOT state
  | ',' -> make_token COMMA state
  | ';' -> make_token SEMICOLON state
  | ":=" -> make_token COLONEQUALS state
  | "::" -> make_token COLONCOLON state
  | ':' -> make_token COLON state
  | "->" -> make_token ARROW state
  (* Comparison operators - multi-char first *)
  | "==" -> make_token EQUAL_EQUAL state
  | "!=" -> make_token NOT_EQUAL state
  | "<>" -> make_token NOT_EQUAL state
  | '!' -> make_token BANG state
  | "<=" -> make_token LESS_EQUAL state
  | ">=" -> make_token GREATER_EQUAL state
  | '=' -> make_token EQUAL state
  | '<' -> make_token LESS state
  | '>' -> make_token GREATER state
  (* Other operators and symbols *)
  | '|' -> make_token BAR state
  | '@' -> make_token AT state
  | '_' -> make_token UNDERSCORE state
  | '*' -> make_token STAR state
  | '+' -> make_token PLUS state
  | '-' -> make_token MINUS state
  | '/' -> make_token SLASH state
  | '^' -> make_token CARET state
  | '~' -> make_token TILDE state
  | '?' -> make_token QUESTION state
  | "+=" -> make_token PLUSEQUAL state
  (* String literals *)
  | '"' ->
      update_location state;
      let str = parse_string lexbuf state.current_location in
      update_location state;
      Some (STRING str, state.current_location)
  (* Type variables: 'a, 'foo *)
  | '\'', lowercase_letter, Star identifier_char ->
      update_location state;
      let lexeme = current_lexeme state in
      let var_name = String.sub lexeme 1 (String.length lexeme - 1) in
      Some (TYPE_VARIABLE var_name, state.current_location)
  (* Polymorphic variant tags: `Some, `None, `A *)
  | '`', (lowercase_letter | uppercase_letter), Star identifier_char ->
      update_location state;
      let lexeme = current_lexeme state in
      let tag_name = String.sub lexeme 1 (String.length lexeme - 1) in
      Some (BACKTICK_TAG tag_name, state.current_location)
  (* Identifiers and keywords *)
  | lowercase_letter, Star identifier_char ->
      update_location state;
      Some (keyword_or_identifier (current_lexeme state), state.current_location)
  | uppercase_letter, Star identifier_char ->
      update_location state;
      Some (UPPERCASE_IDENTIFIER (current_lexeme state), state.current_location)
  (* Hex integers: 0x1A, 0XFF_FF *)
  | '0', ('x' | 'X'), Plus hex_digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
  (* Binary integers: 0b1010, 0B1111_0000 *)
  | '0', ('b' | 'B'), Plus binary_digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
  (* Float with decimal and exponent: 3.14e10, 1.5E-3 *)
  | Plus digit_with_underscore, '.', Star digit_with_underscore, exponent ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with exponent only (no decimal): 1e10, 1_000E5 *)
  | Plus digit_with_underscore, exponent ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with decimal: 3.14, 1_000.50 *)
  | Plus digit_with_underscore, '.', Plus digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Float with trailing dot: 3. (backwards compatibility) *)
  | Plus digit_with_underscore, '.', Star digit ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (FLOAT (parse_float_literal lexeme state.current_location), state.current_location)
  (* Decimal integer with optional underscores: 123, 1_000_000 *)
  | Plus digit_with_underscore ->
      update_location state;
      let lexeme = current_lexeme state in
      Some (INTEGER (parse_int_literal lexeme state.current_location), state.current_location)
  | eof -> make_token EOF state
  | _ -> None

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
  | _ ->
      (* Roll back and try to match a real token *)
      Sedlexing.rollback lexbuf;
      match lex_real_token state with
      | Some result -> result
      | None ->
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

(** {1 Trivia-Aware Lexing}

    The following functions support lexing with trivia preservation,
    used by the formatter and other tools that need to preserve comments. *)

(** Collect a line comment, returning the full text including [--] prefix.
    The newline character is NOT consumed - it is left for the trivia collector
    to pick up as a separate Newline trivia piece. *)
let collect_line_comment lexbuf =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "--";
  let rec read () =
    match%sedlex lexbuf with
    | '\n' ->
        (* Put the newline back so it can be collected as separate trivia *)
        Sedlexing.rollback lexbuf;
        Buffer.contents buffer
    | eof -> Buffer.contents buffer
    | any ->
        Buffer.add_string buffer (Sedlexing.Utf8.lexeme lexbuf);
        read ()
    | _ -> Buffer.contents buffer
  in
  read ()

(** Collect a block comment, returning the full text including delimiters.
    Handles nested comments. *)
let collect_block_comment lexbuf loc =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "(*";
  let rec read depth =
    match%sedlex lexbuf with
    | "*)" ->
        Buffer.add_string buffer "*)";
        if depth = 0 then Buffer.contents buffer
        else read (depth - 1)
    | "(*" ->
        Buffer.add_string buffer "(*";
        read (depth + 1)
    | eof -> Compiler_error.lexer_error loc "Unterminated block comment"
    | any ->
        Buffer.add_string buffer (Sedlexing.Utf8.lexeme lexbuf);
        read depth
    | _ ->
        Buffer.add_string buffer (Sedlexing.Utf8.lexeme lexbuf);
        read depth
  in
  read 0

(** A token with its attached trivia and location.

    The trivia represents whitespace and comments surrounding the token:
    - [leading] contains trivia that appears before this token
    - [trailing] contains trivia that appears after this token on the same line *)
type token_with_trivia = {
  token : token;
  location : Location.t;
  trivia : Trivia.t;
}
[@@deriving show, eq]

(** State for trivia-aware lexing. *)
type trivia_state = {
  state : state;
}

(** Create a trivia-aware lexer state. *)
let create_trivia_state filename content =
  { state = create_state filename content }

(** Make a trivia piece with the current location. *)
let make_trivia_piece state kind =
  update_location state;
  { Trivia.kind; location = state.current_location }

(** Collect a line comment and create a trivia piece. *)
let make_line_comment_piece lexbuf =
  let start_pos, _ = Sedlexing.lexing_positions lexbuf in
  let comment_text = collect_line_comment lexbuf in
  let _, end_pos = Sedlexing.lexing_positions lexbuf in
  let location = Location.from_lexing_positions start_pos end_pos in
  { Trivia.kind = Trivia.LineComment comment_text; location }

(** Collect a block comment and create a trivia piece. *)
let make_block_comment_piece state lexbuf =
  let start_pos, _ = Sedlexing.lexing_positions lexbuf in
  update_location state;
  let comment_text = collect_block_comment lexbuf state.current_location in
  let _, end_pos = Sedlexing.lexing_positions lexbuf in
  let location = Location.from_lexing_positions start_pos end_pos in
  { Trivia.kind = Trivia.BlockComment comment_text; location }

(** Collect all trivia (whitespace and comments) until a real token.
    Returns the trivia pieces in order. *)
let rec collect_leading_trivia trivia_state acc =
  let state = trivia_state.state in
  let lexbuf = state.lexbuf in
  match%sedlex lexbuf with
  | Plus (' ' | '\t') ->
      let piece = make_trivia_piece state (Trivia.Whitespace (Sedlexing.Utf8.lexeme lexbuf)) in
      collect_leading_trivia trivia_state (piece :: acc)
  | '\n' | '\r' | "\r\n" ->
      let piece = make_trivia_piece state Trivia.Newline in
      collect_leading_trivia trivia_state (piece :: acc)
  | "--" ->
      let piece = make_line_comment_piece lexbuf in
      collect_leading_trivia trivia_state (piece :: acc)
  | "(*" ->
      let piece = make_block_comment_piece state lexbuf in
      collect_leading_trivia trivia_state (piece :: acc)
  | _ -> List.rev acc

(** Collect trailing trivia on the same line (whitespace and comments until newline). *)
let rec collect_trailing_trivia trivia_state acc =
  let state = trivia_state.state in
  let lexbuf = state.lexbuf in
  match%sedlex lexbuf with
  | Plus (' ' | '\t') ->
      let piece = make_trivia_piece state (Trivia.Whitespace (Sedlexing.Utf8.lexeme lexbuf)) in
      collect_trailing_trivia trivia_state (piece :: acc)
  | "--" ->
      let piece = make_line_comment_piece lexbuf in
      (* Line comment ends the line, so we stop collecting trailing trivia *)
      List.rev (piece :: acc)
  | "(*" ->
      let piece = make_block_comment_piece state lexbuf in
      collect_trailing_trivia trivia_state (piece :: acc)
  | _ ->
      (* Hit newline, EOF, or a real token - stop collecting *)
      List.rev acc

(** Get the next token with attached trivia.

    Collects leading trivia (whitespace and comments before the token),
    lexes the token, then collects trailing trivia (on the same line). *)
let next_token_with_trivia trivia_state =
  let state = trivia_state.state in
  let leading = collect_leading_trivia trivia_state [] in

  match lex_real_token state with
  | Some (token, location) ->
      let trailing = collect_trailing_trivia trivia_state [] in
      let trivia = Trivia.create ~leading ~trailing in
      { token; location; trivia }
  | None ->
      update_location state;
      Compiler_error.lexer_error state.current_location
        (Printf.sprintf "Unexpected character: %s" (current_lexeme state))

(** Tokenize an entire file with trivia preservation.

    Returns a list of tokens where each token has its attached leading
    and trailing trivia. This is used by the formatter. *)
let tokenize_with_trivia filename content =
  let trivia_state = create_trivia_state filename content in
  let rec collect_tokens acc =
    let tok = next_token_with_trivia trivia_state in
    let acc = tok :: acc in
    match tok.token with
    | EOF -> List.rev acc
    | _ -> collect_tokens acc
  in
  collect_tokens []
