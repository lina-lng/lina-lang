(** CST lexer: wraps the existing lexer to produce green tokens.

    This module bridges the parsing lexer and the CST infrastructure by
    converting lexer tokens and trivia into green tree tokens suitable
    for building the concrete syntax tree.

    The lexer preserves all source text including whitespace and comments
    as trivia attached to tokens. *)

open Parsing

(** {1 Token Conversion} *)

(** [syntax_kind_of_token token] converts a lexer token to its CST syntax kind. *)
let syntax_kind_of_token : Lexer.token -> Syntax_kind.t = function
  | Lexer.INTEGER _ -> Syntax_kind.TK_INTEGER
  | Lexer.FLOAT _ -> Syntax_kind.TK_FLOAT
  | Lexer.STRING _ -> Syntax_kind.TK_STRING
  | Lexer.LOWERCASE_IDENTIFIER _ -> Syntax_kind.TK_LOWERCASE_IDENT
  | Lexer.UPPERCASE_IDENTIFIER _ -> Syntax_kind.TK_UPPERCASE_IDENT
  | Lexer.TYPE_VARIABLE _ -> Syntax_kind.TK_TYPE_VARIABLE
  | Lexer.TRUE -> Syntax_kind.TK_TRUE
  | Lexer.FALSE -> Syntax_kind.TK_FALSE
  | Lexer.LET -> Syntax_kind.TK_LET
  | Lexer.REC -> Syntax_kind.TK_REC
  | Lexer.IN -> Syntax_kind.TK_IN
  | Lexer.FUN -> Syntax_kind.TK_FUN
  | Lexer.IF -> Syntax_kind.TK_IF
  | Lexer.THEN -> Syntax_kind.TK_THEN
  | Lexer.ELSE -> Syntax_kind.TK_ELSE
  | Lexer.TYPE -> Syntax_kind.TK_TYPE
  | Lexer.OF -> Syntax_kind.TK_OF
  | Lexer.AND -> Syntax_kind.TK_AND
  | Lexer.AS -> Syntax_kind.TK_AS
  | Lexer.MATCH -> Syntax_kind.TK_MATCH
  | Lexer.WITH -> Syntax_kind.TK_WITH
  | Lexer.WHEN -> Syntax_kind.TK_WHEN
  | Lexer.MODULE -> Syntax_kind.TK_MODULE
  | Lexer.STRUCT -> Syntax_kind.TK_STRUCT
  | Lexer.END -> Syntax_kind.TK_END
  | Lexer.SIG -> Syntax_kind.TK_SIG
  | Lexer.FUNCTOR -> Syntax_kind.TK_FUNCTOR
  | Lexer.OPEN -> Syntax_kind.TK_OPEN
  | Lexer.INCLUDE -> Syntax_kind.TK_INCLUDE
  | Lexer.VAL -> Syntax_kind.TK_VAL
  | Lexer.EXTERNAL -> Syntax_kind.TK_EXTERNAL
  | Lexer.AT -> Syntax_kind.TK_AT
  | Lexer.LPAREN -> Syntax_kind.TK_LPAREN
  | Lexer.RPAREN -> Syntax_kind.TK_RPAREN
  | Lexer.LBRACKET -> Syntax_kind.TK_LBRACKET
  | Lexer.RBRACKET -> Syntax_kind.TK_RBRACKET
  | Lexer.LBRACE -> Syntax_kind.TK_LBRACE
  | Lexer.RBRACE -> Syntax_kind.TK_RBRACE
  | Lexer.COMMA -> Syntax_kind.TK_COMMA
  | Lexer.SEMICOLON -> Syntax_kind.TK_SEMICOLON
  | Lexer.COLON -> Syntax_kind.TK_COLON
  | Lexer.DOT -> Syntax_kind.TK_DOT
  | Lexer.DOTDOT -> Syntax_kind.TK_DOTDOT
  | Lexer.ARROW -> Syntax_kind.TK_ARROW
  | Lexer.EQUAL -> Syntax_kind.TK_EQUAL
  | Lexer.BAR -> Syntax_kind.TK_BAR
  | Lexer.UNDERSCORE -> Syntax_kind.TK_UNDERSCORE
  | Lexer.STAR -> Syntax_kind.TK_STAR
  | Lexer.PLUS -> Syntax_kind.TK_PLUS
  | Lexer.MINUS -> Syntax_kind.TK_MINUS
  | Lexer.SLASH -> Syntax_kind.TK_SLASH
  | Lexer.LESS -> Syntax_kind.TK_LESS
  | Lexer.GREATER -> Syntax_kind.TK_GREATER
  | Lexer.LESS_EQUAL -> Syntax_kind.TK_LESS_EQUAL
  | Lexer.GREATER_EQUAL -> Syntax_kind.TK_GREATER_EQUAL
  | Lexer.EQUAL_EQUAL -> Syntax_kind.TK_EQUAL_EQUAL
  | Lexer.NOT_EQUAL -> Syntax_kind.TK_NOT_EQUAL
  | Lexer.EOF -> Syntax_kind.TK_EOF

(** [token_text token] returns the source text representation of a token. *)
let token_text : Lexer.token -> string = function
  | Lexer.INTEGER n -> string_of_int n
  | Lexer.FLOAT f -> string_of_float f
  | Lexer.STRING s -> "\"" ^ String.escaped s ^ "\""
  | Lexer.LOWERCASE_IDENTIFIER s -> s
  | Lexer.UPPERCASE_IDENTIFIER s -> s
  | Lexer.TYPE_VARIABLE s -> "'" ^ s
  | Lexer.TRUE -> "true"
  | Lexer.FALSE -> "false"
  | Lexer.LET -> "let"
  | Lexer.REC -> "rec"
  | Lexer.IN -> "in"
  | Lexer.FUN -> "fun"
  | Lexer.IF -> "if"
  | Lexer.THEN -> "then"
  | Lexer.ELSE -> "else"
  | Lexer.TYPE -> "type"
  | Lexer.OF -> "of"
  | Lexer.AND -> "and"
  | Lexer.AS -> "as"
  | Lexer.MATCH -> "match"
  | Lexer.WITH -> "with"
  | Lexer.WHEN -> "when"
  | Lexer.MODULE -> "module"
  | Lexer.STRUCT -> "struct"
  | Lexer.END -> "end"
  | Lexer.SIG -> "sig"
  | Lexer.FUNCTOR -> "functor"
  | Lexer.OPEN -> "open"
  | Lexer.INCLUDE -> "include"
  | Lexer.VAL -> "val"
  | Lexer.EXTERNAL -> "external"
  | Lexer.AT -> "@"
  | Lexer.LPAREN -> "("
  | Lexer.RPAREN -> ")"
  | Lexer.LBRACKET -> "["
  | Lexer.RBRACKET -> "]"
  | Lexer.LBRACE -> "{"
  | Lexer.RBRACE -> "}"
  | Lexer.COMMA -> ","
  | Lexer.SEMICOLON -> ";"
  | Lexer.COLON -> ":"
  | Lexer.DOT -> "."
  | Lexer.DOTDOT -> ".."
  | Lexer.ARROW -> "->"
  | Lexer.EQUAL -> "="
  | Lexer.BAR -> "|"
  | Lexer.UNDERSCORE -> "_"
  | Lexer.STAR -> "*"
  | Lexer.PLUS -> "+"
  | Lexer.MINUS -> "-"
  | Lexer.SLASH -> "/"
  | Lexer.LESS -> "<"
  | Lexer.GREATER -> ">"
  | Lexer.LESS_EQUAL -> "<="
  | Lexer.GREATER_EQUAL -> ">="
  | Lexer.EQUAL_EQUAL -> "=="
  | Lexer.NOT_EQUAL -> "!="
  | Lexer.EOF -> ""

(** {1 Trivia Conversion} *)

(** [convert_trivia_kind kind] converts a parsing trivia kind to CST syntax kind. *)
let convert_trivia_kind : Trivia.trivia_kind -> Syntax_kind.t * string = function
  | Trivia.Whitespace s -> (Syntax_kind.TK_WHITESPACE, s)
  | Trivia.Newline -> (Syntax_kind.TK_NEWLINE, "\n")
  | Trivia.LineComment s -> (Syntax_kind.TK_LINE_COMMENT, s)
  | Trivia.BlockComment s -> (Syntax_kind.TK_BLOCK_COMMENT, s)

(** [convert_trivia_piece piece] converts a parsing trivia piece to a green trivia piece. *)
let convert_trivia_piece (piece : Trivia.trivia_piece) : Green_tree.trivia_piece =
  let kind, text = convert_trivia_kind piece.kind in
  { Green_tree.trivia_kind = kind; trivia_text = text }

(** [convert_trivia_list pieces] converts a list of trivia pieces. *)
let convert_trivia_list pieces = List.map convert_trivia_piece pieces

(** {1 CST Token Type} *)

(** A CST token with position information for source mapping. *)
type cst_token = {
  green : Green_tree.green_token;  (** The green token with trivia *)
  location : Common.Location.t;  (** Source location *)
}

(** {1 Tokenization} *)

(** [convert_token_with_trivia twt] converts a lexer token with trivia to a CST token. *)
let convert_token_with_trivia (twt : Lexer.token_with_trivia) : cst_token =
  let kind = syntax_kind_of_token twt.token in
  let text = token_text twt.token in
  let leading = convert_trivia_list twt.trivia.leading in
  let trailing = convert_trivia_list twt.trivia.trailing in
  let green = Green_tree.make_token_with_trivia kind text ~leading ~trailing in
  { green; location = twt.location }

(** [tokenize filename content] tokenizes source code into CST tokens.

    This is the main entry point for CST tokenization. It produces a list
    of green tokens with all trivia preserved.

    @param filename The name of the source file (for error messages)
    @param content The source code to tokenize
    @return A list of CST tokens with position information *)
let tokenize filename content =
  let tokens_with_trivia = Lexer.tokenize_with_trivia filename content in
  List.map convert_token_with_trivia tokens_with_trivia

(** {1 Streaming Interface} *)

(** Lexer state for incremental tokenization. *)
type state = {
  trivia_state : Lexer.trivia_state;
  mutable peeked : cst_token option;
}

(** [create_state filename content] creates a new CST lexer state. *)
let create_state filename content =
  { trivia_state = Lexer.create_trivia_state filename content; peeked = None }

(** [next_token state] returns the next CST token.

    Advances the lexer and returns the next token with its trivia. *)
let next_token state =
  match state.peeked with
  | Some token ->
      state.peeked <- None;
      token
  | None ->
      let twt = Lexer.next_token_with_trivia state.trivia_state in
      convert_token_with_trivia twt

(** [peek_token state] returns the next token without consuming it. *)
let peek_token state =
  match state.peeked with
  | Some token -> token
  | None ->
      let token = next_token state in
      state.peeked <- Some token;
      token

(** [is_eof state] returns true if the next token is EOF. *)
let is_eof state =
  let token = peek_token state in
  Syntax_kind.equal token.green.kind Syntax_kind.TK_EOF

(** {1 Utilities} *)

(** [green_tokens tokens] extracts just the green tokens from CST tokens. *)
let green_tokens tokens = List.map (fun t -> t.green) tokens

(** [all_green_children tokens] wraps green tokens as green children for node construction. *)
let all_green_children tokens =
  List.map (fun t -> Green_tree.GreenToken t.green) tokens
