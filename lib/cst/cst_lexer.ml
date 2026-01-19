(** CST lexer: wraps the existing lexer to produce green tokens.

    This module bridges the parsing lexer and the CST infrastructure by
    converting lexer tokens and trivia into green tree tokens suitable
    for building the concrete syntax tree.

    The lexer preserves all source text including whitespace and comments
    as trivia attached to tokens. *)

open Parsing

(** {1 Token Conversion} *)

(** [token_metadata token] returns the syntax kind and text for a token.
    This is the single source of truth for token-to-CST conversion,
    avoiding duplicate pattern matching. *)
let token_metadata : Lexer.token -> Syntax_kind.t * string = function
  (* Literals with variable text *)
  | Lexer.INTEGER n -> (Syntax_kind.TK_INTEGER, string_of_int n)
  | Lexer.FLOAT f -> (Syntax_kind.TK_FLOAT, string_of_float f)
  | Lexer.STRING s -> (Syntax_kind.TK_STRING, "\"" ^ String.escaped s ^ "\"")
  | Lexer.LOWERCASE_IDENTIFIER s -> (Syntax_kind.TK_LOWERCASE_IDENT, s)
  | Lexer.UPPERCASE_IDENTIFIER s -> (Syntax_kind.TK_UPPERCASE_IDENT, s)
  | Lexer.TYPE_VARIABLE s -> (Syntax_kind.TK_TYPE_VARIABLE, "'" ^ s)
  | Lexer.BACKTICK_TAG s -> (Syntax_kind.TK_BACKTICK_TAG, "`" ^ s)
  (* Keywords *)
  | Lexer.TRUE -> (Syntax_kind.TK_TRUE, "true")
  | Lexer.FALSE -> (Syntax_kind.TK_FALSE, "false")
  | Lexer.LET -> (Syntax_kind.TK_LET, "let")
  | Lexer.REC -> (Syntax_kind.TK_REC, "rec")
  | Lexer.IN -> (Syntax_kind.TK_IN, "in")
  | Lexer.FUN -> (Syntax_kind.TK_FUN, "fun")
  | Lexer.FUNCTION -> (Syntax_kind.TK_FUNCTION, "function")
  | Lexer.IF -> (Syntax_kind.TK_IF, "if")
  | Lexer.THEN -> (Syntax_kind.TK_THEN, "then")
  | Lexer.ELSE -> (Syntax_kind.TK_ELSE, "else")
  | Lexer.TYPE -> (Syntax_kind.TK_TYPE, "type")
  | Lexer.OF -> (Syntax_kind.TK_OF, "of")
  | Lexer.AND -> (Syntax_kind.TK_AND, "and")
  | Lexer.AS -> (Syntax_kind.TK_AS, "as")
  | Lexer.MATCH -> (Syntax_kind.TK_MATCH, "match")
  | Lexer.WITH -> (Syntax_kind.TK_WITH, "with")
  | Lexer.WHEN -> (Syntax_kind.TK_WHEN, "when")
  (* Module keywords *)
  | Lexer.MODULE -> (Syntax_kind.TK_MODULE, "module")
  | Lexer.STRUCT -> (Syntax_kind.TK_STRUCT, "struct")
  | Lexer.END -> (Syntax_kind.TK_END, "end")
  | Lexer.SIG -> (Syntax_kind.TK_SIG, "sig")
  | Lexer.FUNCTOR -> (Syntax_kind.TK_FUNCTOR, "functor")
  | Lexer.OPEN -> (Syntax_kind.TK_OPEN, "open")
  | Lexer.INCLUDE -> (Syntax_kind.TK_INCLUDE, "include")
  | Lexer.VAL -> (Syntax_kind.TK_VAL, "val")
  | Lexer.PRIVATE -> (Syntax_kind.TK_PRIVATE, "private")
  | Lexer.CONSTRAINT -> (Syntax_kind.TK_CONSTRAINT, "constraint")
  | Lexer.EXTERNAL -> (Syntax_kind.TK_EXTERNAL, "external")
  (* Punctuation *)
  | Lexer.AT -> (Syntax_kind.TK_AT, "@")
  | Lexer.LPAREN -> (Syntax_kind.TK_LPAREN, "(")
  | Lexer.RPAREN -> (Syntax_kind.TK_RPAREN, ")")
  | Lexer.LBRACKET -> (Syntax_kind.TK_LBRACKET, "[")
  | Lexer.RBRACKET -> (Syntax_kind.TK_RBRACKET, "]")
  | Lexer.LBRACE -> (Syntax_kind.TK_LBRACE, "{")
  | Lexer.RBRACE -> (Syntax_kind.TK_RBRACE, "}")
  | Lexer.COMMA -> (Syntax_kind.TK_COMMA, ",")
  | Lexer.SEMICOLON -> (Syntax_kind.TK_SEMICOLON, ";")
  | Lexer.COLON -> (Syntax_kind.TK_COLON, ":")
  | Lexer.DOT -> (Syntax_kind.TK_DOT, ".")
  | Lexer.DOTDOT -> (Syntax_kind.TK_DOTDOT, "..")
  | Lexer.ARROW -> (Syntax_kind.TK_ARROW, "->")
  | Lexer.EQUAL -> (Syntax_kind.TK_EQUAL, "=")
  | Lexer.BAR -> (Syntax_kind.TK_BAR, "|")
  | Lexer.UNDERSCORE -> (Syntax_kind.TK_UNDERSCORE, "_")
  (* Operators *)
  | Lexer.STAR -> (Syntax_kind.TK_STAR, "*")
  | Lexer.PLUS -> (Syntax_kind.TK_PLUS, "+")
  | Lexer.MINUS -> (Syntax_kind.TK_MINUS, "-")
  | Lexer.SLASH -> (Syntax_kind.TK_SLASH, "/")
  | Lexer.MOD -> (Syntax_kind.TK_MOD, "mod")
  | Lexer.CARET -> (Syntax_kind.TK_CARET, "^")
  | Lexer.LESS -> (Syntax_kind.TK_LESS, "<")
  | Lexer.GREATER -> (Syntax_kind.TK_GREATER, ">")
  | Lexer.LESS_EQUAL -> (Syntax_kind.TK_LESS_EQUAL, "<=")
  | Lexer.GREATER_EQUAL -> (Syntax_kind.TK_GREATER_EQUAL, ">=")
  | Lexer.EQUAL_EQUAL -> (Syntax_kind.TK_EQUAL_EQUAL, "==")
  | Lexer.NOT_EQUAL -> (Syntax_kind.TK_NOT_EQUAL, "!=")
  (* Reference operations *)
  | Lexer.REF -> (Syntax_kind.TK_REF, "ref")
  | Lexer.BANG -> (Syntax_kind.TK_BANG, "!")
  | Lexer.COLONEQUALS -> (Syntax_kind.TK_COLONEQUALS, ":=")
  | Lexer.COLONCOLON -> (Syntax_kind.TK_COLONCOLON, "::")
  (* Labeled arguments *)
  | Lexer.TILDE -> (Syntax_kind.TK_TILDE, "~")
  | Lexer.QUESTION -> (Syntax_kind.TK_QUESTION, "?")
  (* Extensible variants *)
  | Lexer.PLUSEQUAL -> (Syntax_kind.TK_PLUSEQUAL, "+=")
  | Lexer.ASSERT -> (Syntax_kind.TK_ASSERT, "assert")
  | Lexer.WHILE -> (Syntax_kind.TK_WHILE, "while")
  | Lexer.DO -> (Syntax_kind.TK_DO, "do")
  | Lexer.DONE -> (Syntax_kind.TK_DONE, "done")
  | Lexer.FOR -> (Syntax_kind.TK_FOR, "for")
  | Lexer.TO -> (Syntax_kind.TK_TO, "to")
  | Lexer.DOWNTO -> (Syntax_kind.TK_DOWNTO, "downto")
  | Lexer.LETOP s -> (Syntax_kind.TK_LETOP, s)
  | Lexer.ANDOP s -> (Syntax_kind.TK_ANDOP, s)
  | Lexer.INFIXOP0 s -> (Syntax_kind.TK_INFIXOP0, s)
  | Lexer.INFIXOP1 s -> (Syntax_kind.TK_INFIXOP1, s)
  | Lexer.INFIXOP2 s -> (Syntax_kind.TK_INFIXOP2, s)
  | Lexer.INFIXOP3 s -> (Syntax_kind.TK_INFIXOP3, s)
  | Lexer.INFIXOP4 s -> (Syntax_kind.TK_INFIXOP4, s)
  | Lexer.PREFIXOP s -> (Syntax_kind.TK_PREFIXOP, s)
  (* EOF *)
  | Lexer.EOF -> (Syntax_kind.TK_EOF, "")

(** [syntax_kind_of_token token] converts a lexer token to its CST syntax kind. *)
let syntax_kind_of_token token = fst (token_metadata token)

(** [token_text token] returns the source text representation of a token. *)
let token_text token = snd (token_metadata token)

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
  let kind, text = token_metadata twt.token in
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
