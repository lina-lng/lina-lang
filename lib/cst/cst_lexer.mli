(** CST lexer: wraps the existing lexer to produce green tokens.

    This module bridges the parsing lexer and the CST infrastructure by
    converting lexer tokens and trivia into green tree tokens suitable
    for building the concrete syntax tree.

    The lexer preserves all source text including whitespace and comments
    as trivia attached to tokens. *)

(** {1 Token Conversion} *)

(** [syntax_kind_of_token token] converts a lexer token to its CST syntax kind. *)
val syntax_kind_of_token : Parsing.Lexer.token -> Syntax_kind.t

(** [token_text token] returns the source text representation of a token. *)
val token_text : Parsing.Lexer.token -> string

(** {1 Trivia Conversion} *)

(** [convert_trivia_piece piece] converts a parsing trivia piece to a green trivia piece. *)
val convert_trivia_piece : Parsing.Trivia.trivia_piece -> Green_tree.trivia_piece

(** [convert_trivia_list pieces] converts a list of trivia pieces. *)
val convert_trivia_list : Parsing.Trivia.trivia_piece list -> Green_tree.trivia_piece list

(** {1 CST Token Type} *)

(** A CST token with position information for source mapping. *)
type cst_token = {
  green : Green_tree.green_token;  (** The green token with trivia *)
  location : Common.Location.t;  (** Source location *)
}

(** {1 Tokenization} *)

(** [tokenize filename content] tokenizes source code into CST tokens.

    This is the main entry point for CST tokenization. It produces a list
    of green tokens with all trivia preserved.

    @param filename The name of the source file (for error messages)
    @param content The source code to tokenize
    @return A list of CST tokens with position information *)
val tokenize : string -> string -> cst_token list

(** {1 Streaming Interface} *)

(** Lexer state for incremental tokenization. *)
type state

(** [create_state filename content] creates a new CST lexer state. *)
val create_state : string -> string -> state

(** [next_token state] returns the next CST token.

    Advances the lexer and returns the next token with its trivia. *)
val next_token : state -> cst_token

(** [peek_token state] returns the next token without consuming it. *)
val peek_token : state -> cst_token

(** [is_eof state] returns true if the next token is EOF. *)
val is_eof : state -> bool

(** {1 Utilities} *)

(** [green_tokens tokens] extracts just the green tokens from CST tokens. *)
val green_tokens : cst_token list -> Green_tree.green_token list

(** [all_green_children tokens] wraps green tokens as green children for node construction. *)
val all_green_children : cst_token list -> Green_tree.green_child list
