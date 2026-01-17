(** Lexical analyzer for Lina source code.

    This module provides a Unicode-aware lexer built on Sedlex that tokenizes
    Lina source files. It supports:
    - All Lina keywords and operators
    - Integer literals (decimal, hex [0x], binary [0b]) with underscore separators
    - Float literals with decimal points and/or exponents
    - String literals with escape sequences ([\n], [\t], [\xHH], [\u{HHHH}])
    - Line comments ([--]) and block comments ([(*  *)])
    - Type variables (['a], ['key])
    - Unicode identifiers

    The lexer tracks source locations for error reporting.

    @see <https://github.com/ocaml-community/sedlex> Sedlex documentation *)

(** {1 Token Types} *)

(** Tokens produced by the lexer.

    Tokens carry their semantic value where applicable (integers, floats,
    strings, identifiers). Location information is returned separately
    from {!next_token} and {!tokenize}. *)
type token =
  | INTEGER of int        (** Integer literal: [42], [0xFF], [0b1010], [1_000] *)
  | FLOAT of float        (** Float literal: [3.14], [1e10], [2.5E-3] *)
  | STRING of string      (** String literal: ["hello"], ["line\n"] *)
  | LOWERCASE_IDENTIFIER of string  (** Lowercase identifier: [foo], [my_value] *)
  | UPPERCASE_IDENTIFIER of string  (** Uppercase identifier: [Some], [MyModule] *)
  | TYPE_VARIABLE of string  (** Type variable: ['a], ['key] (without the quote) *)
  | TRUE                  (** Boolean [true] *)
  | FALSE                 (** Boolean [false] *)
  | LET                   (** [let] keyword *)
  | REC                   (** [rec] keyword *)
  | IN                    (** [in] keyword *)
  | FUN                   (** [fun] keyword *)
  | FUNCTION              (** [function] keyword for pattern-matching functions *)
  | IF                    (** [if] keyword *)
  | THEN                  (** [then] keyword *)
  | ELSE                  (** [else] keyword *)
  | TYPE                  (** [type] keyword *)
  | OF                    (** [of] keyword *)
  | AND                   (** [and] keyword *)
  | AS                    (** [as] keyword *)
  | MATCH                 (** [match] keyword *)
  | WITH                  (** [with] keyword *)
  | WHEN                  (** [when] keyword *)
  | MODULE                (** [module] keyword *)
  | STRUCT                (** [struct] keyword *)
  | END                   (** [end] keyword *)
  | SIG                   (** [sig] keyword *)
  | FUNCTOR               (** [functor] keyword *)
  | OPEN                  (** [open] keyword *)
  | INCLUDE               (** [include] keyword *)
  | VAL                   (** [val] keyword *)
  | PRIVATE               (** [private] keyword for private types *)
  | CONSTRAINT            (** [constraint] keyword for type parameter constraints *)
  | EXTERNAL              (** [external] keyword for FFI declarations *)
  | AT                    (** [@] for FFI attributes *)
  | LPAREN                (** [(] or [()] *)
  | RPAREN                (** [)] *)
  | LBRACKET              (** [\[] *)
  | RBRACKET              (** [\]] *)
  | LBRACE                (** [{] *)
  | RBRACE                (** [}] *)
  | COMMA                 (** [,] *)
  | SEMICOLON             (** [;] *)
  | COLON                 (** [:] *)
  | DOT                   (** [.] *)
  | DOTDOT                (** [..] (row extension) *)
  | ARROW                 (** [->] *)
  | EQUAL                 (** [=] *)
  | BAR                   (** [|] *)
  | UNDERSCORE            (** [_] *)
  | STAR                  (** [*] *)
  | PLUS                  (** [+] *)
  | MINUS                 (** [-] *)
  | SLASH                 (** [/] *)
  | CARET                 (** [^] string concatenation *)
  | LESS                  (** [<] *)
  | GREATER               (** [>] *)
  | LESS_EQUAL            (** [<=] *)
  | GREATER_EQUAL         (** [>=] *)
  | EQUAL_EQUAL           (** [==] *)
  | NOT_EQUAL             (** [!=] *)
  | REF                   (** [ref] keyword *)
  | BANG                  (** [!] dereference *)
  | COLONEQUALS           (** [:=] assignment *)
  | COLONCOLON            (** [::] list cons *)
  | BACKTICK_TAG of string (** [`` `Tag ``] polymorphic variant constructor *)
  | TILDE                 (** [~] labeled argument prefix *)
  | QUESTION              (** [?] optional argument prefix *)
  | PLUSEQUAL             (** [+=] type extension *)
  | ASSERT                (** [assert] keyword *)
  | WHILE                 (** [while] keyword *)
  | DO                    (** [do] keyword *)
  | DONE                  (** [done] keyword *)
  | FOR                   (** [for] keyword *)
  | TO                    (** [to] keyword *)
  | DOWNTO                (** [downto] keyword *)
  | LETOP of string       (** [let*], [let+], etc. binding operators *)
  | ANDOP of string       (** [and*], [and+], etc. binding operators *)
  | EOF                   (** End of input *)
[@@deriving show, eq]

(** {1 Token Display} *)

(** [pp_token fmt token] pretty-prints a token to the formatter [fmt].
    Generated by ppx_deriving. *)
val pp_token : Format.formatter -> token -> unit

(** [show_token token] returns a string representation of [token].
    Generated by ppx_deriving. *)
val show_token : token -> string

(** {1 Lexer State} *)

(** Abstract lexer state.

    Encapsulates the Sedlex buffer and current source location.
    The state is mutable internally but this is hidden from users. *)
type state

(** [create_state filename content] creates a new lexer state.

    @param filename The name of the source file (used in error messages)
    @param content The source code to tokenize
    @return A fresh lexer state positioned at the start of [content] *)
val create_state : string -> string -> state

(** {1 Tokenization} *)

(** [next_token state] returns the next token and its location.

    Advances the lexer state past whitespace and comments, then returns
    the next token along with its source location.

    @param state The lexer state to read from
    @return A pair [(token, location)] where [token] is the next token
            and [location] is its position in the source
    @raise Compiler_error.Lexer_error on invalid input such as:
           - Unterminated string literal
           - Unterminated block comment
           - Invalid escape sequence
           - Invalid number literal
           - Unexpected character *)
val next_token : state -> token * Common.Location.t

(** [tokenize filename content] tokenizes the entire input.

    Convenience function that creates a lexer state and collects all tokens
    into a list. The list always ends with [(EOF, loc)].

    @param filename The name of the source file (used in error messages)
    @param content The source code to tokenize
    @return A list of [(token, location)] pairs in source order
    @raise Compiler_error.Lexer_error on invalid input *)
val tokenize : string -> string -> (token * Common.Location.t) list

(** {1 Trivia-Aware Tokenization}

    The following functions provide tokenization with trivia preservation,
    used by the formatter and refactoring tools that need to preserve
    whitespace and comments. *)

(** A token with its attached trivia and location.

    The trivia represents whitespace and comments surrounding the token:
    - [leading] contains trivia that appears before this token
    - [trailing] contains trivia that appears after this token on the same line *)
type token_with_trivia = {
  token : token;
  location : Common.Location.t;
  trivia : Trivia.t;
}
[@@deriving show, eq]

(** State for trivia-aware lexing.

    Similar to {!state} but tracks trivia (whitespace and comments)
    between tokens instead of discarding them. *)
type trivia_state

(** [create_trivia_state filename content] creates a trivia-aware lexer state.

    @param filename The name of the source file (used in error messages)
    @param content The source code to tokenize
    @return A fresh trivia-aware lexer state positioned at the start *)
val create_trivia_state : string -> string -> trivia_state

(** [next_token_with_trivia state] returns the next token with attached trivia.

    Collects leading trivia (whitespace and comments before the token),
    lexes the token, then collects trailing trivia (on the same line).

    Leading trivia includes:
    - Whitespace (spaces, tabs)
    - Newlines
    - Line comments ([--] to end of line)
    - Block comments ([(*] to [*)])

    Trailing trivia includes only content on the same line as the token,
    up to (but not including) the next newline.

    {b Newline preservation}: Line comments do not consume the trailing newline.
    The newline is left in the buffer using [Sedlexing.rollback] so it can be
    collected as a separate [Newline] trivia piece by the next token's leading
    trivia. This ensures lossless source reconstruction.

    @param state The trivia-aware lexer state
    @return A {!token_with_trivia} with the token, location, and attached trivia
    @raise Compiler_error.Lexer_error on invalid input *)
val next_token_with_trivia : trivia_state -> token_with_trivia

(** [tokenize_with_trivia filename content] tokenizes with trivia preservation.

    Returns a list of tokens where each token has its attached leading
    and trailing trivia. This is the main entry point for the formatter.

    @param filename The name of the source file
    @param content The source code to tokenize
    @return A list of {!token_with_trivia} values with attached trivia
    @raise Compiler_error.Lexer_error on invalid input *)
val tokenize_with_trivia : string -> string -> token_with_trivia list
