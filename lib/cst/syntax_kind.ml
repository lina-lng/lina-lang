(** Syntax kind enumeration for the Concrete Syntax Tree.

    This module defines all possible kinds of tokens and nodes in the CST.
    Every element in the tree (whether a token or a composite node) has
    an associated syntax kind that identifies its grammatical role.

    Token kinds are prefixed with [TK_], node kinds with [NK_]. *)

(** The kind of a syntax element (token or node). *)
type t =
  (* ===== Tokens ===== *)

  (* Keywords *)
  | TK_LET
  | TK_REC
  | TK_IN
  | TK_FUN
  | TK_FUNCTION
  | TK_IF
  | TK_THEN
  | TK_ELSE
  | TK_TYPE
  | TK_OF
  | TK_AND
  | TK_AS
  | TK_MATCH
  | TK_WITH
  | TK_WHEN

  (* Module keywords *)
  | TK_MODULE
  | TK_STRUCT
  | TK_END
  | TK_SIG
  | TK_FUNCTOR
  | TK_OPEN
  | TK_INCLUDE
  | TK_VAL
  | TK_PRIVATE
  | TK_CONSTRAINT

  (* FFI keywords *)
  | TK_EXTERNAL

  (* Literals *)
  | TK_INTEGER
  | TK_FLOAT
  | TK_STRING
  | TK_TRUE
  | TK_FALSE

  (* Identifiers *)
  | TK_LOWERCASE_IDENT
  | TK_UPPERCASE_IDENT
  | TK_TYPE_VARIABLE
  | TK_BACKTICK_TAG     (** `` `Tag `` polymorphic variant constructor *)

  (* Delimiters *)
  | TK_LPAREN
  | TK_RPAREN
  | TK_LBRACKET
  | TK_RBRACKET
  | TK_LBRACE
  | TK_RBRACE

  (* Punctuation *)
  | TK_COMMA
  | TK_SEMICOLON
  | TK_COLON
  | TK_DOT
  | TK_DOTDOT
  | TK_ARROW
  | TK_EQUAL
  | TK_BAR
  | TK_UNDERSCORE
  | TK_AT

  (* Operators *)
  | TK_STAR
  | TK_PLUS
  | TK_MINUS
  | TK_SLASH
  | TK_CARET         (** ^ string concatenation *)
  | TK_LESS
  | TK_GREATER
  | TK_LESS_EQUAL
  | TK_GREATER_EQUAL
  | TK_EQUAL_EQUAL
  | TK_NOT_EQUAL
  | TK_REF           (** ref keyword *)
  | TK_BANG          (** ! dereference operator *)
  | TK_COLONEQUALS   (** := assignment operator *)
  | TK_COLONCOLON    (** :: list cons operator *)
  | TK_TILDE         (** ~ labeled argument prefix *)
  | TK_QUESTION      (** ? optional argument prefix *)
  | TK_PLUSEQUAL     (** += type extension *)

  (* Special *)
  | TK_EOF

  (* Trivia (whitespace and comments) *)
  | TK_WHITESPACE
  | TK_NEWLINE
  | TK_LINE_COMMENT
  | TK_BLOCK_COMMENT

  (* ===== Nodes ===== *)

  (* Top-level *)
  | NK_SOURCE_FILE
  | NK_ERROR

  (* Expressions *)
  | NK_VARIABLE_EXPR
  | NK_CONSTANT_EXPR
  | NK_TUPLE_EXPR
  | NK_CONSTRUCTOR_EXPR
  | NK_APPLY_EXPR
  | NK_INFIX_EXPR
  | NK_FUNCTION_EXPR
  | NK_LET_EXPR
  | NK_IF_EXPR
  | NK_SEQUENCE_EXPR
  | NK_CONSTRAINT_EXPR
  | NK_RECORD_EXPR
  | NK_RECORD_ACCESS_EXPR
  | NK_RECORD_UPDATE_EXPR
  | NK_MATCH_EXPR
  | NK_MODULE_ACCESS_EXPR
  | NK_PAREN_EXPR

  (* Expression components *)
  | NK_RECORD_FIELD
  | NK_MATCH_ARM

  (* Patterns *)
  | NK_VARIABLE_PATTERN
  | NK_WILDCARD_PATTERN
  | NK_CONSTANT_PATTERN
  | NK_TUPLE_PATTERN
  | NK_CONSTRUCTOR_PATTERN
  | NK_ALIAS_PATTERN
  | NK_CONSTRAINT_PATTERN
  | NK_RECORD_PATTERN
  | NK_PAREN_PATTERN

  (* Pattern components *)
  | NK_RECORD_PATTERN_FIELD

  (* Types *)
  | NK_TYPE_VARIABLE
  | NK_TYPE_CONSTRUCTOR
  | NK_TYPE_TUPLE
  | NK_TYPE_ARROW
  | NK_TYPE_RECORD
  | NK_PAREN_TYPE

  (* Type components *)
  | NK_TYPE_RECORD_FIELD

  (* Bindings and declarations *)
  | NK_BINDING
  | NK_TYPE_DECLARATION
  | NK_CONSTRUCTOR_DECLARATION
  | NK_EXTERNAL_DECLARATION

  (* Structure items *)
  | NK_VALUE_DEFINITION
  | NK_TYPE_DEFINITION
  | NK_MODULE_DEFINITION
  | NK_MODULE_TYPE_DEFINITION
  | NK_OPEN_DECLARATION
  | NK_INCLUDE_DECLARATION

  (* Module expressions *)
  | NK_STRUCTURE
  | NK_MODULE_PATH
  | NK_FUNCTOR_EXPR
  | NK_MODULE_APPLY
  | NK_MODULE_CONSTRAINT

  (* Module types (signatures) *)
  | NK_SIGNATURE
  | NK_MODULE_TYPE_PATH
  | NK_FUNCTOR_TYPE
  | NK_MODULE_TYPE_WITH

  (* Signature items *)
  | NK_VALUE_SPECIFICATION
  | NK_TYPE_SPECIFICATION
  | NK_MODULE_SPECIFICATION
  | NK_MODULE_TYPE_SPECIFICATION
  | NK_INCLUDE_SPECIFICATION

  (* Module components *)
  | NK_FUNCTOR_PARAMETER
  | NK_WITH_TYPE_CONSTRAINT
  | NK_WITH_MODULE_CONSTRAINT
  | NK_MODULE_BINDING

  (* Miscellaneous *)
  | NK_LONGIDENT
  | NK_ATTRIBUTE
  | NK_ATTRIBUTE_PAYLOAD
  | NK_TYPE_PARAMETERS
  | NK_ARGUMENT_LIST
[@@deriving show, eq]

(** Check if a syntax kind is a token (as opposed to a node). *)
let is_token = function
  | TK_LET | TK_REC | TK_IN | TK_FUN | TK_FUNCTION | TK_IF | TK_THEN | TK_ELSE
  | TK_TYPE | TK_OF | TK_AND | TK_AS | TK_MATCH | TK_WITH | TK_WHEN
  | TK_MODULE | TK_STRUCT | TK_END | TK_SIG | TK_FUNCTOR
  | TK_OPEN | TK_INCLUDE | TK_VAL | TK_PRIVATE | TK_CONSTRAINT | TK_EXTERNAL
  | TK_INTEGER | TK_FLOAT | TK_STRING | TK_TRUE | TK_FALSE
  | TK_LOWERCASE_IDENT | TK_UPPERCASE_IDENT | TK_TYPE_VARIABLE | TK_BACKTICK_TAG
  | TK_LPAREN | TK_RPAREN | TK_LBRACKET | TK_RBRACKET | TK_LBRACE | TK_RBRACE
  | TK_COMMA | TK_SEMICOLON | TK_COLON | TK_DOT | TK_DOTDOT
  | TK_ARROW | TK_EQUAL | TK_BAR | TK_UNDERSCORE | TK_AT
  | TK_STAR | TK_PLUS | TK_MINUS | TK_SLASH | TK_CARET
  | TK_LESS | TK_GREATER | TK_LESS_EQUAL | TK_GREATER_EQUAL
  | TK_EQUAL_EQUAL | TK_NOT_EQUAL
  | TK_REF | TK_BANG | TK_COLONEQUALS | TK_COLONCOLON | TK_TILDE | TK_QUESTION | TK_PLUSEQUAL
  | TK_EOF
  | TK_WHITESPACE | TK_NEWLINE | TK_LINE_COMMENT | TK_BLOCK_COMMENT -> true
  | _ -> false

(** Check if a syntax kind is a node (as opposed to a token). *)
let is_node kind = not (is_token kind)

(** Check if a syntax kind is trivia (whitespace or comments). *)
let is_trivia = function
  | TK_WHITESPACE | TK_NEWLINE | TK_LINE_COMMENT | TK_BLOCK_COMMENT -> true
  | _ -> false

(** Check if a syntax kind is a comment (line or block). *)
let is_comment = function
  | TK_LINE_COMMENT | TK_BLOCK_COMMENT -> true
  | _ -> false

(** Check if a syntax kind is a keyword. *)
let is_keyword = function
  | TK_LET | TK_REC | TK_IN | TK_FUN | TK_FUNCTION | TK_IF | TK_THEN | TK_ELSE
  | TK_TYPE | TK_OF | TK_AND | TK_AS | TK_MATCH | TK_WITH | TK_WHEN
  | TK_MODULE | TK_STRUCT | TK_END | TK_SIG | TK_FUNCTOR
  | TK_OPEN | TK_INCLUDE | TK_VAL | TK_PRIVATE | TK_CONSTRAINT | TK_EXTERNAL
  | TK_TRUE | TK_FALSE | TK_REF -> true
  | _ -> false

(** Check if a syntax kind is a punctuation token. *)
let is_punctuation = function
  | TK_COMMA | TK_SEMICOLON | TK_COLON | TK_DOT | TK_DOTDOT
  | TK_ARROW | TK_EQUAL | TK_BAR | TK_AT -> true
  | _ -> false

(** Check if a syntax kind is an operator. *)
let is_operator = function
  | TK_STAR | TK_PLUS | TK_MINUS | TK_SLASH | TK_CARET
  | TK_LESS | TK_GREATER | TK_LESS_EQUAL | TK_GREATER_EQUAL
  | TK_EQUAL_EQUAL | TK_NOT_EQUAL
  | TK_BANG | TK_COLONEQUALS | TK_COLONCOLON -> true
  | _ -> false

(** Check if a syntax kind is an expression node. *)
let is_expression = function
  | NK_VARIABLE_EXPR | NK_CONSTANT_EXPR | NK_TUPLE_EXPR
  | NK_CONSTRUCTOR_EXPR | NK_APPLY_EXPR | NK_INFIX_EXPR
  | NK_FUNCTION_EXPR | NK_LET_EXPR | NK_IF_EXPR | NK_SEQUENCE_EXPR
  | NK_CONSTRAINT_EXPR | NK_RECORD_EXPR | NK_RECORD_ACCESS_EXPR
  | NK_RECORD_UPDATE_EXPR | NK_MATCH_EXPR | NK_MODULE_ACCESS_EXPR
  | NK_PAREN_EXPR -> true
  | _ -> false

(** Check if a syntax kind is a pattern node. *)
let is_pattern = function
  | NK_VARIABLE_PATTERN | NK_WILDCARD_PATTERN | NK_CONSTANT_PATTERN
  | NK_TUPLE_PATTERN | NK_CONSTRUCTOR_PATTERN | NK_ALIAS_PATTERN
  | NK_CONSTRAINT_PATTERN | NK_RECORD_PATTERN | NK_PAREN_PATTERN -> true
  | _ -> false

(** Check if a syntax kind is a type node. *)
let is_type = function
  | NK_TYPE_VARIABLE | NK_TYPE_CONSTRUCTOR | NK_TYPE_TUPLE
  | NK_TYPE_ARROW | NK_TYPE_RECORD | NK_PAREN_TYPE -> true
  | _ -> false
