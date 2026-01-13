(** Semantic token support for the LSP server.

    This module provides semantic token highlighting for the Lina language,
    enabling editors to display type-aware syntax highlighting.

    Token types are chosen to be appropriate for ML-family languages:
    - Namespace for module names
    - Type for type names
    - TypeParameter for type variables ('a, 'b)
    - Parameter for function parameters
    - Variable for let-bound values
    - Property for record fields
    - EnumMember for constructors
    - Function for functions
    - Keyword, String, Number, Operator for literals and syntax *)

(** {1 Token Types} *)

(** Semantic token types following LSP specification. *)
type token_type =
  | TokenNamespace     (** Module names *)
  | TokenType          (** Type names: int, option *)
  | TokenTypeParameter (** Type variables: 'a, 'b *)
  | TokenParameter     (** Function parameters *)
  | TokenVariable      (** Let-bound values *)
  | TokenProperty      (** Record fields *)
  | TokenEnumMember    (** Constructors: Some, None *)
  | TokenFunction      (** Functions *)
  | TokenKeyword       (** Keywords: let, match, if *)
  | TokenString        (** String literals *)
  | TokenNumber        (** Numeric literals *)
  | TokenOperator      (** Operators: +, -, * *)

(** Token modifiers for additional semantics. *)
type token_modifier =
  | ModDeclaration  (** Definition site *)
  | ModReadonly     (** Immutable binding (all let bindings in ML) *)
  | ModStatic       (** Module-level binding *)

(** {1 Semantic Tokens} *)

(** A single semantic token with position and classification. *)
type semantic_token = {
  line : int;                    (** 0-indexed line number *)
  start_char : int;              (** 0-indexed character offset *)
  length : int;                  (** Token length in characters *)
  token_type : token_type;       (** Classification *)
  modifiers : token_modifier list;  (** Additional modifiers *)
}

(** {1 Token Legend} *)

(** Token type names as strings for LSP legend. *)
val token_type_names : string list

(** Token modifier names as strings for LSP legend. *)
val token_modifier_names : string list

(** {1 Token Collection} *)

(** [collect_tokens typed_ast] traverses the typed AST and collects
    semantic tokens for all identifiable elements.

    @param typed_ast The typed structure from type inference
    @return List of semantic tokens sorted by position *)
val collect_tokens : Typing.Typed_tree.typed_structure -> semantic_token list

(** {1 Encoding} *)

(** [encode_tokens tokens] encodes tokens in LSP delta format.

    Each token is encoded as 5 integers:
    - deltaLine: relative line from previous token
    - deltaStartChar: relative character (or absolute if new line)
    - length: token length
    - tokenType: index into token types legend
    - tokenModifiers: bitset of modifier indices

    @param tokens List of semantic tokens (will be sorted)
    @return Integer array for LSP SemanticTokens.data field *)
val encode_tokens : semantic_token list -> int list
