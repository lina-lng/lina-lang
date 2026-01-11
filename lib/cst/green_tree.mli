(** Green tree: immutable, position-independent syntax tree.

    The green tree is the core data structure of the CST. It is:
    - Immutable: nodes can be shared and cached
    - Position-independent: no absolute positions, only relative lengths
    - Lossless: preserves all source text including trivia (whitespace, comments)

    Trivia (whitespace and comments) is attached to tokens:
    - Leading trivia: trivia that appears before the token
    - Trailing trivia: trivia on the same line after the token

    The green tree is wrapped by the red tree ({!Red_tree}) which provides
    parent pointers and absolute positions for navigation. *)

(** {1 Trivia} *)

(** A single piece of trivia (whitespace or comment). *)
type trivia_piece = {
  trivia_kind : Syntax_kind.t;
  trivia_text : string;
}
[@@deriving show, eq]

(** {1 Green Tokens} *)

(** A token in the green tree with attached trivia.

    Tokens are the leaves of the syntax tree. Each token stores:
    - Its syntax kind (e.g., [TK_LET], [TK_IDENT])
    - Its text content (the actual source characters)
    - Leading trivia (whitespace/comments before the token)
    - Trailing trivia (whitespace/comments after the token, on the same line) *)
type green_token = {
  kind : Syntax_kind.t;
  text : string;
  leading_trivia : trivia_piece list;
  trailing_trivia : trivia_piece list;
}
[@@deriving show, eq]

(** {1 Green Nodes} *)

(** A child of a green node: either a token or a nested node. *)
type green_child =
  | GreenToken of green_token
  | GreenNode of green_node

(** An interior node in the green tree.

    Nodes represent syntactic constructs composed of tokens and other nodes.
    Each node stores:
    - Its syntax kind (e.g., [NK_LET_EXPR], [NK_FUNCTION_EXPR])
    - The total length of its text (including all children and trivia)
    - Its children (tokens and nested nodes) *)
and green_node = {
  kind : Syntax_kind.t;
  text_len : int;
  children : green_child list;
}
[@@deriving show, eq]

(** {1 Trivia Constructors} *)

(** [make_trivia kind text] creates a trivia piece with the given kind and text.

    @raise Invalid_argument if [kind] is not a trivia kind. *)
val make_trivia : Syntax_kind.t -> string -> trivia_piece

(** [whitespace text] creates a whitespace trivia piece. *)
val whitespace : string -> trivia_piece

(** [newline text] creates a newline trivia piece. *)
val newline : string -> trivia_piece

(** [line_comment text] creates a line comment trivia piece. *)
val line_comment : string -> trivia_piece

(** [block_comment text] creates a block comment trivia piece. *)
val block_comment : string -> trivia_piece

(** {1 Token Constructors} *)

(** [make_token kind text] creates a token with no trivia.

    @raise Invalid_argument if [kind] is not a token kind. *)
val make_token : Syntax_kind.t -> string -> green_token

(** [make_token_with_trivia kind text ~leading ~trailing] creates a token
    with the specified leading and trailing trivia.

    @raise Invalid_argument if [kind] is not a token kind. *)
val make_token_with_trivia :
  Syntax_kind.t ->
  string ->
  leading:trivia_piece list ->
  trailing:trivia_piece list ->
  green_token

(** [token_with_leading token trivia] returns a new token with the given
    leading trivia prepended. *)
val token_with_leading : green_token -> trivia_piece list -> green_token

(** [token_with_trailing token trivia] returns a new token with the given
    trailing trivia appended. *)
val token_with_trailing : green_token -> trivia_piece list -> green_token

(** {1 Length Calculations} *)

(** [trivia_piece_len piece] returns the length of a trivia piece in bytes. *)
val trivia_piece_len : trivia_piece -> int

(** [trivia_len trivia] returns the total length of a trivia list in bytes. *)
val trivia_len : trivia_piece list -> int

(** [token_len token] returns the total length of a token in bytes,
    including all leading and trailing trivia. *)
val token_len : green_token -> int

(** [green_child_len child] returns the length of a green child in bytes. *)
val green_child_len : green_child -> int

(** {1 Node Constructors} *)

(** [make_node kind children] creates a node with the given kind and children.
    The text length is computed automatically from the children.

    @raise Invalid_argument if [kind] is not a node kind. *)
val make_node : Syntax_kind.t -> green_child list -> green_node

(** [make_error_node children] creates an error node wrapping the given children.
    Error nodes are used for error recovery during parsing. *)
val make_error_node : green_child list -> green_node

(** {1 Text Extraction} *)

(** [trivia_piece_text piece] returns the text of a trivia piece. *)
val trivia_piece_text : trivia_piece -> string

(** [trivia_text trivia] returns the concatenated text of all trivia pieces. *)
val trivia_text : trivia_piece list -> string

(** [token_text token] returns the text of a token without trivia. *)
val token_text : green_token -> string

(** [token_full_text token] returns the full text of a token including
    all leading and trailing trivia. *)
val token_full_text : green_token -> string

(** [green_child_text child] returns the full text of a green child. *)
val green_child_text : green_child -> string

(** [green_node_text node] returns the full text of a green node,
    reconstructing all source text including trivia. *)
val green_node_text : green_node -> string

(** {1 Child Access} *)

(** [children node] returns the list of children of a node. *)
val children : green_node -> green_child list

(** [child_count node] returns the number of children of a node. *)
val child_count : green_node -> int

(** [child_at node index] returns the child at the given index, or [None]
    if the index is out of bounds. *)
val child_at : green_node -> int -> green_child option

(** [first_child node] returns the first child of a node, or [None] if empty. *)
val first_child : green_node -> green_child option

(** [last_child node] returns the last child of a node, or [None] if empty. *)
val last_child : green_node -> green_child option

(** {1 Token Navigation} *)

(** [first_token node] returns the first token in the subtree rooted at [node],
    or [None] if the node has no tokens. *)
val first_token : green_node -> green_token option

(** [last_token node] returns the last token in the subtree rooted at [node],
    or [None] if the node has no tokens. *)
val last_token : green_node -> green_token option

(** {1 Node Predicates} *)

(** [is_error node] returns [true] if the node is an error node. *)
val is_error : green_node -> bool

(** [has_errors node] returns [true] if the node or any of its descendants
    is an error node. *)
val has_errors : green_node -> bool

(** [is_empty node] returns [true] if the node has no children. *)
val is_empty : green_node -> bool

(** {1 Iteration} *)

(** [iter_tokens f node] calls [f] on each token in the subtree rooted at [node],
    in left-to-right order. *)
val iter_tokens : (green_token -> unit) -> green_node -> unit

(** [fold_tokens f init node] folds [f] over each token in the subtree rooted
    at [node], in left-to-right order. *)
val fold_tokens : ('a -> green_token -> 'a) -> 'a -> green_node -> 'a

(** [all_tokens node] returns a list of all tokens in the subtree rooted
    at [node], in left-to-right order. *)
val all_tokens : green_node -> green_token list

(** {1 Trivia Queries} *)

(** [has_leading_trivia token] returns [true] if the token has leading trivia. *)
val has_leading_trivia : green_token -> bool

(** [has_trailing_trivia token] returns [true] if the token has trailing trivia. *)
val has_trailing_trivia : green_token -> bool

(** [has_leading_comment token] returns [true] if the token has a leading comment. *)
val has_leading_comment : green_token -> bool

(** [has_trailing_comment token] returns [true] if the token has a trailing comment. *)
val has_trailing_comment : green_token -> bool

(** [leading_newline_count token] returns the number of newlines in the
    leading trivia of a token. *)
val leading_newline_count : green_token -> int
