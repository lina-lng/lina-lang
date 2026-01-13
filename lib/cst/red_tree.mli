(** Red tree: façade over green tree with parent pointers and positions.

    The red tree wraps the green tree ({!Green_tree}) to provide:
    - Parent pointers for upward navigation
    - Absolute byte offsets for source position mapping
    - Sibling indices for horizontal navigation

    Red tree nodes are created lazily as you navigate the tree. This
    maintains the immutability and shareability of the underlying green tree
    while providing the navigation API needed for semantic analysis.

    The term "red tree" comes from Roslyn (the C# compiler), where the
    green/red nomenclature describes this two-layer architecture. *)

(** {1 Types} *)

(** A syntax node wrapping a green node with navigation context. *)
type syntax_node = {
  green : Green_tree.green_node;
  parent : syntax_node option;
  offset : int;  (** Absolute byte offset from start of source *)
  index : int;  (** Index in parent's children list *)
  mutable children_cache : syntax_element list option;
      (** Cached children to avoid O(n) reconstruction on every access *)
}

(** A syntax token wrapping a green token with navigation context. *)
and syntax_token = {
  green : Green_tree.green_token;
  parent : syntax_node;
  offset : int;  (** Absolute byte offset from start of source *)
  index : int;  (** Index in parent's children list *)
}

(** A syntax element is either a node or a token. *)
and syntax_element =
  | SyntaxNode of syntax_node
  | SyntaxToken of syntax_token

(** {1 Root Creation} *)

(** [root green] creates a root syntax node from a green node.
    The root has no parent and offset 0. *)
val root : Green_tree.green_node -> syntax_node

(** [root_at green offset] creates a root syntax node at the given offset.
    Useful when parsing a fragment from the middle of a file. *)
val root_at : Green_tree.green_node -> int -> syntax_node

(** {1 Basic Accessors} *)

(** [kind node] returns the syntax kind of a node. *)
val kind : syntax_node -> Syntax_kind.t

(** [token_kind token] returns the syntax kind of a token. *)
val token_kind : syntax_token -> Syntax_kind.t

(** [element_kind element] returns the syntax kind of an element. *)
val element_kind : syntax_element -> Syntax_kind.t

(** [text_len node] returns the text length of a node in bytes. *)
val text_len : syntax_node -> int

(** [token_text_len token] returns the text length of a token in bytes
    (including trivia). *)
val token_text_len : syntax_token -> int

(** {1 Position Information} *)

(** A span represents a range in the source text. *)
type span = {
  start : int;  (** Start byte offset (inclusive) *)
  length : int;  (** Length in bytes *)
}

(** [span node] returns the span of a node in the source text. *)
val span : syntax_node -> span

(** [token_span token] returns the span of a token in the source text
    (including trivia). *)
val token_span : syntax_token -> span

(** [token_text_span token] returns the span of a token's text only
    (excluding trivia). *)
val token_text_span : syntax_token -> span

(** [element_span element] returns the span of an element. *)
val element_span : syntax_element -> span

(** [end_offset node] returns the byte offset just past the end of the node. *)
val end_offset : syntax_node -> int

(** [token_end_offset token] returns the byte offset just past the end of the token. *)
val token_end_offset : syntax_token -> int

(** {1 Text Extraction} *)

(** [text node] returns the full text of a node (including trivia). *)
val text : syntax_node -> string

(** [token_text token] returns the text of a token (excluding trivia). *)
val token_text : syntax_token -> string

(** [token_full_text token] returns the full text of a token (including trivia). *)
val token_full_text : syntax_token -> string

(** [element_text element] returns the full text of an element. *)
val element_text : syntax_element -> string

(** {1 Parent Navigation} *)

(** [parent node] returns the parent of a node, or [None] for the root. *)
val parent : syntax_node -> syntax_node option

(** [token_parent token] returns the parent node of a token. *)
val token_parent : syntax_token -> syntax_node

(** [element_parent element] returns the parent of an element. *)
val element_parent : syntax_element -> syntax_node option

(** [ancestors node] returns a list of all ancestors from parent to root. *)
val ancestors : syntax_node -> syntax_node list

(** [ancestors_and_self node] returns the node followed by all ancestors. *)
val ancestors_and_self : syntax_node -> syntax_node list

(** {1 Child Navigation} *)

(** [children node] returns all children of a node as syntax elements. *)
val children : syntax_node -> syntax_element list

(** [child_nodes node] returns only the child nodes (not tokens). *)
val child_nodes : syntax_node -> syntax_node list

(** [child_tokens node] returns only the child tokens (not nodes). *)
val child_tokens : syntax_node -> syntax_token list

(** [child_count node] returns the number of children. *)
val child_count : syntax_node -> int

(** [child_at node index] returns the child at the given index. *)
val child_at : syntax_node -> int -> syntax_element option

(** [first_child node] returns the first child, or [None] if empty. *)
val first_child : syntax_node -> syntax_element option

(** [last_child node] returns the last child, or [None] if empty. *)
val last_child : syntax_node -> syntax_element option

(** [first_child_node node] returns the first child that is a node. *)
val first_child_node : syntax_node -> syntax_node option

(** [first_child_token node] returns the first child that is a token. *)
val first_child_token : syntax_node -> syntax_token option

(** {1 Sibling Navigation} *)

(** [next_sibling element] returns the next sibling element, or [None]. *)
val next_sibling : syntax_element -> syntax_element option

(** [prev_sibling element] returns the previous sibling element, or [None]. *)
val prev_sibling : syntax_element -> syntax_element option

(** [next_sibling_node element] returns the next sibling that is a node. *)
val next_sibling_node : syntax_element -> syntax_node option

(** [prev_sibling_node element] returns the previous sibling that is a node. *)
val prev_sibling_node : syntax_element -> syntax_node option

(** [next_sibling_token element] returns the next sibling that is a token. *)
val next_sibling_token : syntax_element -> syntax_token option

(** [prev_sibling_token element] returns the previous sibling that is a token. *)
val prev_sibling_token : syntax_element -> syntax_token option

(** {1 Token Navigation} *)

(** [first_token node] returns the first token in the subtree, or [None]. *)
val first_token : syntax_node -> syntax_token option

(** [last_token node] returns the last token in the subtree, or [None]. *)
val last_token : syntax_node -> syntax_token option

(** [next_token token] returns the next token in document order, or [None]. *)
val next_token : syntax_token -> syntax_token option

(** [prev_token token] returns the previous token in document order, or [None]. *)
val prev_token : syntax_token -> syntax_token option

(** {1 Position-based Lookup} *)

(** [token_at_offset node offset] finds the token containing the given byte offset. *)
val token_at_offset : syntax_node -> int -> syntax_token option

(** [node_at_offset node offset] finds the deepest node containing the given offset. *)
val node_at_offset : syntax_node -> int -> syntax_node option

(** [covering_element node start_offset end_offset] finds the smallest element
    covering the range from [start_offset] to [end_offset]. *)
val covering_element : syntax_node -> int -> int -> syntax_element option

(** {1 Predicates} *)

(** [is_root node] returns [true] if the node is the root. *)
val is_root : syntax_node -> bool

(** [is_error node] returns [true] if the node is an error node. *)
val is_error : syntax_node -> bool

(** [has_errors node] returns [true] if the node or any descendant has errors. *)
val has_errors : syntax_node -> bool

(** {1 Iteration} *)

(** [descendants node] returns all descendant nodes in pre-order. *)
val descendants : syntax_node -> syntax_node list

(** [descendants_and_self node] returns the node and all descendants. *)
val descendants_and_self : syntax_node -> syntax_node list

(** [iter_descendants f node] calls [f] on each descendant in pre-order. *)
val iter_descendants : (syntax_node -> unit) -> syntax_node -> unit

(** [fold_descendants f init node] folds over all descendants in pre-order. *)
val fold_descendants : ('a -> syntax_node -> 'a) -> 'a -> syntax_node -> 'a

(** [all_tokens node] returns all tokens in document order. *)
val all_tokens : syntax_node -> syntax_token list

(** [iter_tokens f node] calls [f] on each token in document order. *)
val iter_tokens : (syntax_token -> unit) -> syntax_node -> unit

(** {1 Finding Nodes by Kind} *)

(** [find_child node kind] finds the first child with the given syntax kind. *)
val find_child : syntax_node -> Syntax_kind.t -> syntax_element option

(** [find_child_node node kind] finds the first child node with the given kind. *)
val find_child_node : syntax_node -> Syntax_kind.t -> syntax_node option

(** [find_child_token node kind] finds the first child token with the given kind. *)
val find_child_token : syntax_node -> Syntax_kind.t -> syntax_token option

(** [find_descendant node kind] finds the first descendant with the given kind. *)
val find_descendant : syntax_node -> Syntax_kind.t -> syntax_element option

(** [find_ancestor node kind] finds the first ancestor with the given kind. *)
val find_ancestor : syntax_node -> Syntax_kind.t -> syntax_node option

(** {1 Trivia Access} *)

(** [leading_trivia token] returns the leading trivia of a token. *)
val leading_trivia : syntax_token -> Green_tree.trivia_piece list

(** [trailing_trivia token] returns the trailing trivia of a token. *)
val trailing_trivia : syntax_token -> Green_tree.trivia_piece list

(** [leading_trivia_text token] returns the text of leading trivia. *)
val leading_trivia_text : syntax_token -> string

(** [trailing_trivia_text token] returns the text of trailing trivia. *)
val trailing_trivia_text : syntax_token -> string

(** [has_leading_comment token] returns [true] if the token has a leading comment. *)
val has_leading_comment : syntax_token -> bool

(** [has_trailing_comment token] returns [true] if the token has a trailing comment. *)
val has_trailing_comment : syntax_token -> bool

(** [leading_newline_count token] returns the number of newlines before the token. *)
val leading_newline_count : syntax_token -> int
