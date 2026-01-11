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
let make_trivia kind text =
  if not (Syntax_kind.is_trivia kind) then
    invalid_arg
      (Printf.sprintf "make_trivia: %s is not a trivia kind"
         (Syntax_kind.show kind));
  { trivia_kind = kind; trivia_text = text }

(** [whitespace text] creates a whitespace trivia piece. *)
let whitespace text = { trivia_kind = TK_WHITESPACE; trivia_text = text }

(** [newline text] creates a newline trivia piece. *)
let newline text = { trivia_kind = TK_NEWLINE; trivia_text = text }

(** [line_comment text] creates a line comment trivia piece. *)
let line_comment text = { trivia_kind = TK_LINE_COMMENT; trivia_text = text }

(** [block_comment text] creates a block comment trivia piece. *)
let block_comment text = { trivia_kind = TK_BLOCK_COMMENT; trivia_text = text }

(** {1 Token Constructors} *)

(** [make_token kind text] creates a token with no trivia.

    @raise Invalid_argument if [kind] is not a token kind. *)
let make_token kind text =
  if not (Syntax_kind.is_token kind) then
    invalid_arg
      (Printf.sprintf "make_token: %s is not a token kind"
         (Syntax_kind.show kind));
  { kind; text; leading_trivia = []; trailing_trivia = [] }

(** [make_token_with_trivia kind text ~leading ~trailing] creates a token
    with the specified leading and trailing trivia. *)
let make_token_with_trivia kind text ~leading ~trailing =
  if not (Syntax_kind.is_token kind) then
    invalid_arg
      (Printf.sprintf "make_token_with_trivia: %s is not a token kind"
         (Syntax_kind.show kind));
  { kind; text; leading_trivia = leading; trailing_trivia = trailing }

(** [token_with_leading token trivia] returns a new token with the given
    leading trivia prepended. *)
let token_with_leading token trivia =
  { token with leading_trivia = trivia @ token.leading_trivia }

(** [token_with_trailing token trivia] returns a new token with the given
    trailing trivia appended. *)
let token_with_trailing token trivia =
  { token with trailing_trivia = token.trailing_trivia @ trivia }

(** {1 Length Calculations} *)

(** [trivia_piece_len piece] returns the length of a trivia piece in bytes. *)
let trivia_piece_len piece = String.length piece.trivia_text

(** [trivia_len trivia] returns the total length of a trivia list in bytes. *)
let trivia_len trivia =
  List.fold_left (fun acc piece -> acc + trivia_piece_len piece) 0 trivia

(** [token_len token] returns the total length of a token in bytes,
    including all leading and trailing trivia. *)
let token_len token =
  trivia_len token.leading_trivia
  + String.length token.text
  + trivia_len token.trailing_trivia

(** [green_child_len child] returns the length of a green child in bytes. *)
let green_child_len = function
  | GreenToken token -> token_len token
  | GreenNode node -> node.text_len

(** {1 Node Constructors} *)

(** [make_node kind children] creates a node with the given kind and children.
    The text length is computed automatically from the children.

    @raise Invalid_argument if [kind] is not a node kind. *)
let make_node kind children =
  if not (Syntax_kind.is_node kind) then
    invalid_arg
      (Printf.sprintf "make_node: %s is not a node kind" (Syntax_kind.show kind));
  let text_len =
    List.fold_left (fun acc child -> acc + green_child_len child) 0 children
  in
  { kind; text_len; children }

(** [make_error_node children] creates an error node wrapping the given children.
    Error nodes are used for error recovery during parsing. *)
let make_error_node children = make_node NK_ERROR children

(** {1 Text Extraction} *)

(** [trivia_piece_text piece] returns the text of a trivia piece. *)
let trivia_piece_text piece = piece.trivia_text

(** [trivia_text trivia] returns the concatenated text of all trivia pieces. *)
let trivia_text trivia =
  String.concat "" (List.map trivia_piece_text trivia)

(** [token_text token] returns the text of a token without trivia. *)
let token_text token = token.text

(** [token_full_text token] returns the full text of a token including
    all leading and trailing trivia. *)
let token_full_text token =
  trivia_text token.leading_trivia ^ token.text ^ trivia_text token.trailing_trivia

(** [green_child_text child] returns the full text of a green child. *)
let rec green_child_text = function
  | GreenToken token -> token_full_text token
  | GreenNode node -> green_node_text node

(** [green_node_text node] returns the full text of a green node,
    reconstructing all source text including trivia. *)
and green_node_text node =
  String.concat "" (List.map green_child_text node.children)

(** {1 Child Access} *)

(** [children node] returns the list of children of a node. *)
let children node = node.children

(** [child_count node] returns the number of children of a node. *)
let child_count node = List.length node.children

(** [child_at node index] returns the child at the given index, or [None]
    if the index is out of bounds. *)
let child_at node index =
  List.nth_opt node.children index

(** [first_child node] returns the first child of a node, or [None] if empty. *)
let first_child node = List.nth_opt node.children 0

(** [last_child node] returns the last child of a node, or [None] if empty. *)
let last_child node =
  match List.rev node.children with
  | [] -> None
  | child :: _ -> Some child

(** {1 Token Navigation} *)

(** [first_token node] returns the first token in the subtree rooted at [node],
    or [None] if the node has no tokens. *)
let rec first_token node =
  let rec find_in_children = function
    | [] -> None
    | GreenToken token :: _ -> Some token
    | GreenNode child :: rest -> (
        match first_token child with
        | Some token -> Some token
        | None -> find_in_children rest)
  in
  find_in_children node.children

(** [last_token node] returns the last token in the subtree rooted at [node],
    or [None] if the node has no tokens. *)
let rec last_token node =
  let rec find_in_children = function
    | [] -> None
    | GreenToken token :: _ -> Some token
    | GreenNode child :: rest -> (
        match last_token child with
        | Some token -> Some token
        | None -> find_in_children rest)
  in
  find_in_children (List.rev node.children)

(** {1 Node Predicates} *)

(** [is_error node] returns [true] if the node is an error node. *)
let is_error node = Syntax_kind.equal node.kind NK_ERROR

(** [has_errors node] returns [true] if the node or any of its descendants
    is an error node. *)
let rec has_errors node =
  is_error node
  || List.exists
       (function
         | GreenToken _ -> false
         | GreenNode child -> has_errors child)
       node.children

(** [is_empty node] returns [true] if the node has no children. *)
let is_empty node = node.children = []

(** {1 Iteration} *)

(** [iter_tokens f node] calls [f] on each token in the subtree rooted at [node],
    in left-to-right order. *)
let rec iter_tokens f node =
  List.iter
    (function
      | GreenToken token -> f token
      | GreenNode child -> iter_tokens f child)
    node.children

(** [fold_tokens f init node] folds [f] over each token in the subtree rooted
    at [node], in left-to-right order. *)
let rec fold_tokens f init node =
  List.fold_left
    (fun acc child ->
      match child with
      | GreenToken token -> f acc token
      | GreenNode node -> fold_tokens f acc node)
    init node.children

(** [all_tokens node] returns a list of all tokens in the subtree rooted
    at [node], in left-to-right order. *)
let all_tokens node = List.rev (fold_tokens (fun acc token -> token :: acc) [] node)

(** {1 Trivia Queries} *)

(** [has_leading_trivia token] returns [true] if the token has leading trivia. *)
let has_leading_trivia token = token.leading_trivia <> []

(** [has_trailing_trivia token] returns [true] if the token has trailing trivia. *)
let has_trailing_trivia token = token.trailing_trivia <> []

(** [has_leading_comment token] returns [true] if the token has a leading comment. *)
let has_leading_comment token =
  List.exists
    (fun piece ->
      match piece.trivia_kind with
      | TK_LINE_COMMENT | TK_BLOCK_COMMENT -> true
      | _ -> false)
    token.leading_trivia

(** [has_trailing_comment token] returns [true] if the token has a trailing comment. *)
let has_trailing_comment token =
  List.exists
    (fun piece ->
      match piece.trivia_kind with
      | TK_LINE_COMMENT | TK_BLOCK_COMMENT -> true
      | _ -> false)
    token.trailing_trivia

(** [leading_newline_count token] returns the number of newlines in the
    leading trivia of a token. *)
let leading_newline_count token =
  List.fold_left
    (fun count piece ->
      if Syntax_kind.equal piece.trivia_kind TK_NEWLINE then count + 1
      else count)
    0 token.leading_trivia
