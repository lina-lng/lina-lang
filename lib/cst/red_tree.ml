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
let root green : syntax_node =
  { green; parent = None; offset = 0; index = 0; children_cache = None }

(** [root_at green offset] creates a root syntax node at the given offset.
    Useful when parsing a fragment from the middle of a file. *)
let root_at green offset : syntax_node =
  { green; parent = None; offset; index = 0; children_cache = None }

(** {1 Basic Accessors} *)

(** [kind node] returns the syntax kind of a node. *)
let kind (node : syntax_node) = node.green.kind

(** [token_kind token] returns the syntax kind of a token. *)
let token_kind (token : syntax_token) = token.green.kind

(** [element_kind element] returns the syntax kind of an element. *)
let element_kind = function
  | SyntaxNode node -> kind node
  | SyntaxToken token -> token_kind token

(** [text_len node] returns the text length of a node in bytes. *)
let text_len (node : syntax_node) = node.green.text_len

(** [token_text_len token] returns the text length of a token in bytes
    (including trivia). *)
let token_text_len (token : syntax_token) = Green_tree.token_len token.green

(** {1 Position Information} *)

(** A span represents a range in the source text. *)
type span = {
  start : int;  (** Start byte offset (inclusive) *)
  length : int;  (** Length in bytes *)
}

(** [span node] returns the span of a node in the source text. *)
let span (node : syntax_node) =
  { start = node.offset; length = node.green.text_len }

(** [token_span token] returns the span of a token in the source text
    (including trivia). *)
let token_span (token : syntax_token) =
  { start = token.offset; length = Green_tree.token_len token.green }

(** [token_text_span token] returns the span of a token's text only
    (excluding trivia). *)
let token_text_span (token : syntax_token) =
  let leading_len = Green_tree.trivia_len token.green.leading_trivia in
  {
    start = token.offset + leading_len;
    length = String.length token.green.text;
  }

(** [element_span element] returns the span of an element. *)
let element_span = function
  | SyntaxNode node -> span node
  | SyntaxToken token -> token_span token

(** [end_offset node] returns the byte offset just past the end of the node. *)
let end_offset (node : syntax_node) = node.offset + node.green.text_len

(** [token_end_offset token] returns the byte offset just past the end of the token. *)
let token_end_offset (token : syntax_token) =
  token.offset + Green_tree.token_len token.green

(** {1 Text Extraction} *)

(** [text node] returns the full text of a node (including trivia). *)
let text (node : syntax_node) = Green_tree.green_node_text node.green

(** [token_text token] returns the text of a token (excluding trivia). *)
let token_text (token : syntax_token) = Green_tree.token_text token.green

(** [token_full_text token] returns the full text of a token (including trivia). *)
let token_full_text (token : syntax_token) =
  Green_tree.token_full_text token.green

(** [element_text element] returns the full text of an element. *)
let element_text = function
  | SyntaxNode node -> text node
  | SyntaxToken token -> token_full_text token

(** {1 Parent Navigation} *)

(** [parent node] returns the parent of a node, or [None] for the root. *)
let parent (node : syntax_node) = node.parent

(** [token_parent token] returns the parent node of a token. *)
let token_parent (token : syntax_token) = token.parent

(** [element_parent element] returns the parent of an element. *)
let element_parent = function
  | SyntaxNode node -> parent node
  | SyntaxToken token -> Some (token_parent token)

(** [ancestors node] returns a list of all ancestors from parent to root. *)
let ancestors (node : syntax_node) =
  let rec go acc (current : syntax_node) =
    match current.parent with
    | None -> List.rev acc
    | Some p -> go (p :: acc) p
  in
  go [] node

(** [ancestors_and_self node] returns the node followed by all ancestors. *)
let ancestors_and_self (node : syntax_node) = node :: ancestors node

(** {1 Child Navigation} *)

(** [children node] returns all children of a node as syntax elements.
    Children are cached to avoid O(n) reconstruction on every access. *)
let children (node : syntax_node) =
  match node.children_cache with
  | Some cached -> cached
  | None ->
      let rec build_children offset index = function
        | [] -> []
        | child :: rest ->
            let element =
              match child with
              | Green_tree.GreenToken green_token ->
                  SyntaxToken
                    ({ green = green_token; parent = node; offset; index }
                      : syntax_token)
              | Green_tree.GreenNode green_node ->
                  SyntaxNode
                    ({
                       green = green_node;
                       parent = Some node;
                       offset;
                       index;
                       children_cache = None;
                     }
                      : syntax_node)
            in
            let child_len = Green_tree.green_child_len child in
            element :: build_children (offset + child_len) (index + 1) rest
      in
      let result = build_children node.offset 0 node.green.children in
      node.children_cache <- Some result;
      result

(** [child_nodes node] returns only the child nodes (not tokens). *)
let child_nodes (node : syntax_node) =
  List.filter_map
    (function
      | SyntaxNode n -> Some n
      | SyntaxToken _ -> None)
    (children node)

(** [child_tokens node] returns only the child tokens (not nodes). *)
let child_tokens (node : syntax_node) =
  List.filter_map
    (function
      | SyntaxToken t -> Some t
      | SyntaxNode _ -> None)
    (children node)

(** [child_count node] returns the number of children. *)
let child_count (node : syntax_node) = List.length node.green.children

(** [child_at node index] returns the child at the given index. *)
let child_at (node : syntax_node) index =
  let all_children = children node in
  List.nth_opt all_children index

(** [first_child node] returns the first child, or [None] if empty. *)
let first_child (node : syntax_node) =
  match children node with
  | [] -> None
  | child :: _ -> Some child

(** [last_child node] returns the last child, or [None] if empty. *)
let last_child (node : syntax_node) =
  let rec find_last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: rest -> find_last rest
  in
  find_last (children node)

(** [first_child_node node] returns the first child that is a node. *)
let first_child_node (node : syntax_node) =
  List.find_map
    (function
      | SyntaxNode n -> Some n
      | SyntaxToken _ -> None)
    (children node)

(** [first_child_token node] returns the first child that is a token. *)
let first_child_token (node : syntax_node) =
  List.find_map
    (function
      | SyntaxToken t -> Some t
      | SyntaxNode _ -> None)
    (children node)

(** {1 Sibling Navigation} *)

(** [next_sibling element] returns the next sibling element, or [None]. *)
let next_sibling = function
  | SyntaxNode node -> (
      match node.parent with
      | None -> None
      | Some p -> child_at p (node.index + 1))
  | SyntaxToken token -> child_at token.parent (token.index + 1)

(** [prev_sibling element] returns the previous sibling element, or [None]. *)
let prev_sibling = function
  | SyntaxNode node -> (
      if node.index = 0 then None
      else
        match node.parent with
        | None -> None
        | Some p -> child_at p (node.index - 1))
  | SyntaxToken token ->
      if token.index = 0 then None else child_at token.parent (token.index - 1)

(** [next_sibling_node element] returns the next sibling that is a node. *)
let rec next_sibling_node element =
  match next_sibling element with
  | None -> None
  | Some (SyntaxNode n) -> Some n
  | Some (SyntaxToken _ as t) -> next_sibling_node t

(** [prev_sibling_node element] returns the previous sibling that is a node. *)
let rec prev_sibling_node element =
  match prev_sibling element with
  | None -> None
  | Some (SyntaxNode n) -> Some n
  | Some (SyntaxToken _ as t) -> prev_sibling_node t

(** [next_sibling_token element] returns the next sibling that is a token. *)
let rec next_sibling_token element =
  match next_sibling element with
  | None -> None
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode _ as n) -> next_sibling_token n

(** [prev_sibling_token element] returns the previous sibling that is a token. *)
let rec prev_sibling_token element =
  match prev_sibling element with
  | None -> None
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode _ as n) -> prev_sibling_token n

(** {1 Token Navigation} *)

(** [first_token node] returns the first token in the subtree, or [None]. *)
let rec first_token (node : syntax_node) =
  match children node with
  | [] -> None
  | SyntaxToken t :: _ -> Some t
  | SyntaxNode n :: rest -> (
      match first_token n with
      | Some t -> Some t
      | None ->
          (* Try remaining siblings *)
          let rec find_in_rest = function
            | [] -> None
            | SyntaxToken t :: _ -> Some t
            | SyntaxNode n :: rest -> (
                match first_token n with
                | Some t -> Some t
                | None -> find_in_rest rest)
          in
          find_in_rest rest)

(** [last_token node] returns the last token in the subtree, or [None]. *)
let rec last_token (node : syntax_node) =
  match List.rev (children node) with
  | [] -> None
  | SyntaxToken t :: _ -> Some t
  | SyntaxNode n :: rest -> (
      match last_token n with
      | Some t -> Some t
      | None ->
          let rec find_in_rest = function
            | [] -> None
            | SyntaxToken t :: _ -> Some t
            | SyntaxNode n :: rest -> (
                match last_token n with
                | Some t -> Some t
                | None -> find_in_rest rest)
          in
          find_in_rest rest)

(** [next_token token] returns the next token in document order, or [None]. *)
let rec next_token (token : syntax_token) =
  (* First try next sibling *)
  match next_sibling (SyntaxToken token) with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> first_token n
  | None -> (
      (* Go up to parent and try its next sibling *)
      let parent = token.parent in
      match parent.parent with
      | None -> None
      | Some grandparent ->
          let parent_element : syntax_element =
            SyntaxNode
              {
                green = parent.green;
                parent = Some grandparent;
                offset = parent.offset;
                index = parent.index;
                children_cache = None;
              }
          in
          next_token_from_element parent_element)

and next_token_from_element element =
  match next_sibling element with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> first_token n
  | None -> (
      match element_parent element with
      | None -> None
      | Some p -> next_token_from_element (SyntaxNode p))

(** [prev_token token] returns the previous token in document order, or [None]. *)
let rec prev_token (token : syntax_token) =
  match prev_sibling (SyntaxToken token) with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> last_token n
  | None -> (
      let parent = token.parent in
      match parent.parent with
      | None -> None
      | Some grandparent ->
          let parent_element : syntax_element =
            SyntaxNode
              {
                green = parent.green;
                parent = Some grandparent;
                offset = parent.offset;
                index = parent.index;
                children_cache = None;
              }
          in
          prev_token_from_element parent_element)

and prev_token_from_element element =
  match prev_sibling element with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> last_token n
  | None -> (
      match element_parent element with
      | None -> None
      | Some p -> prev_token_from_element (SyntaxNode p))

(** {1 Position-based Lookup} *)

(** [token_at_offset node offset] finds the token containing the given byte offset. *)
let rec token_at_offset (node : syntax_node) byte_offset =
  if byte_offset < node.offset || byte_offset >= end_offset node then None
  else
    let rec search = function
      | [] -> None
      | element :: rest -> (
          let elem_span = element_span element in
          if byte_offset < elem_span.start then None
          else if byte_offset < elem_span.start + elem_span.length then
            match element with
            | SyntaxToken t -> Some t
            | SyntaxNode n -> token_at_offset n byte_offset
          else search rest)
    in
    search (children node)

(** [node_at_offset node offset] finds the deepest node containing the given offset. *)
let rec node_at_offset (node : syntax_node) byte_offset =
  if byte_offset < node.offset || byte_offset >= end_offset node then None
  else
    let rec search best = function
      | [] -> Some best
      | element :: rest -> (
          let elem_span = element_span element in
          if byte_offset < elem_span.start then Some best
          else if byte_offset < elem_span.start + elem_span.length then
            match element with
            | SyntaxToken _ -> Some best
            | SyntaxNode n -> node_at_offset n byte_offset
          else search best rest)
    in
    search node (children node)

(** [covering_element node range] finds the smallest element covering the range. *)
let covering_element (node : syntax_node) start_ofs end_ofs =
  let rec find (current : syntax_node) =
    (* Current node covers the range, try to find a smaller one *)
    let covering_child =
      List.find_opt
        (fun element ->
          let s = element_span element in
          s.start <= start_ofs && end_ofs <= s.start + s.length)
        (children current)
    in
    match covering_child with
    | Some (SyntaxNode n) -> find n
    | Some (SyntaxToken t) -> SyntaxToken t
    | None -> SyntaxNode current
  in
  if start_ofs >= node.offset && end_ofs <= node.offset + node.green.text_len
  then Some (find node)
  else None

(** {1 Predicates} *)

(** [is_root node] returns [true] if the node is the root. *)
let is_root (node : syntax_node) = Option.is_none node.parent

(** [is_error node] returns [true] if the node is an error node. *)
let is_error (node : syntax_node) = Green_tree.is_error node.green

(** [has_errors node] returns [true] if the node or any descendant has errors. *)
let has_errors (node : syntax_node) = Green_tree.has_errors node.green

(** {1 Iteration} *)

(** [descendants node] returns all descendant nodes in pre-order. *)
let rec descendants (node : syntax_node) =
  let children_nodes = child_nodes node in
  children_nodes @ List.concat_map descendants children_nodes

(** [descendants_and_self node] returns the node and all descendants. *)
let descendants_and_self (node : syntax_node) = node :: descendants node

(** [iter_descendants f node] calls [f] on each descendant in pre-order. *)
let rec iter_descendants f (node : syntax_node) =
  List.iter
    (fun child ->
      f child;
      iter_descendants f child)
    (child_nodes node)

(** [fold_descendants f init node] folds over all descendants in pre-order. *)
let rec fold_descendants f init (node : syntax_node) =
  List.fold_left
    (fun acc child -> fold_descendants f (f acc child) child)
    init (child_nodes node)

(** [all_tokens node] returns all tokens in document order. *)
let all_tokens (node : syntax_node) =
  let rec collect acc (current : syntax_node) =
    List.fold_left
      (fun acc element ->
        match element with
        | SyntaxToken t -> t :: acc
        | SyntaxNode n -> collect acc n)
      acc (children current)
  in
  List.rev (collect [] node)

(** [iter_tokens f node] calls [f] on each token in document order. *)
let iter_tokens f (node : syntax_node) = List.iter f (all_tokens node)

(** {1 Finding Nodes by Kind} *)

(** [find_child node kind] finds the first child with the given syntax kind. *)
let find_child (node : syntax_node) kind =
  List.find_opt
    (fun element -> Syntax_kind.equal (element_kind element) kind)
    (children node)

(** [find_child_node node kind] finds the first child node with the given kind. *)
let find_child_node (node : syntax_node) kind =
  match find_child node kind with
  | Some (SyntaxNode n) -> Some n
  | Some (SyntaxToken _) -> None
  | None -> None

(** [find_child_token node kind] finds the first child token with the given kind. *)
let find_child_token (node : syntax_node) kind =
  match find_child node kind with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode _) -> None
  | None -> None

(** [find_descendant node kind] finds the first descendant with the given kind. *)
let rec find_descendant (node : syntax_node) kind =
  List.find_map
    (fun element ->
      if Syntax_kind.equal (element_kind element) kind then Some element
      else
        match element with
        | SyntaxToken _ -> None
        | SyntaxNode n -> find_descendant n kind)
    (children node)

(** [find_ancestor node kind] finds the first ancestor with the given kind. *)
let rec find_ancestor (node : syntax_node) kind =
  match node.parent with
  | None -> None
  | Some p ->
      if Syntax_kind.equal p.green.kind kind then Some p
      else find_ancestor p kind

(** {1 Trivia Access} *)

(** [leading_trivia token] returns the leading trivia of a token. *)
let leading_trivia (token : syntax_token) = token.green.leading_trivia

(** [trailing_trivia token] returns the trailing trivia of a token. *)
let trailing_trivia (token : syntax_token) = token.green.trailing_trivia

(** [leading_trivia_text token] returns the text of leading trivia. *)
let leading_trivia_text (token : syntax_token) =
  Green_tree.trivia_text token.green.leading_trivia

(** [trailing_trivia_text token] returns the text of trailing trivia. *)
let trailing_trivia_text (token : syntax_token) =
  Green_tree.trivia_text token.green.trailing_trivia

(** [has_leading_comment token] returns [true] if the token has a leading comment. *)
let has_leading_comment (token : syntax_token) =
  Green_tree.has_leading_comment token.green

(** [has_trailing_comment token] returns [true] if the token has a trailing comment. *)
let has_trailing_comment (token : syntax_token) =
  Green_tree.has_trailing_comment token.green

(** [leading_newline_count token] returns the number of newlines before the token. *)
let leading_newline_count (token : syntax_token) =
  Green_tree.leading_newline_count token.green
