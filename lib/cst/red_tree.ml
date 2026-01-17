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

type syntax_node = {
  green : Green_tree.green_node;
  parent : syntax_node option;
  offset : int;
  index : int;
  mutable children_cache : syntax_element list option;
}

and syntax_token = {
  green : Green_tree.green_token;
  parent : syntax_node;
  offset : int;
  index : int;
}

and syntax_element =
  | SyntaxNode of syntax_node
  | SyntaxToken of syntax_token

(** {1 Root Creation} *)

let root green : syntax_node =
  { green; parent = None; offset = 0; index = 0; children_cache = None }

(** Useful when parsing a fragment from the middle of a file. *)
let root_at green offset : syntax_node =
  { green; parent = None; offset; index = 0; children_cache = None }

(** {1 Basic Accessors} *)

let kind (node : syntax_node) = node.green.kind
let token_kind (token : syntax_token) = token.green.kind

let element_kind = function
  | SyntaxNode node -> kind node
  | SyntaxToken token -> token_kind token

let text_len (node : syntax_node) = node.green.text_len

(** Includes trivia in the length. *)
let token_text_len (token : syntax_token) = Green_tree.token_len token.green

(** {1 Position Information} *)

type span = {
  start : int;
  length : int;
}

let span (node : syntax_node) =
  { start = node.offset; length = node.green.text_len }

(** Includes trivia in the span. *)
let token_span (token : syntax_token) =
  { start = token.offset; length = Green_tree.token_len token.green }

(** Excludes trivia from the span. *)
let token_text_span (token : syntax_token) =
  let leading_len = Green_tree.trivia_len token.green.leading_trivia in
  {
    start = token.offset + leading_len;
    length = String.length token.green.text;
  }

let element_span = function
  | SyntaxNode node -> span node
  | SyntaxToken token -> token_span token

let end_offset (node : syntax_node) = node.offset + node.green.text_len

let token_end_offset (token : syntax_token) =
  token.offset + Green_tree.token_len token.green

(** {1 Text Extraction} *)

(** Includes trivia. *)
let text (node : syntax_node) = Green_tree.green_node_text node.green

(** Excludes trivia. *)
let token_text (token : syntax_token) = Green_tree.token_text token.green

(** Includes trivia. *)
let token_full_text (token : syntax_token) =
  Green_tree.token_full_text token.green

let element_text = function
  | SyntaxNode node -> text node
  | SyntaxToken token -> token_full_text token

(** {1 Parent Navigation} *)

let parent (node : syntax_node) = node.parent
let token_parent (token : syntax_token) = token.parent

let element_parent = function
  | SyntaxNode node -> parent node
  | SyntaxToken token -> Some (token_parent token)

let ancestors (node : syntax_node) =
  let rec go acc (current : syntax_node) =
    match current.parent with
    | None -> List.rev acc
    | Some p -> go (p :: acc) p
  in
  go [] node

let ancestors_and_self (node : syntax_node) = node :: ancestors node

(** {1 Child Navigation} *)

let make_child_element parent_node offset index = function
  | Green_tree.GreenToken green_token ->
      SyntaxToken { green = green_token; parent = parent_node; offset; index }
  | Green_tree.GreenNode green_node ->
      SyntaxNode {
        green = green_node;
        parent = Some parent_node;
        offset;
        index;
        children_cache = None;
      }

(** Cached to avoid O(n) reconstruction on every access. *)
let children (node : syntax_node) =
  match node.children_cache with
  | Some cached -> cached
  | None ->
      let rec build offset index = function
        | [] -> []
        | child :: rest ->
            let element = make_child_element node offset index child in
            let child_len = Green_tree.green_child_len child in
            element :: build (offset + child_len) (index + 1) rest
      in
      let built_children = build node.offset 0 node.green.children in
      node.children_cache <- Some built_children;
      built_children

let extract_node = function SyntaxNode n -> Some n | SyntaxToken _ -> None
let extract_token = function SyntaxToken t -> Some t | SyntaxNode _ -> None

let child_nodes node = List.filter_map extract_node (children node)
let child_tokens node = List.filter_map extract_token (children node)
let child_count (node : syntax_node) = List.length node.green.children

let child_at (node : syntax_node) index =
  let all_children = children node in
  List.nth_opt all_children index

let first_child (node : syntax_node) =
  match children node with
  | [] -> None
  | child :: _ -> Some child

let last_child (node : syntax_node) =
  match List.rev (children node) with
  | [] -> None
  | head :: _ -> Some head

let first_child_node node = List.find_map extract_node (children node)
let first_child_token node = List.find_map extract_token (children node)

(** {1 Sibling Navigation} *)

let element_parent_and_index = function
  | SyntaxNode node -> (node.parent, node.index)
  | SyntaxToken token -> (Some token.parent, token.index)

let sibling_at_offset element offset =
  let parent_opt, index = element_parent_and_index element in
  let target = index + offset in
  if target < 0 then None
  else match parent_opt with None -> None | Some p -> child_at p target

let next_sibling element = sibling_at_offset element 1
let prev_sibling element = sibling_at_offset element (-1)

let rec find_typed_sibling sibling_fn extract element =
  match sibling_fn element with
  | None -> None
  | Some elem ->
      match extract elem with
      | Some found -> Some found
      | None -> find_typed_sibling sibling_fn extract elem

let next_sibling_node element = find_typed_sibling next_sibling extract_node element
let prev_sibling_node element = find_typed_sibling prev_sibling extract_node element
let next_sibling_token element = find_typed_sibling next_sibling extract_token element
let prev_sibling_token element = find_typed_sibling prev_sibling extract_token element

(** {1 Token Navigation} *)

let rec find_token_in_siblings search_fn = function
  | [] -> None
  | SyntaxToken t :: _ -> Some t
  | SyntaxNode n :: rest ->
      match search_fn n with
      | Some t -> Some t
      | None -> find_token_in_siblings search_fn rest

let rec first_token (node : syntax_node) =
  find_token_in_siblings first_token (children node)

let rec last_token (node : syntax_node) =
  find_token_in_siblings last_token (List.rev (children node))

let node_as_element (node : syntax_node) (grandparent : syntax_node) : syntax_element =
  SyntaxNode {
    green = node.green;
    parent = Some grandparent;
    offset = node.offset;
    index = node.index;
    children_cache = None;
  }

let rec traverse_token ~sibling_fn ~descend_fn (token : syntax_token) =
  match sibling_fn (SyntaxToken token) with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> descend_fn n
  | None ->
      let parent = token.parent in
      match parent.parent with
      | None -> None
      | Some grandparent ->
          traverse_from_element ~sibling_fn ~descend_fn (node_as_element parent grandparent)

and traverse_from_element ~sibling_fn ~descend_fn element =
  match sibling_fn element with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode n) -> descend_fn n
  | None ->
      match element_parent element with
      | None -> None
      | Some p -> traverse_from_element ~sibling_fn ~descend_fn (SyntaxNode p)

let next_token token = traverse_token ~sibling_fn:next_sibling ~descend_fn:first_token token
let prev_token token = traverse_token ~sibling_fn:prev_sibling ~descend_fn:last_token token

(** {1 Position-based Lookup} *)

type offset_search_result =
  | BeforeAll
  | NotFound
  | Found of syntax_element

let rec find_element_at_offset byte_offset = function
  | [] -> NotFound
  | element :: rest ->
      let elem_span = element_span element in
      if byte_offset < elem_span.start then BeforeAll
      else if byte_offset < elem_span.start + elem_span.length then Found element
      else find_element_at_offset byte_offset rest

let rec token_at_offset (node : syntax_node) byte_offset =
  if byte_offset < node.offset || byte_offset >= end_offset node then None
  else
    match find_element_at_offset byte_offset (children node) with
    | NotFound | BeforeAll -> None
    | Found (SyntaxToken t) -> Some t
    | Found (SyntaxNode n) -> token_at_offset n byte_offset

let rec node_at_offset (node : syntax_node) byte_offset =
  if byte_offset < node.offset || byte_offset >= end_offset node then None
  else
    match find_element_at_offset byte_offset (children node) with
    | NotFound | BeforeAll -> Some node
    | Found (SyntaxToken _) -> Some node
    | Found (SyntaxNode n) -> node_at_offset n byte_offset

let covering_element (node : syntax_node) start_ofs end_ofs =
  let rec find (current : syntax_node) =
    (* Current node covers the range; try to find a smaller covering child *)
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

let is_root (node : syntax_node) = Option.is_none node.parent
let is_error (node : syntax_node) = Green_tree.is_error node.green
let has_errors (node : syntax_node) = Green_tree.has_errors node.green

(** {1 Iteration} *)

let rec descendants (node : syntax_node) =
  let children_nodes = child_nodes node in
  children_nodes @ List.concat_map descendants children_nodes

let descendants_and_self (node : syntax_node) = node :: descendants node

let rec iter_descendants f (node : syntax_node) =
  List.iter
    (fun child ->
      f child;
      iter_descendants f child)
    (child_nodes node)

let rec fold_descendants f init (node : syntax_node) =
  List.fold_left
    (fun acc child -> fold_descendants f (f acc child) child)
    init (child_nodes node)

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

let iter_tokens f (node : syntax_node) = List.iter f (all_tokens node)

(** {1 Finding Nodes by Kind} *)

let find_child (node : syntax_node) kind =
  List.find_opt
    (fun element -> Syntax_kind.equal (element_kind element) kind)
    (children node)

let find_child_node (node : syntax_node) kind =
  match find_child node kind with
  | Some (SyntaxNode n) -> Some n
  | Some (SyntaxToken _) -> None
  | None -> None

let find_child_token (node : syntax_node) kind =
  match find_child node kind with
  | Some (SyntaxToken t) -> Some t
  | Some (SyntaxNode _) -> None
  | None -> None

let rec find_descendant (node : syntax_node) kind =
  List.find_map
    (fun element ->
      if Syntax_kind.equal (element_kind element) kind then Some element
      else
        match element with
        | SyntaxToken _ -> None
        | SyntaxNode n -> find_descendant n kind)
    (children node)

let rec find_ancestor (node : syntax_node) kind =
  match node.parent with
  | None -> None
  | Some p ->
      if Syntax_kind.equal p.green.kind kind then Some p
      else find_ancestor p kind

(** {1 Trivia Access} *)

let leading_trivia (token : syntax_token) = token.green.leading_trivia
let trailing_trivia (token : syntax_token) = token.green.trailing_trivia
let leading_trivia_text (token : syntax_token) =
  Green_tree.trivia_text token.green.leading_trivia
let trailing_trivia_text (token : syntax_token) =
  Green_tree.trivia_text token.green.trailing_trivia
let has_leading_comment (token : syntax_token) =
  Green_tree.has_leading_comment token.green
let has_trailing_comment (token : syntax_token) =
  Green_tree.has_trailing_comment token.green
let leading_newline_count (token : syntax_token) =
  Green_tree.leading_newline_count token.green
