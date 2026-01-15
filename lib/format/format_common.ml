(** Common utilities for CST formatting.

    This module provides shared utilities for formatting CST nodes. *)

open Cst
open Doc

(** {1 Trivia Formatting} *)

let format_trivia_piece (piece : Green_tree.trivia_piece) : doc =
  match piece.trivia_kind with
  | Syntax_kind.TK_WHITESPACE ->
      (* Normalize horizontal whitespace to a single space *)
      text " "
  | Syntax_kind.TK_NEWLINE ->
      (* Use literalline to preserve newlines without automatic indentation *)
      literalline
  | Syntax_kind.TK_LINE_COMMENT ->
      (* Line comments include the -- prefix *)
      text piece.trivia_text
  | Syntax_kind.TK_BLOCK_COMMENT ->
      (* Block comments may span multiple lines *)
      let lines = String.split_on_char '\n' piece.trivia_text in
      (match lines with
       | [] -> empty
       | [single] -> text single
       | first :: rest ->
           List.fold_left
             (fun acc line -> concat acc (concat literalline (text line)))
             (text first)
             rest)
  | _ -> empty

let format_leading_trivia_piece (piece : Green_tree.trivia_piece) : doc =
  match piece.trivia_kind with
  | Syntax_kind.TK_WHITESPACE ->
      (* Skip leading whitespace - indentation is handled by the Doc system *)
      empty
  | _ -> format_trivia_piece piece

let format_leading_trivia (trivia : Green_tree.trivia_piece list) : doc =
  concat_list (List.map format_leading_trivia_piece trivia)

let format_trailing_trivia (trivia : Green_tree.trivia_piece list) : doc =
  match trivia with
  | [] -> empty
  | _ ->
      (* Only use line_suffix for line comments (they must stay at end of line).
         Block comments should be emitted inline to preserve position. *)
      let has_line_comment =
        List.exists
          (fun (piece : Green_tree.trivia_piece) ->
            match piece.trivia_kind with
            | Syntax_kind.TK_LINE_COMMENT -> true
            | _ -> false)
          trivia
      in
      if has_line_comment then
        line_suffix (concat_list (List.map format_trivia_piece trivia))
      else concat_list (List.map format_trivia_piece trivia)

(** {1 Token Formatting} *)

let format_token_trivia (token : Red_tree.syntax_token) (d : doc) : doc =
  let leading = format_leading_trivia token.green.leading_trivia in
  let trailing = format_trailing_trivia token.green.trailing_trivia in
  concat leading (concat d trailing)

let format_token (token : Red_tree.syntax_token) : doc =
  let txt = text token.green.text in
  format_token_trivia token txt

let format_token_text (token : Red_tree.syntax_token) : doc =
  text token.green.text

(** {1 Blank Line Detection} *)

let count_newlines (trivia : Green_tree.trivia_piece list) : int =
  List.fold_left
    (fun count (piece : Green_tree.trivia_piece) ->
      match piece.trivia_kind with
      | Syntax_kind.TK_NEWLINE -> count + 1
      | _ -> count)
    0 trivia

let has_blank_line_before (token : Red_tree.syntax_token) : bool =
  count_newlines token.green.leading_trivia >= 2

let blank_lines_before (token : Red_tree.syntax_token) : int =
  let newlines = count_newlines token.green.leading_trivia in
  if newlines > 1 then newlines - 1 else 0

(** {1 List Formatting} *)

let format_separated (sep : doc) (docs : doc list) : doc =
  separate sep docs

let format_separated_trailing (sep : doc) ~(trailing : bool) (docs : doc list)
    : doc =
  match docs with
  | [] -> empty
  | _ ->
      let joined = separate sep docs in
      if trailing then joined ^^ sep else joined

let format_soft_separated (sep : doc) (docs : doc list) : doc =
  match docs with
  | [] -> empty
  | [d] -> d
  | d :: ds ->
      List.fold_left (fun acc x -> acc ^^ sep ^^ softline ^^ x) d ds

(** {1 Forward References for Mutual Recursion}

    These references enable cross-module formatting calls between
    expression, pattern, type, and module formatters. They are
    initialized in format_cst.ml before any formatting occurs. *)

let format_node_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_expression_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_pattern_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_type_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

(** Convenience wrappers for forward references. *)
let format_expression node = !format_expression_ref node
let format_pattern node = !format_pattern_ref node
let format_type node = !format_type_ref node

(** {1 Generic Formatting} *)

let format_children (node : Red_tree.syntax_node) : doc =
  let children = Red_tree.children node in
  concat_list
    (List.map
       (function
         | Red_tree.SyntaxToken token -> format_token token
         | Red_tree.SyntaxNode child -> !format_node_ref child)
       children)

(** {1 Optional Formatting Helpers} *)

let format_optional_token = function
  | Some tok -> format_token tok
  | None -> empty

let format_node_list formatter nodes =
  match nodes with
  | [] -> empty
  | _ -> concat_list (List.map formatter nodes)

let format_node_list_sep ~sep formatter nodes =
  match nodes with
  | [] -> empty
  | _ -> format_separated sep (List.map formatter nodes)

(** {1 Required Accessor Combinators}

    These combinators handle the common pattern of extracting multiple
    optional values from a node and formatting them if all are present.
    Falls back to format_children if any accessor returns None. *)

let with_required2 opt_a opt_b ~fallback ~success =
  match opt_a, opt_b with
  | Some a, Some b -> success a b
  | _ -> fallback ()

let with_required3 opt_a opt_b opt_c ~fallback ~success =
  match opt_a, opt_b, opt_c with
  | Some a, Some b, Some c -> success a b c
  | _ -> fallback ()

let with_required4 opt_a opt_b opt_c opt_d ~fallback ~success =
  match opt_a, opt_b, opt_c, opt_d with
  | Some a, Some b, Some c, Some d -> success a b c d
  | _ -> fallback ()

let with_required5 opt_a opt_b opt_c opt_d opt_e ~fallback ~success =
  match opt_a, opt_b, opt_c, opt_d, opt_e with
  | Some a, Some b, Some c, Some d, Some e -> success a b c d e
  | _ -> fallback ()

(** Format a token if present, otherwise use a fallback doc. *)
let format_token_or fallback = function
  | Some tok -> format_token tok
  | None -> fallback

(** Check if there's a blank line before a node (via its first token). *)
let has_blank_before node =
  match Red_tree.first_token node with
  | Some tok -> has_blank_line_before tok
  | None -> false

(** Format items with blank line preservation between them.

    Uses [hardline ^^ hardline] between items that had blank lines
    in the source, otherwise just [hardline]. *)
let format_items_with_blank_sep ~formatter items =
  let rec go = function
    | [] -> empty
    | [item] -> formatter item
    | item :: next :: rest ->
        let formatted = formatter item in
        let sep = if has_blank_before next then hardline ^^ hardline else hardline in
        formatted ^^ sep ^^ go (next :: rest)
  in
  go items

(** {1 Format Combinators} *)

(** Format a parenthesized construct: (inner)

    @param lparen accessor for left paren token
    @param rparen accessor for right paren token
    @param inner accessor for inner node
    @param formatter how to format the inner node *)
let format_parens
    ~(lparen_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(rparen_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(inner : Red_tree.syntax_node -> Red_tree.syntax_node option)
    ~(formatter : Red_tree.syntax_node -> doc) (node : Red_tree.syntax_node) : doc =
  match lparen_tok node, inner node, rparen_tok node with
  | Some lp, Some inner_node, Some rp ->
      let lp_doc = format_token lp in
      let inner_doc = formatter inner_node in
      let rp_doc = format_token rp in
      group (lp_doc ^^ inner_doc ^^ rp_doc)
  | _ -> format_children node

(** Format a tuple-like construct: (elem, elem, ...)

    Uses fallback delimiters when tokens are missing. *)
let format_tuple_like
    ~(lparen_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(rparen_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(elements : Red_tree.syntax_node -> Red_tree.syntax_node list)
    ~(formatter : Red_tree.syntax_node -> doc) (node : Red_tree.syntax_node) : doc =
  let lparen_doc =
    match lparen_tok node with
    | Some tok -> format_token tok
    | None -> lparen
  in
  let rparen_doc =
    match rparen_tok node with
    | Some tok -> format_token tok
    | None -> rparen
  in
  match elements node with
  | [] -> lparen_doc ^^ rparen_doc
  | elems ->
      let formatted = List.map formatter elems in
      let elements_doc = format_separated (comma ^^ space) formatted in
      group (lparen_doc ^^ elements_doc ^^ rparen_doc)

(** Format a braced container: { field; field; ... }

    Uses block formatting for multi-element containers. *)
let format_braced
    ~(lbrace_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(rbrace_tok : Red_tree.syntax_node -> Red_tree.syntax_token option)
    ~(fields : Red_tree.syntax_node -> Red_tree.syntax_node list)
    ~(format_field : Red_tree.syntax_node -> doc) (node : Red_tree.syntax_node) : doc =
  match lbrace_tok node, rbrace_tok node with
  | Some lbrace, Some rbrace ->
      let lbrace_doc = format_token lbrace in
      let rbrace_doc = format_token rbrace in
      (match fields node with
       | [] -> lbrace_doc ^^ rbrace_doc
       | field_nodes ->
           let formatted = List.map format_field field_nodes in
           let fields_doc = format_separated (semi ^^ space) formatted in
           block lbrace_doc fields_doc rbrace_doc)
  | _ -> format_children node

(** {1 Error Recovery} *)

(** Format a node with error recovery.

    If the node is an error, preserve its text exactly.
    If the node has errors in children, use conservative format_children.
    Otherwise, apply the formatter based on node kind. *)
let with_error_recovery ~(formatter : Syntax_kind.t -> Red_tree.syntax_node -> doc)
    (node : Red_tree.syntax_node) : doc =
  if Red_tree.is_error node then
    text (Red_tree.text node)
  else if Red_tree.has_errors node then
    format_children node
  else
    formatter (Red_tree.kind node) node
