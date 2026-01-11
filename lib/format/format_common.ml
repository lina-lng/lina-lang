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

(** {1 Generic Formatting} *)

let format_node_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_children (node : Red_tree.syntax_node) : doc =
  let children = Red_tree.children node in
  concat_list
    (List.map
       (function
         | Red_tree.SyntaxToken token -> format_token token
         | Red_tree.SyntaxNode child -> !format_node_ref child)
       children)
