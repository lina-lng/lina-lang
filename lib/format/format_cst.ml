(** CST-based formatter: formats CST nodes while preserving trivia.

    This module provides formatting functions that work directly on the CST,
    preserving all comments and whitespace in their original positions.
    Unlike the AST-based formatter, this ensures lossless formatting. *)

open Cst
open Doc

(** {1 Trivia Formatting} *)

(** [format_trivia_piece piece] formats a single trivia piece to a doc. *)
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

(** [format_leading_trivia_piece piece] formats a leading trivia piece.
    Whitespace in leading trivia is skipped (indentation handled by Doc). *)
let format_leading_trivia_piece (piece : Green_tree.trivia_piece) : doc =
  match piece.trivia_kind with
  | Syntax_kind.TK_WHITESPACE ->
      (* Skip leading whitespace - indentation is handled by the Doc system *)
      empty
  | _ -> format_trivia_piece piece

(** [format_leading_trivia trivia] formats leading trivia.
    Leading trivia appears before a token and may include newlines and comments. *)
let format_leading_trivia (trivia : Green_tree.trivia_piece list) : doc =
  concat_list (List.map format_leading_trivia_piece trivia)

(** [format_trailing_trivia trivia] formats trailing trivia.
    Trailing trivia appears on the same line after a token. *)
let format_trailing_trivia (trivia : Green_tree.trivia_piece list) : doc =
  match trivia with
  | [] -> empty
  | _ ->
      (* Only use line_suffix for line comments (they must stay at end of line).
         Block comments should be emitted inline to preserve position. *)
      let has_line_comment =
        List.exists
          (fun (p : Green_tree.trivia_piece) ->
            match p.trivia_kind with
            | TK_LINE_COMMENT -> true
            | _ -> false)
          trivia
      in
      if has_line_comment then
        line_suffix (concat_list (List.map format_trivia_piece trivia))
      else concat_list (List.map format_trivia_piece trivia)

(** [format_token_trivia token doc] wraps a formatted token with its trivia. *)
let format_token_trivia (token : Red_tree.syntax_token) (d : doc) : doc =
  let leading = format_leading_trivia token.green.leading_trivia in
  let trailing = format_trailing_trivia token.green.trailing_trivia in
  concat leading (concat d trailing)

(** {1 Token Formatting} *)

(** [format_token token] formats a token including its trivia. *)
let format_token (token : Red_tree.syntax_token) : doc =
  let txt = text token.green.text in
  format_token_trivia token txt

(** [format_token_text token] formats just the token text without trivia. *)
let format_token_text (token : Red_tree.syntax_token) : doc =
  text token.green.text

(** {1 Node Formatting} *)

(** Forward reference for recursive formatting. *)
let format_node_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

(** [format_children node] formats all children of a node. *)
let format_children (node : Red_tree.syntax_node) : doc =
  let children = Red_tree.children node in
  concat_list
    (List.map
       (function
         | Red_tree.SyntaxToken token -> format_token token
         | Red_tree.SyntaxNode child -> !format_node_ref child)
       children)

(** [format_expression node] formats an expression node. *)
let format_expression (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_CONSTANT_EXPR | NK_VARIABLE_EXPR | NK_CONSTRUCTOR_EXPR ->
      format_children node
  | NK_PAREN_EXPR -> group (format_children node)
  | NK_TUPLE_EXPR -> group (format_children node)
  | NK_APPLY_EXPR -> group (format_children node)
  | NK_INFIX_EXPR -> group (nest indent_width (format_children node))
  | NK_FUNCTION_EXPR -> group (nest indent_width (format_children node))
  | NK_LET_EXPR -> group (nest indent_width (format_children node))
  | NK_IF_EXPR -> group (nest indent_width (format_children node))
  | NK_MATCH_EXPR -> group (nest indent_width (format_children node))
  | NK_RECORD_EXPR -> group (nest indent_width (format_children node))
  | NK_RECORD_ACCESS_EXPR | NK_RECORD_UPDATE_EXPR -> format_children node
  | NK_SEQUENCE_EXPR | NK_CONSTRAINT_EXPR | NK_MODULE_ACCESS_EXPR ->
      format_children node
  | _ -> format_children node

(** [format_pattern node] formats a pattern node. *)
let format_pattern (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_VARIABLE_PATTERN | NK_WILDCARD_PATTERN | NK_CONSTANT_PATTERN ->
      format_children node
  | NK_CONSTRUCTOR_PATTERN -> group (format_children node)
  | NK_TUPLE_PATTERN -> group (format_children node)
  | NK_RECORD_PATTERN -> group (nest indent_width (format_children node))
  | NK_PAREN_PATTERN | NK_ALIAS_PATTERN | NK_CONSTRAINT_PATTERN ->
      format_children node
  | _ -> format_children node

(** [format_type node] formats a type node. *)
let format_type (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_TYPE_VARIABLE | NK_TYPE_CONSTRUCTOR -> format_children node
  | NK_TYPE_TUPLE -> group (format_children node)
  | NK_TYPE_ARROW -> group (nest indent_width (format_children node))
  | NK_TYPE_RECORD -> group (nest indent_width (format_children node))
  | NK_PAREN_TYPE -> format_children node
  | _ -> format_children node

(** [format_binding node] formats a binding. *)
let format_binding (node : Red_tree.syntax_node) : doc =
  group (nest indent_width (format_children node))

(** [format_structure_item node] formats a structure item. *)
let format_structure_item (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_VALUE_DEFINITION -> group (nest indent_width (format_children node))
  | NK_TYPE_DEFINITION -> group (nest indent_width (format_children node))
  | NK_MODULE_DEFINITION -> group (nest indent_width (format_children node))
  | NK_MODULE_TYPE_DEFINITION ->
      group (nest indent_width (format_children node))
  | NK_OPEN_DECLARATION | NK_INCLUDE_DECLARATION -> format_children node
  | NK_EXTERNAL_DECLARATION ->
      group (nest indent_width (format_children node))
  | _ -> format_children node

(** [format_module_expr node] formats a module expression. *)
let format_module_expr (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_STRUCTURE -> group (nest indent_width (format_children node))
  | NK_MODULE_PATH -> format_children node
  | NK_FUNCTOR_EXPR -> group (nest indent_width (format_children node))
  | NK_MODULE_APPLY -> group (format_children node)
  | NK_MODULE_CONSTRAINT -> format_children node
  | _ -> format_children node

(** [format_signature_item node] formats a signature item. *)
let format_signature_item (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  | NK_VALUE_SPECIFICATION -> group (nest indent_width (format_children node))
  | NK_TYPE_SPECIFICATION -> group (nest indent_width (format_children node))
  | NK_MODULE_SPECIFICATION ->
      group (nest indent_width (format_children node))
  | NK_MODULE_TYPE_SPECIFICATION ->
      group (nest indent_width (format_children node))
  | NK_INCLUDE_SPECIFICATION -> format_children node
  | _ -> format_children node

(** [format_node node] formats any CST node to a doc. *)
let format_node (node : Red_tree.syntax_node) : doc =
  match Red_tree.kind node with
  (* Source file *)
  | NK_SOURCE_FILE -> format_children node
  (* Expressions *)
  | NK_VARIABLE_EXPR | NK_CONSTANT_EXPR | NK_TUPLE_EXPR | NK_CONSTRUCTOR_EXPR
  | NK_APPLY_EXPR | NK_INFIX_EXPR | NK_FUNCTION_EXPR | NK_LET_EXPR | NK_IF_EXPR
  | NK_SEQUENCE_EXPR | NK_CONSTRAINT_EXPR | NK_RECORD_EXPR
  | NK_RECORD_ACCESS_EXPR | NK_RECORD_UPDATE_EXPR | NK_MATCH_EXPR
  | NK_MODULE_ACCESS_EXPR | NK_PAREN_EXPR ->
      format_expression node
  (* Expression components *)
  | NK_RECORD_FIELD -> format_children node
  | NK_MATCH_ARM -> group (nest indent_width (format_children node))
  (* Patterns *)
  | NK_VARIABLE_PATTERN | NK_WILDCARD_PATTERN | NK_CONSTANT_PATTERN
  | NK_TUPLE_PATTERN | NK_CONSTRUCTOR_PATTERN | NK_ALIAS_PATTERN
  | NK_CONSTRAINT_PATTERN | NK_RECORD_PATTERN | NK_PAREN_PATTERN ->
      format_pattern node
  (* Pattern components *)
  | NK_RECORD_PATTERN_FIELD -> format_children node
  (* Types *)
  | NK_TYPE_VARIABLE | NK_TYPE_CONSTRUCTOR | NK_TYPE_TUPLE | NK_TYPE_ARROW
  | NK_TYPE_RECORD | NK_PAREN_TYPE ->
      format_type node
  (* Type components *)
  | NK_TYPE_RECORD_FIELD -> format_children node
  (* Bindings and declarations *)
  | NK_BINDING -> format_binding node
  | NK_TYPE_DECLARATION -> group (nest indent_width (format_children node))
  | NK_CONSTRUCTOR_DECLARATION -> format_children node
  | NK_EXTERNAL_DECLARATION -> format_structure_item node
  (* Structure items *)
  | NK_VALUE_DEFINITION | NK_TYPE_DEFINITION | NK_MODULE_DEFINITION
  | NK_MODULE_TYPE_DEFINITION | NK_OPEN_DECLARATION | NK_INCLUDE_DECLARATION ->
      format_structure_item node
  (* Module expressions *)
  | NK_STRUCTURE | NK_MODULE_PATH | NK_FUNCTOR_EXPR | NK_MODULE_APPLY
  | NK_MODULE_CONSTRAINT ->
      format_module_expr node
  (* Module types *)
  | NK_SIGNATURE -> group (nest indent_width (format_children node))
  | NK_MODULE_TYPE_PATH -> format_children node
  | NK_FUNCTOR_TYPE -> group (nest indent_width (format_children node))
  | NK_MODULE_TYPE_WITH -> format_children node
  (* Signature items *)
  | NK_VALUE_SPECIFICATION | NK_TYPE_SPECIFICATION | NK_MODULE_SPECIFICATION
  | NK_MODULE_TYPE_SPECIFICATION | NK_INCLUDE_SPECIFICATION ->
      format_signature_item node
  (* Module components *)
  | NK_FUNCTOR_PARAMETER -> format_children node
  | NK_WITH_TYPE_CONSTRAINT | NK_WITH_MODULE_CONSTRAINT -> format_children node
  | NK_MODULE_BINDING -> format_children node
  (* Miscellaneous *)
  | NK_LONGIDENT | NK_ATTRIBUTE | NK_ATTRIBUTE_PAYLOAD | NK_TYPE_PARAMETERS
  | NK_ARGUMENT_LIST ->
      format_children node
  (* Error recovery *)
  | NK_ERROR -> format_children node
  (* Fallback for tokens that somehow end up here *)
  | _ -> format_children node

(* Initialize the forward reference *)
let () = format_node_ref := format_node

(** {1 Main Entry Points} *)

(** [format_source_file node] formats a source file CST node.

    @param node The root node (should be NK_SOURCE_FILE)
    @return A doc representing the formatted source *)
let format_source_file (node : Red_tree.syntax_node) : doc = format_node node

(** [format_string ~width content] parses and formats a source string.

    @param width Target line width (default 80)
    @param content The source code to format
    @return The formatted source code as a string *)
let format_string ?(width = 80) content =
  let root = Cst_parser.parse "input" content in
  let d = format_source_file root in
  render ~width d

(** [format_green_node node] formats a green node by wrapping it in a red tree.

    @param node The green node to format
    @return A doc *)
let format_green_node (node : Green_tree.green_node) : doc =
  let red = Red_tree.root node in
  format_node red
