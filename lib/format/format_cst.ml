(** CST-based formatter: formats CST nodes while preserving trivia.

    This module provides formatting functions that work directly on the CST,
    preserving all comments and whitespace in their original positions.
    Unlike the AST-based formatter, this ensures lossless formatting.

    {2 Architecture}

    The formatter is organized into specialized modules:
    - {!Format_common} - Trivia handling, token formatting, blank line detection
    - {!Format_accessors} - Typed child accessors for CST nodes
    - {!Format_expr} - Expression formatting
    - {!Format_pattern} - Pattern formatting
    - {!Format_type} - Type expression formatting
    - {!Format_module} - Module-level formatting (structures, signatures) *)

open Cst
open Doc
open Format_common

(** {1 Forward Reference Initialization}

    The formatting modules have mutual dependencies that are resolved through
    forward references. This section initializes those references. *)

(** Initialize cross-module forward references *)
let () =
  (* Set up Format_common.format_node_ref for generic formatting *)
  let rec format_node node = format_node_impl node
  and format_node_impl node =
    let kind = Red_tree.kind node in
    if Red_tree.is_error node then
      (* For error nodes, preserve original text exactly *)
      text (Red_tree.text node)
    else if Syntax_kind.is_expression kind then
      Format_expr.format_expression node
    else if Syntax_kind.is_pattern kind then
      Format_pattern.format_pattern node
    else if Syntax_kind.is_type kind then
      Format_type.format_type node
    else if Format_accessors.is_structure_item kind then
      Format_module.format_structure_item node
    else if Format_accessors.is_signature_item kind then
      Format_module.format_signature_item node
    else
      format_node_by_kind node
  and format_node_by_kind node =
    match Red_tree.kind node with
    (* Source file *)
    | Syntax_kind.NK_SOURCE_FILE ->
        format_source_file_items node
    (* Expression components *)
    | Syntax_kind.NK_RECORD_FIELD ->
        Format_expr.format_record_field node
    | Syntax_kind.NK_MATCH_ARM ->
        Format_expr.format_match_arm node
    (* Pattern components *)
    | Syntax_kind.NK_RECORD_PATTERN_FIELD ->
        Format_pattern.format_record_pattern_field node
    (* Type components *)
    | Syntax_kind.NK_TYPE_RECORD_FIELD ->
        Format_type.format_type_record_field node
    (* Bindings and declarations *)
    | Syntax_kind.NK_BINDING ->
        Format_module.format_binding node
    | Syntax_kind.NK_TYPE_DECLARATION ->
        Format_module.format_type_declaration node
    | Syntax_kind.NK_CONSTRUCTOR_DECLARATION ->
        Format_module.format_constructor_declaration node
    (* Module expressions *)
    | Syntax_kind.NK_STRUCTURE ->
        Format_module.format_structure node
    | Syntax_kind.NK_MODULE_PATH ->
        format_children node
    | Syntax_kind.NK_FUNCTOR_EXPR ->
        Format_module.format_functor_expr node
    | Syntax_kind.NK_MODULE_APPLY ->
        Format_module.format_module_apply node
    | Syntax_kind.NK_MODULE_CONSTRAINT ->
        Format_module.format_module_constraint node
    (* Module types *)
    | Syntax_kind.NK_SIGNATURE ->
        Format_module.format_signature node
    | Syntax_kind.NK_MODULE_TYPE_PATH | Syntax_kind.NK_FUNCTOR_TYPE
    | Syntax_kind.NK_MODULE_TYPE_WITH ->
        format_children node
    (* Module components *)
    | Syntax_kind.NK_FUNCTOR_PARAMETER ->
        Format_module.format_functor_parameter node
    | Syntax_kind.NK_WITH_TYPE_CONSTRAINT | Syntax_kind.NK_WITH_MODULE_CONSTRAINT
    | Syntax_kind.NK_MODULE_BINDING ->
        format_children node
    (* Miscellaneous *)
    | Syntax_kind.NK_LONGIDENT | Syntax_kind.NK_ATTRIBUTE
    | Syntax_kind.NK_ATTRIBUTE_PAYLOAD | Syntax_kind.NK_TYPE_PARAMETERS
    | Syntax_kind.NK_ARGUMENT_LIST ->
        format_children node
    (* Error recovery and fallback *)
    | Syntax_kind.NK_ERROR ->
        format_children node
    | _ ->
        format_children node

  and format_source_file_items node =
    let items = Red_tree.child_nodes node in
    let items_doc = Format_module.format_structure_items items in
    (* Also format the EOF token to capture trailing trivia (comments, newlines) *)
    let eof_doc =
      match Red_tree.child_tokens node |> List.rev with
      | [] -> empty
      | eof :: _ when Syntax_kind.equal eof.green.kind Syntax_kind.TK_EOF ->
          format_token eof
      | _ -> empty
    in
    items_doc ^^ eof_doc
  in

  (* Initialize the forward reference in Format_common *)
  format_node_ref := format_node;

  (* Initialize cross-module references for patterns and types *)
  Format_expr.format_pattern_ref := Format_pattern.format_pattern;
  Format_expr.format_type_ref := Format_type.format_type;
  Format_pattern.format_type_ref := Format_type.format_type;

  (* Initialize references in Format_module for expressions/patterns/types *)
  Format_module.format_expression_ref := Format_expr.format_expression;
  Format_module.format_pattern_ref := Format_pattern.format_pattern;
  Format_module.format_type_ref := Format_type.format_type

(** {1 Exported Formatters} *)

(** [format_node node] formats any CST node to a doc.

    This is the main entry point for formatting individual nodes.
    It dispatches to the appropriate semantic formatter based on node kind. *)
let format_node node = !format_node_ref node

(** [format_expression node] formats an expression node. *)
let format_expression = Format_expr.format_expression

(** [format_pattern node] formats a pattern node. *)
let format_pattern = Format_pattern.format_pattern

(** [format_type node] formats a type node. *)
let format_type = Format_type.format_type

(** [format_binding node] formats a binding. *)
let format_binding = Format_module.format_binding

(** [format_structure_item node] formats a structure item. *)
let format_structure_item = Format_module.format_structure_item

(** [format_module_expr node] formats a module expression. *)
let format_module_expr = Format_module.format_module_expr

(** [format_signature_item node] formats a signature item. *)
let format_signature_item = Format_module.format_signature_item

(** {1 Main Entry Points} *)

(** [format_source_file node] formats a source file CST node.

    @param node The root node (should be NK_SOURCE_FILE)
    @return A doc representing the formatted source *)
let format_source_file (node : Red_tree.syntax_node) : doc = format_node node

(** [format_string ~width ~indent content] parses and formats a source string.

    @param width Target line width (default 80)
    @param indent Spaces per indentation level (default 2)
    @param content The source code to format
    @return The formatted source code as a string *)
let format_string ?(width = 80) ?(indent = 2) content =
  (* Set the indent width for this formatting run *)
  let old_indent = indent_width () in
  set_indent_width indent;
  (* Parse and format *)
  let root = Cst_parser.parse "input" content in
  let d = format_source_file root in
  let result = render ~width d in
  (* Restore the old indent width *)
  set_indent_width old_indent;
  (* Ensure trailing newline *)
  if String.length result > 0 && result.[String.length result - 1] <> '\n' then
    result ^ "\n"
  else
    result

(** [format_green_node node] formats a green node by wrapping it in a red tree.

    @param node The green node to format
    @return A doc *)
let format_green_node (node : Green_tree.green_node) : doc =
  let red = Red_tree.root node in
  format_node red
