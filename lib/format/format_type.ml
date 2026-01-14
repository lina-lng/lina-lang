(** Type expression formatting with semantic awareness.

    This module provides semantic formatters for type expression nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** {1 Specific Formatters} *)

let rec format_type_arrow node =
  match
    Type_arrow.domain node, Type_arrow.arrow node, Type_arrow.codomain node
  with
  | Some domain, Some arrow_tok, Some codomain ->
      let domain_doc = format_type domain in
      let arrow_doc = format_token arrow_tok in
      let codomain_doc = format_type codomain in
      group (domain_doc ^^ arrow_doc ^^ codomain_doc)
  | _ -> format_children node

and format_type_tuple node =
  (* Use format_children to include the * separators *)
  format_children node

and format_type_constructor node =
  (* Type constructor: 'a option, ('a, 'b) result, int *)
  (* Just format children for now - this preserves the structure *)
  format_children node

and format_type_record node =
  match Type_record.lbrace node, Type_record.rbrace node with
  | Some lbrace_tok, Some rbrace_tok ->
      let lbrace_doc = format_token lbrace_tok in
      let rbrace_doc = format_token rbrace_tok in
      let fields = Type_record.fields node in
      (match fields with
       | [] -> lbrace_doc ^^ rbrace_doc
       | _ ->
           let formatted = List.map format_type_record_field fields in
           let fields_doc = format_separated (semi ^^ space) formatted in
           block lbrace_doc fields_doc rbrace_doc)
  | _ -> format_children node

and format_type_record_field node =
  match
    Type_record_field.name node,
    Type_record_field.colon node,
    Type_record_field.type_expr node
  with
  | Some name_tok, Some colon_tok, Some ty ->
      let name_doc = format_token name_tok in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      group (name_doc ^^ colon_doc ^^ ty_doc)
  | _ -> format_children node

and format_paren_type node =
  match
    Paren_type.lparen node, Paren_type.inner node, Paren_type.rparen node
  with
  | Some lp, Some inner, Some rp ->
      let lp_doc = format_token lp in
      let inner_doc = format_type inner in
      let rp_doc = format_token rp in
      group (lp_doc ^^ inner_doc ^^ rp_doc)
  | _ -> format_children node

(** {1 Main Dispatcher} *)

and format_type node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then text (Red_tree.text node)
  else if Red_tree.has_errors node then format_children node
  else
    match kind with
    | Syntax_kind.NK_TYPE_ARROW -> format_type_arrow node
    | Syntax_kind.NK_TYPE_TUPLE -> format_type_tuple node
    | Syntax_kind.NK_TYPE_CONSTRUCTOR -> format_type_constructor node
    | Syntax_kind.NK_TYPE_RECORD -> format_type_record node
    | Syntax_kind.NK_PAREN_TYPE -> format_paren_type node
    (* Simple types - just format children *)
    | Syntax_kind.NK_TYPE_VARIABLE -> format_children node
    | _ -> format_children node
