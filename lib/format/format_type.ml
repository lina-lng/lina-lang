(** Type expression formatting with semantic awareness.

    This module provides semantic formatters for type expression nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** {1 Specific Formatters} *)

let rec format_type_arrow node =
  with_required3
    (Type_arrow.domain node)
    (Type_arrow.arrow node)
    (Type_arrow.codomain node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun domain arrow_tok codomain ->
      let domain_doc = format_type domain in
      let arrow_doc = format_token arrow_tok in
      let codomain_doc = format_type codomain in
      group (domain_doc ^^ arrow_doc ^^ codomain_doc))

and format_type_tuple node =
  (* Use format_children to include the * separators *)
  format_children node

and format_type_constructor node =
  (* Type constructor: 'a option, ('a, 'b) result, int *)
  (* Just format children for now - this preserves the structure *)
  format_children node

and format_type_record node =
  format_braced
    ~lbrace_tok:Type_record.lbrace
    ~rbrace_tok:Type_record.rbrace
    ~fields:Type_record.fields
    ~format_field:format_type_record_field
    node

and format_type_record_field node =
  with_required3
    (Type_record_field.name node)
    (Type_record_field.colon node)
    (Type_record_field.type_expr node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun name_tok colon_tok ty ->
      let name_doc = format_token name_tok in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      group (name_doc ^^ colon_doc ^^ ty_doc))

and format_paren_type node =
  format_parens
    ~lparen_tok:Paren_type.lparen
    ~rparen_tok:Paren_type.rparen
    ~inner:Paren_type.inner
    ~formatter:format_type
    node

(** {1 Main Dispatcher} *)

and format_type node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_TYPE_ARROW -> format_type_arrow node
    | Syntax_kind.NK_TYPE_TUPLE -> format_type_tuple node
    | Syntax_kind.NK_TYPE_CONSTRUCTOR -> format_type_constructor node
    | Syntax_kind.NK_TYPE_RECORD -> format_type_record node
    | Syntax_kind.NK_PAREN_TYPE -> format_paren_type node
    | Syntax_kind.NK_TYPE_VARIABLE -> format_children node
    | _ -> format_children node)
