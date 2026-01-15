(** Pattern formatting with semantic awareness.

    This module provides semantic formatters for pattern nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** Use centralized forward ref from Format_common *)
let format_type = format_type

(** {1 Specific Formatters} *)

let rec format_tuple_pattern node =
  format_tuple_like
    ~lparen_tok:Tuple_pattern.lparen
    ~rparen_tok:Tuple_pattern.rparen
    ~elements:Tuple_pattern.elements
    ~formatter:format_pattern
    node

and format_constructor_pattern node =
  match Constructor_pattern.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      (match Constructor_pattern.argument node with
       | Some arg ->
           let arg_doc = format_pattern arg in
           group (name_doc ^^ arg_doc)
       | None -> name_doc)
  | None -> format_children node

and format_record_pattern node =
  format_braced
    ~lbrace_tok:Record_pattern.lbrace
    ~rbrace_tok:Record_pattern.rbrace
    ~fields:Record_pattern.fields
    ~format_field:format_record_pattern_field
    node

and format_record_pattern_field node =
  match Record_pattern_field.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      (match Record_pattern_field.equals node, Record_pattern_field.pattern node
       with
       | Some eq, Some pat ->
           let eq_doc = format_token eq in
           let pat_doc = format_pattern pat in
           group (name_doc ^^ eq_doc ^^ pat_doc)
       | _ ->
           (* Punning: just the name *)
           name_doc)
  | None -> format_children node

and format_alias_pattern node =
  with_required3
    (Alias_pattern.pattern node)
    (Alias_pattern.as_keyword node)
    (Alias_pattern.name node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun pat as_kw name ->
      let pat_doc = format_pattern pat in
      let as_doc = format_token as_kw in
      let name_doc = format_token name in
      group (pat_doc ^^ as_doc ^^ name_doc))

and format_constraint_pattern node =
  with_required3
    (Constraint_pattern.pattern node)
    (Constraint_pattern.colon node)
    (Constraint_pattern.type_expr node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun pat colon_tok ty ->
      let pat_doc = format_pattern pat in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      group (pat_doc ^^ colon_doc ^^ ty_doc))

and format_paren_pattern node =
  format_parens
    ~lparen_tok:Paren_pattern.lparen
    ~rparen_tok:Paren_pattern.rparen
    ~inner:Paren_pattern.inner
    ~formatter:format_pattern
    node

(** {1 Main Dispatcher} *)

and format_pattern node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_TUPLE_PATTERN -> format_tuple_pattern node
    | Syntax_kind.NK_CONSTRUCTOR_PATTERN -> format_constructor_pattern node
    | Syntax_kind.NK_RECORD_PATTERN -> format_record_pattern node
    | Syntax_kind.NK_ALIAS_PATTERN -> format_alias_pattern node
    | Syntax_kind.NK_CONSTRAINT_PATTERN -> format_constraint_pattern node
    | Syntax_kind.NK_PAREN_PATTERN -> format_paren_pattern node
    | Syntax_kind.NK_VARIABLE_PATTERN
    | Syntax_kind.NK_WILDCARD_PATTERN
    | Syntax_kind.NK_CONSTANT_PATTERN ->
        format_children node
    | _ -> format_children node)
