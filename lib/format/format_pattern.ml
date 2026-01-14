(** Pattern formatting with semantic awareness.

    This module provides semantic formatters for pattern nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** Forward declaration for type formatting *)
let format_type_ref : (Red_tree.syntax_node -> doc) ref = ref (fun _ -> empty)

let format_type node = !format_type_ref node

(** {1 Specific Formatters} *)

let rec format_tuple_pattern node =
  let lparen_doc =
    match Tuple_pattern.lparen node with
    | Some tok -> format_token tok
    | None -> lparen
  in
  let rparen_doc =
    match Tuple_pattern.rparen node with
    | Some tok -> format_token tok
    | None -> rparen
  in
  let elements = Tuple_pattern.elements node in
  match elements with
  | [] -> lparen_doc ^^ rparen_doc
  | _ ->
      let formatted = List.map format_pattern elements in
      let elements_doc = format_separated (comma ^^ space) formatted in
      group (lparen_doc ^^ elements_doc ^^ rparen_doc)

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
  match Record_pattern.lbrace node, Record_pattern.rbrace node with
  | Some lbrace_tok, Some rbrace_tok ->
      let lbrace_doc = format_token lbrace_tok in
      let rbrace_doc = format_token rbrace_tok in
      let fields = Record_pattern.fields node in
      (match fields with
       | [] -> lbrace_doc ^^ rbrace_doc
       | _ ->
           let formatted = List.map format_record_pattern_field fields in
           let fields_doc = format_separated (semi ^^ space) formatted in
           block lbrace_doc fields_doc rbrace_doc)
  | _ -> format_children node

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
  match
    Alias_pattern.pattern node, Alias_pattern.as_keyword node, Alias_pattern.name node
  with
  | Some pat, Some as_kw, Some name ->
      let pat_doc = format_pattern pat in
      let as_doc = format_token as_kw in
      let name_doc = format_token name in
      group (pat_doc ^^ as_doc ^^ name_doc)
  | _ -> format_children node

and format_constraint_pattern node =
  match
    Constraint_pattern.pattern node,
    Constraint_pattern.colon node,
    Constraint_pattern.type_expr node
  with
  | Some pat, Some colon_tok, Some ty ->
      let pat_doc = format_pattern pat in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      group (pat_doc ^^ colon_doc ^^ ty_doc)
  | _ -> format_children node

and format_paren_pattern node =
  match
    Paren_pattern.lparen node, Paren_pattern.inner node, Paren_pattern.rparen node
  with
  | Some lp, Some inner, Some rp ->
      let lp_doc = format_token lp in
      let inner_doc = format_pattern inner in
      let rp_doc = format_token rp in
      group (lp_doc ^^ inner_doc ^^ rp_doc)
  | _ -> format_children node

(** {1 Main Dispatcher} *)

and format_pattern node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then text (Red_tree.text node)
  else if Red_tree.has_errors node then format_children node
  else
    match kind with
    | Syntax_kind.NK_TUPLE_PATTERN -> format_tuple_pattern node
    | Syntax_kind.NK_CONSTRUCTOR_PATTERN -> format_constructor_pattern node
    | Syntax_kind.NK_RECORD_PATTERN -> format_record_pattern node
    | Syntax_kind.NK_ALIAS_PATTERN -> format_alias_pattern node
    | Syntax_kind.NK_CONSTRAINT_PATTERN -> format_constraint_pattern node
    | Syntax_kind.NK_PAREN_PATTERN -> format_paren_pattern node
    (* Simple patterns - just format children *)
    | Syntax_kind.NK_VARIABLE_PATTERN
    | Syntax_kind.NK_WILDCARD_PATTERN
    | Syntax_kind.NK_CONSTANT_PATTERN ->
        format_children node
    | _ -> format_children node
