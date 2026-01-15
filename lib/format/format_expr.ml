(** Expression formatting with semantic awareness.

    This module provides semantic formatters for expression nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** {1 Internal Helpers} *)

(** Use centralized forward refs from Format_common *)
let format_pattern = format_pattern
let format_type = format_type

(** {1 Specific Formatters} *)

let rec format_let_expr node =
  with_required3
    (Let_expr.let_keyword node)
    (Let_expr.in_keyword node)
    (Let_expr.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun let_kw in_kw body ->
      let let_doc = format_token let_kw in
      let rec_doc = format_optional_token (Let_expr.rec_keyword node) in
      let bindings_doc = format_node_list format_binding (Let_expr.bindings node) in
      let in_doc = format_token in_kw in
      let body_doc = format_expression body in
      group (let_doc ^^ rec_doc ^^ bindings_doc ^^ in_doc ^^ body_doc))

and format_binding node =
  with_required3
    (Binding.pattern node)
    (Binding.equals node)
    (Binding.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun pat eq body ->
      let pat_doc = format_pattern pat in
      let ty_doc =
        match Binding.type_annotation node with
        | Some ty -> format_type ty
        | None -> empty
      in
      let eq_doc = format_token eq in
      let body_doc = format_expression body in
      group (pat_doc ^^ ty_doc ^^ eq_doc ^^ body_doc))

and format_if_expr node =
  with_required4
    (If_expr.if_keyword node)
    (If_expr.condition node)
    (If_expr.then_keyword node)
    (If_expr.then_branch node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun if_kw cond then_kw then_br ->
      let if_doc = format_token if_kw in
      let cond_doc = format_expression cond in
      let then_doc = format_token then_kw in
      let then_body = format_expression then_br in
      let else_part =
        match If_expr.else_keyword node, If_expr.else_branch node with
        | Some else_kw, Some else_br ->
            let else_doc = format_token else_kw in
            let else_body = format_expression else_br in
            else_doc ^^ else_body
        | _ -> empty
      in
      group (if_doc ^^ cond_doc ^^ then_doc ^^ then_body ^^ else_part))

and format_match_expr node =
  with_required3
    (Match_expr.match_keyword node)
    (Match_expr.scrutinee node)
    (Match_expr.with_keyword node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun match_kw scrut with_kw ->
      let match_doc = format_token match_kw in
      let scrut_doc = format_expression scrut in
      let with_doc = format_token with_kw in
      let arms_doc = format_node_list format_match_arm (Match_expr.arms node) in
      group (match_doc ^^ scrut_doc ^^ with_doc ^^ arms_doc))

and format_match_arm node =
  with_required3
    (Match_arm.pattern node)
    (Match_arm.arrow node)
    (Match_arm.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun pat arrow_tok body ->
      let bar_doc = format_optional_token (Match_arm.bar node) in
      let pat_doc = format_pattern pat in
      let guard_doc =
        match Match_arm.when_keyword node, Match_arm.guard node with
        | Some when_kw, Some guard ->
            format_token when_kw ^^ format_expression guard
        | _ -> empty
      in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_expression body in
      group (bar_doc ^^ pat_doc ^^ guard_doc ^^ arrow_doc ^^ body_doc))

and format_function_expr node =
  with_required3
    (Function_expr.fun_keyword node)
    (Function_expr.arrow node)
    (Function_expr.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun fun_kw arrow_tok body ->
      let fun_doc = format_token fun_kw in
      let params_doc = format_node_list format_pattern (Function_expr.parameters node) in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_expression body in
      group (fun_doc ^^ params_doc ^^ arrow_doc ^^ body_doc))

and format_apply_expr node =
  match Apply_expr.func node with
  | Some func ->
      let func_doc = format_expression func in
      let args = Apply_expr.arguments node in
      let args_doc =
        match args with
        | [] -> empty
        | _ ->
            let formatted = List.map format_expression args in
            concat_list formatted
      in
      group (func_doc ^^ args_doc)
  | None -> format_children node

and format_infix_expr node =
  with_required3
    (Infix_expr.left node)
    (Infix_expr.operator node)
    (Infix_expr.right node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun left op right ->
      let left_doc = format_expression left in
      let op_doc = format_token op in
      let right_doc = format_expression right in
      group (left_doc ^^ op_doc ^^ right_doc))

and format_tuple_expr node =
  format_tuple_like
    ~lparen_tok:Tuple_expr.lparen
    ~rparen_tok:Tuple_expr.rparen
    ~elements:Tuple_expr.elements
    ~formatter:format_expression
    node

and format_record_expr node =
  format_braced
    ~lbrace_tok:Record_expr.lbrace
    ~rbrace_tok:Record_expr.rbrace
    ~fields:Record_expr.fields
    ~format_field:format_record_field
    node

and format_record_field node =
  match Record_field.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      (match Record_field.equals node, Record_field.value node with
       | Some eq, Some value ->
           let eq_doc = format_token eq in
           let value_doc = format_expression value in
           group (name_doc ^^ eq_doc ^^ value_doc)
       | _ ->
           (* Punning: just the name *)
           name_doc)
  | None -> format_children node

and format_record_update node =
  match
    ( Record_update.lbrace node,
      Record_update.base node,
      Record_update.with_keyword node,
      Record_update.rbrace node )
  with
  | Some lbrace_tok, Some base, Some with_kw, Some rbrace_tok ->
      let lbrace_doc = format_token lbrace_tok in
      let base_doc = format_expression base in
      let with_doc = format_token with_kw in
      let rbrace_doc = format_token rbrace_tok in
      let fields = Record_update.fields node in
      let fields_doc =
        match fields with
        | [] -> empty
        | _ ->
            let formatted = List.map format_record_field fields in
            concat_list formatted
      in
      group (lbrace_doc ^^ base_doc ^^ with_doc ^^ fields_doc ^^ rbrace_doc)
  | _ -> format_children node

and format_paren_expr node =
  format_parens
    ~lparen_tok:Paren_expr.lparen
    ~rparen_tok:Paren_expr.rparen
    ~inner:Paren_expr.inner
    ~formatter:format_expression
    node

and format_constraint_expr node =
  with_required3
    (Constraint_expr.expr node)
    (Constraint_expr.colon node)
    (Constraint_expr.type_expr node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun expr colon_tok ty ->
      let expr_doc = format_expression expr in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      group (expr_doc ^^ colon_doc ^^ ty_doc))

and format_sequence_expr node =
  with_required3
    (Sequence_expr.first node)
    (Sequence_expr.semicolon node)
    (Sequence_expr.second node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun first semi_tok second ->
      let first_doc = format_expression first in
      let semi_doc = format_token semi_tok in
      let second_doc = format_expression second in
      group (first_doc ^^ semi_doc ^^ second_doc))

and format_record_access node =
  with_required3
    (Record_access.expr node)
    (Record_access.dot node)
    (Record_access.field node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun expr dot_tok field ->
      let expr_doc = format_expression expr in
      let dot_doc = format_token dot_tok in
      let field_doc = format_token field in
      expr_doc ^^ dot_doc ^^ field_doc)

and format_module_access node =
  (* Module access: Module.value - just concatenate children *)
  format_children node

and format_constructor_expr node =
  match Constructor_expr.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      (match Constructor_expr.argument node with
       | Some arg ->
           let arg_doc = format_expression arg in
           group (name_doc ^^ arg_doc)
       | None -> name_doc)
  | None -> format_children node

(** {1 Main Dispatcher} *)

and format_expression node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_LET_EXPR -> format_let_expr node
    | Syntax_kind.NK_IF_EXPR -> format_if_expr node
    | Syntax_kind.NK_MATCH_EXPR -> format_match_expr node
    | Syntax_kind.NK_FUNCTION_EXPR -> format_function_expr node
    | Syntax_kind.NK_APPLY_EXPR -> format_apply_expr node
    | Syntax_kind.NK_INFIX_EXPR -> format_infix_expr node
    | Syntax_kind.NK_TUPLE_EXPR -> format_tuple_expr node
    | Syntax_kind.NK_RECORD_EXPR -> format_record_expr node
    | Syntax_kind.NK_RECORD_UPDATE_EXPR -> format_record_update node
    | Syntax_kind.NK_RECORD_ACCESS_EXPR -> format_record_access node
    | Syntax_kind.NK_PAREN_EXPR -> format_paren_expr node
    | Syntax_kind.NK_CONSTRAINT_EXPR -> format_constraint_expr node
    | Syntax_kind.NK_SEQUENCE_EXPR -> format_sequence_expr node
    | Syntax_kind.NK_MODULE_ACCESS_EXPR -> format_module_access node
    | Syntax_kind.NK_CONSTRUCTOR_EXPR -> format_constructor_expr node
    | Syntax_kind.NK_VARIABLE_EXPR
    | Syntax_kind.NK_CONSTANT_EXPR ->
        format_children node
    | _ -> format_children node)
