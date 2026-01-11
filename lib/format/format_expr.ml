(** Expression formatting with semantic awareness.

    This module provides semantic formatters for expression nodes. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** Forward declarations for mutual recursion *)
let format_pattern_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_type_ref : (Red_tree.syntax_node -> doc) ref = ref (fun _ -> empty)

(** {1 Internal Helpers} *)

let format_pattern node = !format_pattern_ref node
let format_type node = !format_type_ref node

(** {1 Specific Formatters} *)

let rec format_let_expr node =
  match Let_expr.let_keyword node, Let_expr.in_keyword node, Let_expr.body node
  with
  | Some let_kw, Some in_kw, Some body ->
      let let_doc = format_token let_kw in
      let rec_doc =
        match Let_expr.rec_keyword node with
        | Some rec_kw -> format_token rec_kw
        | None -> empty
      in
      let bindings = Let_expr.bindings node in
      let bindings_doc =
        match bindings with
        | [] -> empty
        | _ ->
            let formatted =
              List.map
                (fun binding -> format_binding binding)
                bindings
            in
            concat_list formatted
      in
      let in_doc = format_token in_kw in
      let body_doc = format_expression body in
      (* Use ^^ since trivia already has spacing *)
      group (let_doc ^^ rec_doc ^^ bindings_doc ^^ in_doc ^^ body_doc)
  | _ -> format_children node

and format_binding node =
  match Binding.pattern node, Binding.equals node, Binding.body node with
  | Some pat, Some eq, Some body ->
      let pat_doc = format_pattern pat in
      let ty_doc =
        match Binding.type_annotation node with
        | Some ty -> format_type ty
        | None -> empty
      in
      let eq_doc = format_token eq in
      let body_doc = format_expression body in
      (* Use ^^ since trivia already has spacing *)
      group (pat_doc ^^ ty_doc ^^ eq_doc ^^ body_doc)
  | _ -> format_children node

and format_if_expr node =
  match
    ( If_expr.if_keyword node,
      If_expr.condition node,
      If_expr.then_keyword node,
      If_expr.then_branch node )
  with
  | Some if_kw, Some cond, Some then_kw, Some then_br ->
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
      (* Use ^^ since trivia already has spacing *)
      group (if_doc ^^ cond_doc ^^ then_doc ^^ then_body ^^ else_part)
  | _ -> format_children node

and format_match_expr node =
  match
    Match_expr.match_keyword node, Match_expr.scrutinee node, Match_expr.with_keyword node
  with
  | Some match_kw, Some scrut, Some with_kw ->
      let match_doc = format_token match_kw in
      let scrut_doc = format_expression scrut in
      let with_doc = format_token with_kw in
      let arms = Match_expr.arms node in
      let arms_doc =
        match arms with
        | [] -> empty
        | _ ->
            let formatted = List.map format_match_arm arms in
            concat_list formatted
      in
      (* Use ^^ since trivia already has spacing *)
      group (match_doc ^^ scrut_doc ^^ with_doc ^^ arms_doc)
  | _ -> format_children node

and format_match_arm node =
  match Match_arm.pattern node, Match_arm.arrow node, Match_arm.body node with
  | Some pat, Some arrow_tok, Some body ->
      let bar_doc =
        match Match_arm.bar node with
        | Some bar -> format_token bar
        | None -> empty  (* No bar = no bar (first arm without explicit bar) *)
      in
      let pat_doc = format_pattern pat in
      let guard_doc =
        match Match_arm.when_keyword node, Match_arm.guard node with
        | Some when_kw, Some guard ->
            format_token when_kw ^^ format_expression guard
        | _ -> empty
      in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_expression body in
      (* Use ^^ since trivia already has spacing *)
      group (bar_doc ^^ pat_doc ^^ guard_doc ^^ arrow_doc ^^ body_doc)
  | _ -> format_children node

and format_function_expr node =
  match Function_expr.fun_keyword node, Function_expr.arrow node, Function_expr.body node
  with
  | Some fun_kw, Some arrow_tok, Some body ->
      let fun_doc = format_token fun_kw in
      let params = Function_expr.parameters node in
      let params_doc =
        match params with
        | [] -> empty
        | _ ->
            let formatted = List.map format_pattern params in
            concat_list formatted
      in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_expression body in
      (* Use ^^ since trivia already has spacing *)
      group (fun_doc ^^ params_doc ^^ arrow_doc ^^ body_doc)
  | _ -> format_children node

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
      (* Use ^^ since trivia already has spacing *)
      group (func_doc ^^ args_doc)
  | None -> format_children node

and format_infix_expr node =
  match
    Infix_expr.left node, Infix_expr.operator node, Infix_expr.right node
  with
  | Some left, Some op, Some right ->
      let left_doc = format_expression left in
      (* Use format_token which includes trivia - this handles spacing *)
      let op_doc = format_token op in
      let right_doc = format_expression right in
      (* Just concatenate - trivia already has the spacing *)
      group (left_doc ^^ op_doc ^^ right_doc)
  | _ -> format_children node

and format_tuple_expr node =
  let lparen_doc =
    match Tuple_expr.lparen node with
    | Some tok -> format_token tok
    | None -> lparen
  in
  let rparen_doc =
    match Tuple_expr.rparen node with
    | Some tok -> format_token tok
    | None -> rparen
  in
  let elements = Tuple_expr.elements node in
  match elements with
  | [] -> lparen_doc ^^ rparen_doc
  | _ ->
      let formatted = List.map format_expression elements in
      let elements_doc = format_separated (comma ^^ space) formatted in
      group (lparen_doc ^^ elements_doc ^^ rparen_doc)

and format_record_expr node =
  match Record_expr.lbrace node, Record_expr.rbrace node with
  | Some lbrace_tok, Some rbrace_tok ->
      let lbrace_doc = format_token lbrace_tok in
      let rbrace_doc = format_token rbrace_tok in
      let fields = Record_expr.fields node in
      (match fields with
       | [] -> lbrace_doc ^^ rbrace_doc
       | _ ->
           let formatted = List.map format_record_field fields in
           let fields_doc = format_separated (semi ^^ space) formatted in
           (* Use block combinator for { ... } *)
           block lbrace_doc fields_doc rbrace_doc)
  | _ -> format_children node

and format_record_field node =
  match Record_field.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      (match Record_field.equals node, Record_field.value node with
       | Some eq, Some value ->
           let eq_doc = format_token eq in
           let value_doc = format_expression value in
           (* Use ^^ since trivia already has spacing *)
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
      (* Use ^^ since trivia already has spacing *)
      group (lbrace_doc ^^ base_doc ^^ with_doc ^^ fields_doc ^^ rbrace_doc)
  | _ -> format_children node

and format_paren_expr node =
  match Paren_expr.lparen node, Paren_expr.inner node, Paren_expr.rparen node
  with
  | Some lp, Some inner, Some rp ->
      let lp_doc = format_token lp in
      let inner_doc = format_expression inner in
      let rp_doc = format_token rp in
      group (lp_doc ^^ inner_doc ^^ rp_doc)
  | _ -> format_children node

and format_constraint_expr node =
  match
    Constraint_expr.expr node, Constraint_expr.colon node, Constraint_expr.type_expr node
  with
  | Some expr, Some colon_tok, Some ty ->
      let expr_doc = format_expression expr in
      let colon_doc = format_token colon_tok in
      let ty_doc = format_type ty in
      (* Use ^^ since trivia already has spacing *)
      group (expr_doc ^^ colon_doc ^^ ty_doc)
  | _ -> format_children node

and format_sequence_expr node =
  match
    Sequence_expr.first node, Sequence_expr.semicolon node, Sequence_expr.second node
  with
  | Some first, Some semi_tok, Some second ->
      let first_doc = format_expression first in
      let semi_doc = format_token semi_tok in
      let second_doc = format_expression second in
      (* Use ^^ since trivia already has spacing/newlines *)
      group (first_doc ^^ semi_doc ^^ second_doc)
  | _ -> format_children node

and format_record_access node =
  match
    Record_access.expr node, Record_access.dot node, Record_access.field node
  with
  | Some expr, Some dot_tok, Some field ->
      let expr_doc = format_expression expr in
      let dot_doc = format_token dot_tok in
      let field_doc = format_token field in
      expr_doc ^^ dot_doc ^^ field_doc
  | _ -> format_children node

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
           (* Use ^^ since trivia already has spacing *)
           group (name_doc ^^ arg_doc)
       | None -> name_doc)
  | None -> format_children node

(** {1 Main Dispatcher} *)

and format_expression node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then
    (* For error nodes, preserve original text *)
    text (Red_tree.text node)
  else if Red_tree.has_errors node then
    (* When tree has errors, be conservative *)
    format_children node
  else
    match kind with
    (* Let expressions *)
    | Syntax_kind.NK_LET_EXPR -> format_let_expr node
    (* Conditionals *)
    | Syntax_kind.NK_IF_EXPR -> format_if_expr node
    (* Pattern matching *)
    | Syntax_kind.NK_MATCH_EXPR -> format_match_expr node
    | Syntax_kind.NK_FUNCTION_EXPR -> format_function_expr node
    (* Function application *)
    | Syntax_kind.NK_APPLY_EXPR -> format_apply_expr node
    (* Infix operators *)
    | Syntax_kind.NK_INFIX_EXPR -> format_infix_expr node
    (* Tuples *)
    | Syntax_kind.NK_TUPLE_EXPR -> format_tuple_expr node
    (* Records *)
    | Syntax_kind.NK_RECORD_EXPR -> format_record_expr node
    | Syntax_kind.NK_RECORD_UPDATE_EXPR -> format_record_update node
    | Syntax_kind.NK_RECORD_ACCESS_EXPR -> format_record_access node
    (* Parenthesized *)
    | Syntax_kind.NK_PAREN_EXPR -> format_paren_expr node
    (* Type constraint *)
    | Syntax_kind.NK_CONSTRAINT_EXPR -> format_constraint_expr node
    (* Sequence *)
    | Syntax_kind.NK_SEQUENCE_EXPR -> format_sequence_expr node
    (* Module access *)
    | Syntax_kind.NK_MODULE_ACCESS_EXPR -> format_module_access node
    (* Constructor *)
    | Syntax_kind.NK_CONSTRUCTOR_EXPR -> format_constructor_expr node
    (* Atoms - use format_children for simple tokens *)
    | Syntax_kind.NK_VARIABLE_EXPR
    | Syntax_kind.NK_CONSTANT_EXPR ->
        format_children node
    (* Fallback for any other expressions *)
    | _ -> format_children node
