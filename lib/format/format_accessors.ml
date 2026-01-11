(** Typed child accessors for CST nodes.

    This module provides type-safe accessors for extracting children from CST
    nodes by their semantic role rather than position. *)

open Cst

(** {1 Internal Helpers} *)

let find_token kind node = Red_tree.find_child_token node kind
let find_node kind node = Red_tree.find_child_node node kind

let filter_nodes kind node =
  Red_tree.child_nodes node
  |> List.filter (fun child -> Syntax_kind.equal (Red_tree.kind child) kind)

let filter_expression_nodes node =
  Red_tree.child_nodes node
  |> List.filter (fun child -> Syntax_kind.is_expression (Red_tree.kind child))

let filter_pattern_nodes node =
  Red_tree.child_nodes node
  |> List.filter (fun child -> Syntax_kind.is_pattern (Red_tree.kind child))

let filter_type_nodes node =
  Red_tree.child_nodes node
  |> List.filter (fun child -> Syntax_kind.is_type (Red_tree.kind child))

let opt_exn accessor node =
  match accessor node with
  | Some value -> value
  | None -> raise Not_found

(** {1 Expression Accessors} *)

module Let_expr = struct
  let let_keyword node = find_token Syntax_kind.TK_LET node
  let let_keyword_exn node = opt_exn let_keyword node

  let rec_keyword node = find_token Syntax_kind.TK_REC node

  let bindings node = filter_nodes Syntax_kind.NK_BINDING node

  let in_keyword node = find_token Syntax_kind.TK_IN node
  let in_keyword_exn node = opt_exn in_keyword node

  let body node =
    (* Body is the last expression child (after 'in') *)
    let exprs = filter_expression_nodes node in
    match List.rev exprs with
    | body :: _ -> Some body
    | [] -> None

  let body_exn node = opt_exn body node
end

module If_expr = struct
  let if_keyword node = find_token Syntax_kind.TK_IF node
  let if_keyword_exn node = opt_exn if_keyword node

  let then_keyword node = find_token Syntax_kind.TK_THEN node
  let then_keyword_exn node = opt_exn then_keyword node

  let else_keyword node = find_token Syntax_kind.TK_ELSE node

  (* For if expressions, children are: condition, then_branch, else_branch (optional) *)
  let condition node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | cond :: _ -> Some cond
    | [] -> None

  let condition_exn node = opt_exn condition node

  let then_branch node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | _ :: then_br :: _ -> Some then_br
    | _ -> None

  let then_branch_exn node = opt_exn then_branch node

  let else_branch node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | _ :: _ :: else_br :: _ -> Some else_br
    | _ -> None
end

module Match_expr = struct
  let match_keyword node = find_token Syntax_kind.TK_MATCH node
  let match_keyword_exn node = opt_exn match_keyword node

  let with_keyword node = find_token Syntax_kind.TK_WITH node
  let with_keyword_exn node = opt_exn with_keyword node

  let scrutinee node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | scrut :: _ -> Some scrut
    | [] -> None

  let scrutinee_exn node = opt_exn scrutinee node

  let arms node = filter_nodes Syntax_kind.NK_MATCH_ARM node
end

module Match_arm = struct
  let bar node = find_token Syntax_kind.TK_BAR node

  let pattern node =
    let pats = filter_pattern_nodes node in
    match pats with
    | pat :: _ -> Some pat
    | [] -> None

  let pattern_exn node = opt_exn pattern node

  let when_keyword node = find_token Syntax_kind.TK_WHEN node

  let guard node =
    (* Guard is the expression after 'when', if present *)
    match when_keyword node with
    | None -> None
    | Some _ ->
        let exprs = filter_expression_nodes node in
        (* First expression is the guard, second is the body *)
        (match exprs with
         | guard :: _ :: _ -> Some guard
         | _ -> None)

  let arrow node = find_token Syntax_kind.TK_ARROW node
  let arrow_exn node = opt_exn arrow node

  let body node =
    let exprs = filter_expression_nodes node in
    (* Body is the last expression *)
    match List.rev exprs with
    | body :: _ -> Some body
    | [] -> None

  let body_exn node = opt_exn body node
end

module Function_expr = struct
  let fun_keyword node = find_token Syntax_kind.TK_FUN node
  let fun_keyword_exn node = opt_exn fun_keyword node

  let parameters node = filter_pattern_nodes node

  let arrow node = find_token Syntax_kind.TK_ARROW node
  let arrow_exn node = opt_exn arrow node

  let body node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | body :: _ -> Some body
    | [] -> None

  let body_exn node = opt_exn body node
end

module Apply_expr = struct
  let func node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | func :: _ -> Some func
    | [] -> None

  let func_exn node = opt_exn func node

  let arguments node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | _ :: args -> args
    | [] -> []
end

module Infix_expr = struct
  let left node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | left :: _ -> Some left
    | [] -> None

  let left_exn node = opt_exn left node

  let operator node =
    (* Find the operator token (any operator kind) *)
    Red_tree.child_tokens node
    |> List.find_opt (fun token ->
           Syntax_kind.is_operator (Red_tree.token_kind token))

  let operator_exn node = opt_exn operator node

  let right node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | _ :: right :: _ -> Some right
    | _ -> None

  let right_exn node = opt_exn right node
end

module Tuple_expr = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let elements node = filter_expression_nodes node
end

module Record_expr = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let lbrace_exn node = opt_exn lbrace node
  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let rbrace_exn node = opt_exn rbrace node
  let fields node = filter_nodes Syntax_kind.NK_RECORD_FIELD node
end

module Record_field = struct
  let name node =
    find_token Syntax_kind.TK_LOWERCASE_IDENT node

  let name_exn node = opt_exn name node

  let equals node = find_token Syntax_kind.TK_EQUAL node

  let value node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | value :: _ -> Some value
    | [] -> None
end

module Record_update = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let lbrace_exn node = opt_exn lbrace node

  let with_keyword node = find_token Syntax_kind.TK_WITH node
  let with_keyword_exn node = opt_exn with_keyword node

  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let rbrace_exn node = opt_exn rbrace node

  let base node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | base :: _ -> Some base
    | [] -> None

  let base_exn node = opt_exn base node

  let fields node = filter_nodes Syntax_kind.NK_RECORD_FIELD node
end

module Paren_expr = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let lparen_exn node = opt_exn lparen node

  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let rparen_exn node = opt_exn rparen node

  let inner node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | inner :: _ -> Some inner
    | [] -> None

  let inner_exn node = opt_exn inner node
end

module Constraint_expr = struct
  let colon node = find_token Syntax_kind.TK_COLON node

  let expr node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | expr :: _ -> Some expr
    | [] -> None

  let expr_exn node = opt_exn expr node

  let type_expr node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  let type_expr_exn node = opt_exn type_expr node
end

module Sequence_expr = struct
  let semicolon node = find_token Syntax_kind.TK_SEMICOLON node

  let first node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | first :: _ -> Some first
    | [] -> None

  let first_exn node = opt_exn first node

  let second node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | _ :: second :: _ -> Some second
    | _ -> None

  let second_exn node = opt_exn second node
end

module Record_access = struct
  let dot node = find_token Syntax_kind.TK_DOT node

  let expr node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | expr :: _ -> Some expr
    | [] -> None

  let expr_exn node = opt_exn expr node

  let field node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let field_exn node = opt_exn field node
end

module Module_access = struct
  let path node = find_node Syntax_kind.NK_LONGIDENT node
  let dot node = find_token Syntax_kind.TK_DOT node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
end

module Constructor_expr = struct
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let name_exn node = opt_exn name node

  let argument node =
    let exprs = filter_expression_nodes node in
    match exprs with
    | arg :: _ -> Some arg
    | [] -> None
end

(** {1 Pattern Accessors} *)

module Tuple_pattern = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let elements node = filter_pattern_nodes node
end

module Constructor_pattern = struct
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let name_exn node = opt_exn name node

  let argument node =
    let pats = filter_pattern_nodes node in
    match pats with
    | arg :: _ -> Some arg
    | [] -> None
end

module Record_pattern = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let lbrace_exn node = opt_exn lbrace node

  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let rbrace_exn node = opt_exn rbrace node

  let fields node = filter_nodes Syntax_kind.NK_RECORD_PATTERN_FIELD node
end

module Record_pattern_field = struct
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let equals node = find_token Syntax_kind.TK_EQUAL node

  let pattern node =
    let pats = filter_pattern_nodes node in
    match pats with
    | pat :: _ -> Some pat
    | [] -> None
end

module Alias_pattern = struct
  let as_keyword node = find_token Syntax_kind.TK_AS node
  let as_keyword_exn node = opt_exn as_keyword node

  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let pattern node =
    let pats = filter_pattern_nodes node in
    match pats with
    | pat :: _ -> Some pat
    | [] -> None

  let pattern_exn node = opt_exn pattern node
end

module Constraint_pattern = struct
  let colon node = find_token Syntax_kind.TK_COLON node

  let pattern node =
    let pats = filter_pattern_nodes node in
    match pats with
    | pat :: _ -> Some pat
    | [] -> None

  let pattern_exn node = opt_exn pattern node

  let type_expr node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  let type_expr_exn node = opt_exn type_expr node
end

module Paren_pattern = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let lparen_exn node = opt_exn lparen node

  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let rparen_exn node = opt_exn rparen node

  let inner node =
    let pats = filter_pattern_nodes node in
    match pats with
    | inner :: _ -> Some inner
    | [] -> None

  let inner_exn node = opt_exn inner node
end

(** {1 Type Accessors} *)

module Type_arrow = struct
  let arrow node = find_token Syntax_kind.TK_ARROW node
  let arrow_exn node = opt_exn arrow node

  let domain node =
    let types = filter_type_nodes node in
    match types with
    | dom :: _ -> Some dom
    | [] -> None

  let domain_exn node = opt_exn domain node

  let codomain node =
    let types = filter_type_nodes node in
    match types with
    | _ :: cod :: _ -> Some cod
    | _ -> None

  let codomain_exn node = opt_exn codomain node
end

module Type_tuple = struct
  let elements node = filter_type_nodes node
end

module Type_constructor = struct
  let arguments node =
    (* Arguments are type nodes before the constructor name *)
    let types = filter_type_nodes node in
    (* Last type node is the constructor itself if it's a path *)
    types

  let name node =
    find_node Syntax_kind.NK_LONGIDENT node

  let name_exn node = opt_exn name node
end

module Type_record = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let lbrace_exn node = opt_exn lbrace node

  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let rbrace_exn node = opt_exn rbrace node

  let fields node = filter_nodes Syntax_kind.NK_TYPE_RECORD_FIELD node
end

module Type_record_field = struct
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let colon node = find_token Syntax_kind.TK_COLON node

  let type_expr node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  let type_expr_exn node = opt_exn type_expr node
end

module Paren_type = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let lparen_exn node = opt_exn lparen node

  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let rparen_exn node = opt_exn rparen node

  let inner node =
    let types = filter_type_nodes node in
    match types with
    | inner :: _ -> Some inner
    | [] -> None

  let inner_exn node = opt_exn inner node
end

(** {1 Binding Accessors} *)

module Binding = struct
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let equals_exn node = opt_exn equals node

  (** Get the name pattern (first pattern in the binding). *)
  let name node =
    let pats = filter_pattern_nodes node in
    match pats with
    | name :: _ -> Some name
    | [] -> None

  let name_exn node = opt_exn name node

  (** Get the function parameters (all patterns after the name).
      For [let f x y = expr], this returns [[x; y]].
      For [let x = expr], this returns [[]]. *)
  let parameters node =
    let pats = filter_pattern_nodes node in
    match pats with
    | _ :: params -> params
    | [] -> []

  (** Alias for backwards compatibility. *)
  let pattern node = name node
  let pattern_exn node = name_exn node

  let type_annotation node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  (** Get ALL body expressions (the CST may not wrap the entire expression
      in a single node, e.g., [x + y] may be [NK_VARIABLE_EXPR, NK_INFIX_EXPR]). *)
  let body_expressions node = filter_expression_nodes node

  (** Get the first body expression (for simple bindings). *)
  let body node =
    match body_expressions node with
    | body :: _ -> Some body
    | [] -> None

  let body_exn node = opt_exn body node
end

(** {1 Structure Item Accessors} *)

module Value_definition = struct
  let let_keyword node = find_token Syntax_kind.TK_LET node
  let let_keyword_exn node = opt_exn let_keyword node

  let rec_keyword node = find_token Syntax_kind.TK_REC node

  let bindings node = filter_nodes Syntax_kind.NK_BINDING node
end

module Type_definition = struct
  let type_keyword node = find_token Syntax_kind.TK_TYPE node
  let type_keyword_exn node = opt_exn type_keyword node

  let declarations node = filter_nodes Syntax_kind.NK_TYPE_DECLARATION node
end

module Type_declaration = struct
  let params node = find_node Syntax_kind.NK_TYPE_PARAMETERS node

  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let equals node = find_token Syntax_kind.TK_EQUAL node

  let body node =
    (* Body could be various type-related nodes *)
    let children = Red_tree.child_nodes node in
    children
    |> List.find_opt (fun child ->
           let kind = Red_tree.kind child in
           Syntax_kind.is_type kind ||
           Syntax_kind.equal kind Syntax_kind.NK_CONSTRUCTOR_DECLARATION)
end

module Constructor_declaration = struct
  let bar node = find_token Syntax_kind.TK_BAR node

  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let name_exn node = opt_exn name node

  let of_keyword node = find_token Syntax_kind.TK_OF node

  let argument_type node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None
end

module Module_definition = struct
  let module_keyword node = find_token Syntax_kind.TK_MODULE node
  let module_keyword_exn node = opt_exn module_keyword node

  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let name_exn node = opt_exn name node

  let params node = filter_nodes Syntax_kind.NK_FUNCTOR_PARAMETER node

  let type_annotation node =
    (* Module type annotation after colon *)
    find_node Syntax_kind.NK_SIGNATURE node

  let equals node = find_token Syntax_kind.TK_EQUAL node
  let equals_exn node = opt_exn equals node

  let body node =
    (* Body is a module expression *)
    let children = Red_tree.child_nodes node in
    children
    |> List.find_opt (fun child ->
           let kind = Red_tree.kind child in
           Syntax_kind.equal kind Syntax_kind.NK_STRUCTURE ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_PATH ||
           Syntax_kind.equal kind Syntax_kind.NK_FUNCTOR_EXPR ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_APPLY ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_CONSTRAINT)

  let body_exn node = opt_exn body node
end

module Open_declaration = struct
  let open_keyword node = find_token Syntax_kind.TK_OPEN node
  let open_keyword_exn node = opt_exn open_keyword node

  let path node =
    find_node Syntax_kind.NK_MODULE_PATH node

  let path_exn node = opt_exn path node
end

module Include_declaration = struct
  let include_keyword node = find_token Syntax_kind.TK_INCLUDE node
  let include_keyword_exn node = opt_exn include_keyword node

  let module_expr node =
    let children = Red_tree.child_nodes node in
    children
    |> List.find_opt (fun child ->
           let kind = Red_tree.kind child in
           Syntax_kind.equal kind Syntax_kind.NK_STRUCTURE ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_PATH ||
           Syntax_kind.equal kind Syntax_kind.NK_FUNCTOR_EXPR ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_APPLY)

  let module_expr_exn node = opt_exn module_expr node
end

module External_declaration = struct
  let external_keyword node = find_token Syntax_kind.TK_EXTERNAL node
  let external_keyword_exn node = opt_exn external_keyword node

  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let colon node = find_token Syntax_kind.TK_COLON node

  let type_expr node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  let type_expr_exn node = opt_exn type_expr node

  let equals node = find_token Syntax_kind.TK_EQUAL node

  let lua_name node = find_token Syntax_kind.TK_STRING node
end

(** {1 Module Expression Accessors} *)

(** {1 Helper Functions} *)

let is_structure_item kind =
  Syntax_kind.equal kind Syntax_kind.NK_VALUE_DEFINITION ||
  Syntax_kind.equal kind Syntax_kind.NK_TYPE_DEFINITION ||
  Syntax_kind.equal kind Syntax_kind.NK_MODULE_DEFINITION ||
  Syntax_kind.equal kind Syntax_kind.NK_MODULE_TYPE_DEFINITION ||
  Syntax_kind.equal kind Syntax_kind.NK_OPEN_DECLARATION ||
  Syntax_kind.equal kind Syntax_kind.NK_INCLUDE_DECLARATION ||
  Syntax_kind.equal kind Syntax_kind.NK_EXTERNAL_DECLARATION

let is_signature_item kind =
  Syntax_kind.equal kind Syntax_kind.NK_VALUE_SPECIFICATION ||
  Syntax_kind.equal kind Syntax_kind.NK_TYPE_SPECIFICATION ||
  Syntax_kind.equal kind Syntax_kind.NK_MODULE_SPECIFICATION ||
  Syntax_kind.equal kind Syntax_kind.NK_MODULE_TYPE_SPECIFICATION ||
  Syntax_kind.equal kind Syntax_kind.NK_INCLUDE_SPECIFICATION

module Structure = struct
  let struct_keyword node = find_token Syntax_kind.TK_STRUCT node
  let struct_keyword_exn node = opt_exn struct_keyword node

  let end_keyword node = find_token Syntax_kind.TK_END node
  let end_keyword_exn node = opt_exn end_keyword node

  let items node =
    Red_tree.child_nodes node
    |> List.filter (fun child ->
           is_structure_item (Red_tree.kind child))
end

module Functor_expr = struct
  let functor_keyword node = find_token Syntax_kind.TK_FUNCTOR node
  let functor_keyword_exn node = opt_exn functor_keyword node

  let params node = filter_nodes Syntax_kind.NK_FUNCTOR_PARAMETER node

  let arrow node = find_token Syntax_kind.TK_ARROW node
  let arrow_exn node = opt_exn arrow node

  let body node =
    let children = Red_tree.child_nodes node in
    children
    |> List.find_opt (fun child ->
           let kind = Red_tree.kind child in
           Syntax_kind.equal kind Syntax_kind.NK_STRUCTURE ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_PATH ||
           Syntax_kind.equal kind Syntax_kind.NK_FUNCTOR_EXPR ||
           Syntax_kind.equal kind Syntax_kind.NK_MODULE_APPLY)

  let body_exn node = opt_exn body node
end

module Functor_parameter = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let lparen_exn node = opt_exn lparen node

  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let rparen_exn node = opt_exn rparen node

  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let name_exn node = opt_exn name node

  let colon node = find_token Syntax_kind.TK_COLON node

  let module_type node =
    find_node Syntax_kind.NK_SIGNATURE node

  let module_type_exn node = opt_exn module_type node
end

module Module_apply = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node

  let functor_expr node =
    let children = Red_tree.child_nodes node in
    match children with
    | func :: _ -> Some func
    | [] -> None

  let functor_expr_exn node = opt_exn functor_expr node

  let argument node =
    let children = Red_tree.child_nodes node in
    match children with
    | _ :: arg :: _ -> Some arg
    | _ -> None

  let argument_exn node = opt_exn argument node
end

module Module_constraint = struct
  let colon node = find_token Syntax_kind.TK_COLON node

  let module_expr node =
    let children = Red_tree.child_nodes node in
    match children with
    | expr :: _ -> Some expr
    | [] -> None

  let module_expr_exn node = opt_exn module_expr node

  let module_type node =
    find_node Syntax_kind.NK_SIGNATURE node

  let module_type_exn node = opt_exn module_type node
end

(** {1 Signature Accessors} *)

module Signature = struct
  let sig_keyword node = find_token Syntax_kind.TK_SIG node
  let sig_keyword_exn node = opt_exn sig_keyword node

  let end_keyword node = find_token Syntax_kind.TK_END node
  let end_keyword_exn node = opt_exn end_keyword node

  let items node =
    Red_tree.child_nodes node
    |> List.filter (fun child ->
           is_signature_item (Red_tree.kind child))
end

module Value_specification = struct
  let val_keyword node = find_token Syntax_kind.TK_VAL node
  let val_keyword_exn node = opt_exn val_keyword node

  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let name_exn node = opt_exn name node

  let colon node = find_token Syntax_kind.TK_COLON node

  let type_expr node =
    let types = filter_type_nodes node in
    match types with
    | ty :: _ -> Some ty
    | [] -> None

  let type_expr_exn node = opt_exn type_expr node
end

let first_expression_child node =
  filter_expression_nodes node
  |> (function
      | first :: _ -> Some first
      | [] -> None)

let expression_children = filter_expression_nodes
let pattern_children = filter_pattern_nodes
let type_children = filter_type_nodes
