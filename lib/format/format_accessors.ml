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

let filter_children_by predicate node =
  Red_tree.child_nodes node
  |> List.filter (fun child -> predicate (Red_tree.kind child))

let filter_expression_nodes = filter_children_by Syntax_kind.is_expression
let filter_pattern_nodes = filter_children_by Syntax_kind.is_pattern
let filter_type_nodes = filter_children_by Syntax_kind.is_type

let is_module_expr_kind kind =
  List.exists (Syntax_kind.equal kind) Syntax_kind.[
    NK_STRUCTURE; NK_MODULE_PATH; NK_FUNCTOR_EXPR; NK_MODULE_APPLY
  ]

let is_module_expr_kind_with_constraint kind =
  is_module_expr_kind kind || Syntax_kind.equal kind Syntax_kind.NK_MODULE_CONSTRAINT

(** {1 Positional Access Helpers} *)

let first_of filter node =
  match filter node with
  | first :: _ -> Some first
  | [] -> None

let second_of filter node =
  match filter node with
  | _ :: second :: _ -> Some second
  | _ -> None

let third_of filter node =
  match filter node with
  | _ :: _ :: third :: _ -> Some third
  | _ -> None

let last_of filter node =
  match List.rev (filter node) with
  | last :: _ -> Some last
  | [] -> None

let rest_of filter node =
  match filter node with
  | _ :: rest -> rest
  | [] -> []

(** {1 Expression Accessors} *)

module Let_expr = struct
  let let_keyword node = find_token Syntax_kind.TK_LET node
  let rec_keyword node = find_token Syntax_kind.TK_REC node
  let bindings node = filter_nodes Syntax_kind.NK_BINDING node
  let in_keyword node = find_token Syntax_kind.TK_IN node
  let body node = last_of filter_expression_nodes node
end

module If_expr = struct
  let if_keyword node = find_token Syntax_kind.TK_IF node
  let then_keyword node = find_token Syntax_kind.TK_THEN node
  let else_keyword node = find_token Syntax_kind.TK_ELSE node
  let condition node = first_of filter_expression_nodes node
  let then_branch node = second_of filter_expression_nodes node
  let else_branch node = third_of filter_expression_nodes node
end

module Match_expr = struct
  let match_keyword node = find_token Syntax_kind.TK_MATCH node
  let with_keyword node = find_token Syntax_kind.TK_WITH node
  let scrutinee node = first_of filter_expression_nodes node
  let arms node = filter_nodes Syntax_kind.NK_MATCH_ARM node
end

module Match_arm = struct
  let bar node = find_token Syntax_kind.TK_BAR node
  let pattern node = first_of filter_pattern_nodes node
  let when_keyword node = find_token Syntax_kind.TK_WHEN node

  let guard node =
    Option.bind (when_keyword node) (fun _ ->
      match filter_expression_nodes node with
      | guard_expr :: _ :: _ -> Some guard_expr
      | _ -> None)

  let arrow node = find_token Syntax_kind.TK_ARROW node
  let body node = last_of filter_expression_nodes node
end

module Function_expr = struct
  let fun_keyword node = find_token Syntax_kind.TK_FUN node
  let parameters node = filter_pattern_nodes node
  let arrow node = find_token Syntax_kind.TK_ARROW node
  let body node = first_of filter_expression_nodes node
end

module Apply_expr = struct
  let func node = first_of filter_expression_nodes node
  let arguments node = rest_of filter_expression_nodes node
end

module Infix_expr = struct
  let left node = first_of filter_expression_nodes node

  let operator node =
    Red_tree.child_tokens node
    |> List.find_opt (fun token ->
           Syntax_kind.is_operator (Red_tree.token_kind token))

  let right node = second_of filter_expression_nodes node
end

module Tuple_expr = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let elements node = filter_expression_nodes node
end

module Record_expr = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let fields node = filter_nodes Syntax_kind.NK_RECORD_FIELD node
end

module Record_field = struct
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let value node = first_of filter_expression_nodes node
end

module Record_update = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let with_keyword node = find_token Syntax_kind.TK_WITH node
  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let base node = first_of filter_expression_nodes node
  let fields node = filter_nodes Syntax_kind.NK_RECORD_FIELD node
end

module Paren_expr = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let inner node = first_of filter_expression_nodes node
end

module Constraint_expr = struct
  let colon node = find_token Syntax_kind.TK_COLON node
  let expr node = first_of filter_expression_nodes node
  let type_expr node = first_of filter_type_nodes node
end

module Sequence_expr = struct
  let semicolon node = find_token Syntax_kind.TK_SEMICOLON node
  let first node = first_of filter_expression_nodes node
  let second node = second_of filter_expression_nodes node
end

module Record_access = struct
  let dot node = find_token Syntax_kind.TK_DOT node
  let expr node = first_of filter_expression_nodes node
  let field node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
end

module Module_access = struct
  let path node = find_node Syntax_kind.NK_LONGIDENT node
  let dot node = find_token Syntax_kind.TK_DOT node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
end

module Constructor_expr = struct
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let argument node = first_of filter_expression_nodes node
end

(** {1 Pattern Accessors} *)

module Tuple_pattern = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let elements node = filter_pattern_nodes node
end

module Constructor_pattern = struct
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let argument node = first_of filter_pattern_nodes node
end

module Record_pattern = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let fields node = filter_nodes Syntax_kind.NK_RECORD_PATTERN_FIELD node
end

module Record_pattern_field = struct
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let pattern node = first_of filter_pattern_nodes node
end

module Alias_pattern = struct
  let as_keyword node = find_token Syntax_kind.TK_AS node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let pattern node = first_of filter_pattern_nodes node
end

module Constraint_pattern = struct
  let colon node = find_token Syntax_kind.TK_COLON node
  let pattern node = first_of filter_pattern_nodes node
  let type_expr node = first_of filter_type_nodes node
end

module Paren_pattern = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let inner node = first_of filter_pattern_nodes node
end

(** {1 Type Accessors} *)

module Type_arrow = struct
  let arrow node = find_token Syntax_kind.TK_ARROW node
  let domain node = first_of filter_type_nodes node
  let codomain node = second_of filter_type_nodes node
end

module Type_tuple = struct
  let elements node = filter_type_nodes node
end

module Type_constructor = struct
  let arguments node = filter_type_nodes node
  let name node = find_node Syntax_kind.NK_LONGIDENT node
end

module Type_record = struct
  let lbrace node = find_token Syntax_kind.TK_LBRACE node
  let rbrace node = find_token Syntax_kind.TK_RBRACE node
  let fields node = filter_nodes Syntax_kind.NK_TYPE_RECORD_FIELD node
end

module Type_record_field = struct
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let colon node = find_token Syntax_kind.TK_COLON node
  let type_expr node = first_of filter_type_nodes node
end

module Paren_type = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let inner node = first_of filter_type_nodes node
end

(** {1 Binding Accessors} *)

module Binding = struct
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let name node = first_of filter_pattern_nodes node
  let parameters node = rest_of filter_pattern_nodes node
  let pattern node = name node
  let type_annotation node = first_of filter_type_nodes node
  let body_expressions node = filter_expression_nodes node
  let body node = first_of filter_expression_nodes node
end

(** {1 Structure Item Accessors} *)

module Value_definition = struct
  let let_keyword node = find_token Syntax_kind.TK_LET node
  let rec_keyword node = find_token Syntax_kind.TK_REC node
  let bindings node = filter_nodes Syntax_kind.NK_BINDING node
end

module Type_definition = struct
  let type_keyword node = find_token Syntax_kind.TK_TYPE node
  let declarations node = filter_nodes Syntax_kind.NK_TYPE_DECLARATION node
end

module Type_declaration = struct
  let params node = find_node Syntax_kind.NK_TYPE_PARAMETERS node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let equals node = find_token Syntax_kind.TK_EQUAL node

  let body node =
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
  let of_keyword node = find_token Syntax_kind.TK_OF node
  let argument_type node = first_of filter_type_nodes node
end

module Module_definition = struct
  let module_keyword node = find_token Syntax_kind.TK_MODULE node
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let params node = filter_nodes Syntax_kind.NK_FUNCTOR_PARAMETER node
  let type_annotation node = find_node Syntax_kind.NK_SIGNATURE node
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let body node = filter_children_by is_module_expr_kind_with_constraint node |> first_of Fun.id
end

module Open_declaration = struct
  let open_keyword node = find_token Syntax_kind.TK_OPEN node
  let path node = find_node Syntax_kind.NK_MODULE_PATH node
end

module Include_declaration = struct
  let include_keyword node = find_token Syntax_kind.TK_INCLUDE node

  let module_expr node = filter_children_by is_module_expr_kind node |> first_of Fun.id
end

module External_declaration = struct
  let external_keyword node = find_token Syntax_kind.TK_EXTERNAL node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let colon node = find_token Syntax_kind.TK_COLON node
  let type_expr node = first_of filter_type_nodes node
  let equals node = find_token Syntax_kind.TK_EQUAL node
  let lua_name node = find_token Syntax_kind.TK_STRING node
end

(** {1 Module Expression Accessors} *)

(** {1 Helper Functions} *)

let is_structure_item kind =
  List.exists (Syntax_kind.equal kind) Syntax_kind.[
    NK_VALUE_DEFINITION; NK_TYPE_DEFINITION; NK_MODULE_DEFINITION;
    NK_MODULE_TYPE_DEFINITION; NK_OPEN_DECLARATION; NK_INCLUDE_DECLARATION;
    NK_EXTERNAL_DECLARATION
  ]

let is_signature_item kind =
  List.exists (Syntax_kind.equal kind) Syntax_kind.[
    NK_VALUE_SPECIFICATION; NK_TYPE_SPECIFICATION; NK_MODULE_SPECIFICATION;
    NK_MODULE_TYPE_SPECIFICATION; NK_INCLUDE_SPECIFICATION
  ]

module Structure = struct
  let struct_keyword node = find_token Syntax_kind.TK_STRUCT node
  let end_keyword node = find_token Syntax_kind.TK_END node

  let items node =
    Red_tree.child_nodes node
    |> List.filter (fun child ->
           is_structure_item (Red_tree.kind child))
end

module Functor_expr = struct
  let functor_keyword node = find_token Syntax_kind.TK_FUNCTOR node
  let params node = filter_nodes Syntax_kind.NK_FUNCTOR_PARAMETER node
  let arrow node = find_token Syntax_kind.TK_ARROW node
  let body node = filter_children_by is_module_expr_kind node |> first_of Fun.id
end

module Functor_parameter = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let name node = find_token Syntax_kind.TK_UPPERCASE_IDENT node
  let colon node = find_token Syntax_kind.TK_COLON node
  let module_type node = find_node Syntax_kind.NK_SIGNATURE node
end

module Module_apply = struct
  let lparen node = find_token Syntax_kind.TK_LPAREN node
  let rparen node = find_token Syntax_kind.TK_RPAREN node
  let functor_expr node = first_of Red_tree.child_nodes node
  let argument node = second_of Red_tree.child_nodes node
end

module Module_constraint = struct
  let colon node = find_token Syntax_kind.TK_COLON node
  let module_expr node = first_of Red_tree.child_nodes node
  let module_type node = find_node Syntax_kind.NK_SIGNATURE node
end

(** {1 Signature Accessors} *)

module Signature = struct
  let sig_keyword node = find_token Syntax_kind.TK_SIG node
  let end_keyword node = find_token Syntax_kind.TK_END node

  let items node =
    Red_tree.child_nodes node
    |> List.filter (fun child ->
           is_signature_item (Red_tree.kind child))
end

module Value_specification = struct
  let val_keyword node = find_token Syntax_kind.TK_VAL node
  let name node = find_token Syntax_kind.TK_LOWERCASE_IDENT node
  let colon node = find_token Syntax_kind.TK_COLON node
  let type_expr node = first_of filter_type_nodes node
end

let first_expression_child node = first_of filter_expression_nodes node
let expression_children = filter_expression_nodes
let pattern_children = filter_pattern_nodes
let type_children = filter_type_nodes
