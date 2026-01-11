(** Typed child accessors for CST nodes.

    This module provides type-safe accessors for extracting children from CST
    nodes by their semantic role rather than position. This makes formatting
    code more readable and resilient to grammar changes.

    Each accessor returns [None] if the expected child is not found, allowing
    graceful fallback to generic formatting. For cases where the child is
    required, [*_exn] variants raise [Not_found].

    {2 Usage Pattern}

    {[
      let format_let_expr node =
        match Let_expr.let_keyword node, Let_expr.body node with
        | Some kw, Some body -> (* semantic formatting *)
        | _ -> format_children node (* fallback *)
    ]} *)

open Cst

(** {1 Expression Accessors} *)

(** Accessors for [NK_LET_EXPR] nodes. *)
module Let_expr : sig
  val let_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val let_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val rec_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option

  val bindings : Red_tree.syntax_node -> Red_tree.syntax_node list

  val in_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val in_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_IF_EXPR] nodes. *)
module If_expr : sig
  val if_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val if_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val condition : Red_tree.syntax_node -> Red_tree.syntax_node option
  val condition_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val then_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val then_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val then_branch : Red_tree.syntax_node -> Red_tree.syntax_node option
  val then_branch_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val else_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option

  val else_branch : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_MATCH_EXPR] nodes. *)
module Match_expr : sig
  val match_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val match_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val scrutinee : Red_tree.syntax_node -> Red_tree.syntax_node option
  val scrutinee_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val with_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val with_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val arms : Red_tree.syntax_node -> Red_tree.syntax_node list
end

(** Accessors for [NK_MATCH_ARM] nodes. *)
module Match_arm : sig
  val bar : Red_tree.syntax_node -> Red_tree.syntax_token option

  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val pattern_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val when_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option

  val guard : Red_tree.syntax_node -> Red_tree.syntax_node option

  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val arrow_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_FUNCTION_EXPR] nodes (lambda expressions). *)
module Function_expr : sig
  val fun_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val fun_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val parameters : Red_tree.syntax_node -> Red_tree.syntax_node list

  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val arrow_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_APPLY_EXPR] nodes (function application). *)
module Apply_expr : sig
  val func : Red_tree.syntax_node -> Red_tree.syntax_node option
  val func_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val arguments : Red_tree.syntax_node -> Red_tree.syntax_node list
end

(** Accessors for [NK_INFIX_EXPR] nodes (binary operations). *)
module Infix_expr : sig
  val left : Red_tree.syntax_node -> Red_tree.syntax_node option
  val left_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val operator : Red_tree.syntax_node -> Red_tree.syntax_token option
  val operator_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val right : Red_tree.syntax_node -> Red_tree.syntax_node option
  val right_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_TUPLE_EXPR] nodes. *)
module Tuple_expr : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option

  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** Accessors for [NK_RECORD_EXPR] nodes. *)
module Record_expr : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_RECORD_FIELD] nodes. *)
module Record_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option

  val value : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_RECORD_UPDATE_EXPR] nodes. *)
module Record_update : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val base : Red_tree.syntax_node -> Red_tree.syntax_node option
  val base_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val with_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val with_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_PAREN_EXPR] nodes. *)
module Paren_expr : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val inner_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_CONSTRAINT_EXPR] nodes. *)
module Constraint_expr : sig
  val expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_SEQUENCE_EXPR] nodes. *)
module Sequence_expr : sig
  val first : Red_tree.syntax_node -> Red_tree.syntax_node option
  val first_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val semicolon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val second : Red_tree.syntax_node -> Red_tree.syntax_node option
  val second_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_RECORD_ACCESS_EXPR] nodes. *)
module Record_access : sig
  val expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val dot : Red_tree.syntax_node -> Red_tree.syntax_token option

  val field : Red_tree.syntax_node -> Red_tree.syntax_token option
  val field_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_MODULE_ACCESS_EXPR] nodes. *)
module Module_access : sig
  val path : Red_tree.syntax_node -> Red_tree.syntax_node option

  val dot : Red_tree.syntax_node -> Red_tree.syntax_token option

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** Accessors for [NK_CONSTRUCTOR_EXPR] nodes. *)
module Constructor_expr : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** {1 Pattern Accessors} *)

(** Accessors for [NK_TUPLE_PATTERN] nodes. *)
module Tuple_pattern : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option

  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** Accessors for [NK_CONSTRUCTOR_PATTERN] nodes. *)
module Constructor_pattern : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_RECORD_PATTERN] nodes. *)
module Record_pattern : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_RECORD_PATTERN_FIELD] nodes. *)
module Record_pattern_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option

  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_ALIAS_PATTERN] nodes. *)
module Alias_pattern : sig
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val pattern_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val as_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val as_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_CONSTRAINT_PATTERN] nodes. *)
module Constraint_pattern : sig
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val pattern_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_PAREN_PATTERN] nodes. *)
module Paren_pattern : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val inner_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** {1 Type Accessors} *)

(** Accessors for [NK_TYPE_ARROW] nodes. *)
module Type_arrow : sig
  val domain : Red_tree.syntax_node -> Red_tree.syntax_node option
  val domain_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val arrow_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val codomain : Red_tree.syntax_node -> Red_tree.syntax_node option
  val codomain_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_TYPE_TUPLE] nodes. *)
module Type_tuple : sig
  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list
end

(** Accessors for [NK_TYPE_CONSTRUCTOR] nodes. *)
module Type_constructor : sig
  val arguments : Red_tree.syntax_node -> Red_tree.syntax_node list

  val name : Red_tree.syntax_node -> Red_tree.syntax_node option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_TYPE_RECORD] nodes. *)
module Type_record : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list

  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rbrace_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_TYPE_RECORD_FIELD] nodes. *)
module Type_record_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_PAREN_TYPE] nodes. *)
module Paren_type : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val inner_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** {1 Binding Accessors} *)

(** Accessors for [NK_BINDING] nodes. *)
module Binding : sig
  (** Get the name pattern (first pattern in the binding). *)
  val name : Red_tree.syntax_node -> Red_tree.syntax_node option

  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  (** Get the function parameters (all patterns after the name).
      For [let f x y = expr], this returns [[x; y]].
      For [let x = expr], this returns [[]]. *)
  val parameters : Red_tree.syntax_node -> Red_tree.syntax_node list

  (** Alias for [name], for backwards compatibility. *)
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option

  val pattern_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val type_annotation : Red_tree.syntax_node -> Red_tree.syntax_node option

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val equals_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  (** Get ALL body expressions (the CST may not wrap the entire expression
      in a single node, e.g., [x + y] may be [NK_VARIABLE_EXPR, NK_INFIX_EXPR]). *)
  val body_expressions : Red_tree.syntax_node -> Red_tree.syntax_node list

  (** Get the first body expression (for simple bindings). *)
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option

  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** {1 Structure Item Accessors} *)

(** Accessors for [NK_VALUE_DEFINITION] nodes. *)
module Value_definition : sig
  val let_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val let_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val rec_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option

  val bindings : Red_tree.syntax_node -> Red_tree.syntax_node list
end

(** Accessors for [NK_TYPE_DEFINITION] nodes. *)
module Type_definition : sig
  val type_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val declarations : Red_tree.syntax_node -> Red_tree.syntax_node list
end

(** Accessors for [NK_TYPE_DECLARATION] nodes. *)
module Type_declaration : sig
  val params : Red_tree.syntax_node -> Red_tree.syntax_node option

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_CONSTRUCTOR_DECLARATION] nodes. *)
module Constructor_declaration : sig
  val bar : Red_tree.syntax_node -> Red_tree.syntax_token option

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val of_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option

  val argument_type : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** Accessors for [NK_MODULE_DEFINITION] nodes. *)
module Module_definition : sig
  val module_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val module_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val params : Red_tree.syntax_node -> Red_tree.syntax_node list

  val type_annotation : Red_tree.syntax_node -> Red_tree.syntax_node option

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val equals_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_OPEN_DECLARATION] nodes. *)
module Open_declaration : sig
  val open_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val open_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val path : Red_tree.syntax_node -> Red_tree.syntax_node option
  val path_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_INCLUDE_DECLARATION] nodes. *)
module Include_declaration : sig
  val include_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val include_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val module_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val module_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_EXTERNAL_DECLARATION] nodes. *)
module External_declaration : sig
  val external_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val external_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option

  val lua_name : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** {1 Module Expression Accessors} *)

(** Accessors for [NK_STRUCTURE] nodes. *)
module Structure : sig
  val struct_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val struct_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val items : Red_tree.syntax_node -> Red_tree.syntax_node list

  val end_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val end_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_FUNCTOR_EXPR] nodes. *)
module Functor_expr : sig
  val functor_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val functor_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val params : Red_tree.syntax_node -> Red_tree.syntax_node list

  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val arrow_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
  val body_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** Accessors for [NK_FUNCTOR_PARAMETER] nodes. *)
module Functor_parameter : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val module_type : Red_tree.syntax_node -> Red_tree.syntax_node option
  val module_type_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rparen_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_MODULE_APPLY] nodes. *)
module Module_apply : sig
  val functor_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val functor_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option

  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
  val argument_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** Accessors for [NK_MODULE_CONSTRAINT] nodes. *)
module Module_constraint : sig
  val module_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val module_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val module_type : Red_tree.syntax_node -> Red_tree.syntax_node option
  val module_type_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** {1 Signature Accessors} *)

(** Accessors for [NK_SIGNATURE] nodes. *)
module Signature : sig
  val sig_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val sig_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val items : Red_tree.syntax_node -> Red_tree.syntax_node list

  val end_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val end_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token
end

(** Accessors for [NK_VALUE_SPECIFICATION] nodes. *)
module Value_specification : sig
  val val_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val val_keyword_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name_exn : Red_tree.syntax_node -> Red_tree.syntax_token

  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option

  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_expr_exn : Red_tree.syntax_node -> Red_tree.syntax_node
end

(** {1 Helper Functions} *)

(** [is_structure_item kind] returns [true] if [kind] is a structure item. *)
val is_structure_item : Syntax_kind.t -> bool

(** [is_signature_item kind] returns [true] if [kind] is a signature item. *)
val is_signature_item : Syntax_kind.t -> bool

(** [first_expression_child node] returns the first child that is an expression. *)
val first_expression_child : Red_tree.syntax_node -> Red_tree.syntax_node option

(** [expression_children node] returns all children that are expressions. *)
val expression_children : Red_tree.syntax_node -> Red_tree.syntax_node list

(** [pattern_children node] returns all children that are patterns. *)
val pattern_children : Red_tree.syntax_node -> Red_tree.syntax_node list

(** [type_children node] returns all children that are types. *)
val type_children : Red_tree.syntax_node -> Red_tree.syntax_node list
