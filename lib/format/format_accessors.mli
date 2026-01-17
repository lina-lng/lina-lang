(** Typed child accessors for CST nodes.

    This module provides type-safe accessors for extracting children from CST
    nodes by their semantic role rather than position. This makes formatting
    code more readable and resilient to grammar changes.

    Each accessor returns [None] if the expected child is not found, allowing
    graceful fallback to generic formatting.

    {2 Usage Pattern}

    {[
      let format_let_expr node =
        match Let_expr.let_keyword node, Let_expr.body node with
        | Some kw, Some body -> (* semantic formatting *)
        | _ -> format_children node (* fallback *)
    ]} *)

open Cst

(** {1 Expression Accessors} *)

module Let_expr : sig
  val let_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rec_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val bindings : Red_tree.syntax_node -> Red_tree.syntax_node list
  val in_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module If_expr : sig
  val if_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val condition : Red_tree.syntax_node -> Red_tree.syntax_node option
  val then_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val then_branch : Red_tree.syntax_node -> Red_tree.syntax_node option
  val else_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val else_branch : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Match_expr : sig
  val match_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val scrutinee : Red_tree.syntax_node -> Red_tree.syntax_node option
  val with_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val arms : Red_tree.syntax_node -> Red_tree.syntax_node list
end

module Match_arm : sig
  val bar : Red_tree.syntax_node -> Red_tree.syntax_token option
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val when_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val guard : Red_tree.syntax_node -> Red_tree.syntax_node option
  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Function_expr : sig
  val fun_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val parameters : Red_tree.syntax_node -> Red_tree.syntax_node list
  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Apply_expr : sig
  val func : Red_tree.syntax_node -> Red_tree.syntax_node option
  val arguments : Red_tree.syntax_node -> Red_tree.syntax_node list
end

module Infix_expr : sig
  val left : Red_tree.syntax_node -> Red_tree.syntax_node option
  val operator : Red_tree.syntax_node -> Red_tree.syntax_token option
  val right : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Tuple_expr : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Record_expr : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Record_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val value : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Record_update : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val base : Red_tree.syntax_node -> Red_tree.syntax_node option
  val with_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Paren_expr : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Constraint_expr : sig
  val expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Sequence_expr : sig
  val first : Red_tree.syntax_node -> Red_tree.syntax_node option
  val semicolon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val second : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Record_access : sig
  val expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val dot : Red_tree.syntax_node -> Red_tree.syntax_token option
  val field : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Module_access : sig
  val path : Red_tree.syntax_node -> Red_tree.syntax_node option
  val dot : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Constructor_expr : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** {1 Pattern Accessors} *)

module Tuple_pattern : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Constructor_pattern : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Record_pattern : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Record_pattern_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Alias_pattern : sig
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val as_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Constraint_pattern : sig
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Paren_pattern : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** {1 Type Accessors} *)

module Type_arrow : sig
  val domain : Red_tree.syntax_node -> Red_tree.syntax_node option
  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val codomain : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Type_tuple : sig
  val elements : Red_tree.syntax_node -> Red_tree.syntax_node list
end

module Type_constructor : sig
  val arguments : Red_tree.syntax_node -> Red_tree.syntax_node list
  val name : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Type_record : sig
  val lbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
  val fields : Red_tree.syntax_node -> Red_tree.syntax_node list
  val rbrace : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Type_record_field : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Paren_type : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val inner : Red_tree.syntax_node -> Red_tree.syntax_node option
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** {1 Binding Accessors} *)

module Binding : sig
  val name : Red_tree.syntax_node -> Red_tree.syntax_node option
  val parameters : Red_tree.syntax_node -> Red_tree.syntax_node list
  val pattern : Red_tree.syntax_node -> Red_tree.syntax_node option
  val type_annotation : Red_tree.syntax_node -> Red_tree.syntax_node option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body_expressions : Red_tree.syntax_node -> Red_tree.syntax_node list
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** {1 Structure Item Accessors} *)

module Value_definition : sig
  val let_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val rec_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val bindings : Red_tree.syntax_node -> Red_tree.syntax_node list
end

module Type_definition : sig
  val type_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val declarations : Red_tree.syntax_node -> Red_tree.syntax_node list
end

module Type_declaration : sig
  val params : Red_tree.syntax_node -> Red_tree.syntax_node option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Constructor_declaration : sig
  val bar : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val of_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val argument_type : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Module_definition : sig
  val module_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val params : Red_tree.syntax_node -> Red_tree.syntax_node list
  val type_annotation : Red_tree.syntax_node -> Red_tree.syntax_node option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Open_declaration : sig
  val open_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val path : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Include_declaration : sig
  val include_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val module_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module External_declaration : sig
  val external_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val equals : Red_tree.syntax_node -> Red_tree.syntax_token option
  val lua_name : Red_tree.syntax_node -> Red_tree.syntax_token option
end

(** {1 Module Expression Accessors} *)

module Structure : sig
  val struct_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val items : Red_tree.syntax_node -> Red_tree.syntax_node list
  val end_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Functor_expr : sig
  val functor_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val params : Red_tree.syntax_node -> Red_tree.syntax_node list
  val arrow : Red_tree.syntax_node -> Red_tree.syntax_token option
  val body : Red_tree.syntax_node -> Red_tree.syntax_node option
end

module Functor_parameter : sig
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val module_type : Red_tree.syntax_node -> Red_tree.syntax_node option
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Module_apply : sig
  val functor_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val lparen : Red_tree.syntax_node -> Red_tree.syntax_token option
  val argument : Red_tree.syntax_node -> Red_tree.syntax_node option
  val rparen : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Module_constraint : sig
  val module_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val module_type : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** {1 Signature Accessors} *)

module Signature : sig
  val sig_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val items : Red_tree.syntax_node -> Red_tree.syntax_node list
  val end_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
end

module Value_specification : sig
  val val_keyword : Red_tree.syntax_node -> Red_tree.syntax_token option
  val name : Red_tree.syntax_node -> Red_tree.syntax_token option
  val colon : Red_tree.syntax_node -> Red_tree.syntax_token option
  val type_expr : Red_tree.syntax_node -> Red_tree.syntax_node option
end

(** {1 Helper Functions} *)

val is_structure_item : Syntax_kind.t -> bool
val is_signature_item : Syntax_kind.t -> bool
val first_expression_child : Red_tree.syntax_node -> Red_tree.syntax_node option
val expression_children : Red_tree.syntax_node -> Red_tree.syntax_node list
val pattern_children : Red_tree.syntax_node -> Red_tree.syntax_node list
val type_children : Red_tree.syntax_node -> Red_tree.syntax_node list
