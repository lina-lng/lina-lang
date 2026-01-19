(** Built-in types and operators.

    This module defines the built-in types (option, result), their constructors
    (None, Some, Ok, Error), and operators (+, -, *, /, etc.) that are available
    in every Lina program. *)

(** {1 Built-in Type Schemes} *)

(** Binary integer operation: int -> int -> int *)
val binary_int_op_type : Types.type_scheme

(** Integer comparison: int -> int -> bool *)
val comparison_int_type : Types.type_scheme

(** String concatenation: string -> string -> string *)
val string_concat_type : Types.type_scheme

(** Polymorphic print: 'a -> unit *)
val polymorphic_print_type : Types.type_scheme

(** Boolean negation: bool -> bool *)
val bool_not_type : Types.type_scheme

(** Binary boolean operation: bool -> bool -> bool *)
val binary_bool_op_type : Types.type_scheme

(** List append: 'a list -> 'a list -> 'a list *)
val list_append_type : Types.type_scheme

(** {1 Option Type} *)

(** Built-in option type declaration. *)
val option_type_declaration : Types.type_declaration

(** None constructor for option type. *)
val none_constructor : Types.constructor_info

(** Some constructor for option type. *)
val some_constructor : Types.constructor_info

(** {1 Result Type} *)

(** Built-in result type declaration. *)
val result_type_declaration : Types.type_declaration

(** Ok constructor for result type. *)
val ok_constructor : Types.constructor_info

(** Error constructor for result type. *)
val error_constructor : Types.constructor_info

(** {1 List Type} *)

(** Built-in list type declaration: type 'a list = Nil | Cons of 'a * 'a list *)
val list_type_declaration : Types.type_declaration

(** Nil constructor for list type: Nil : 'a list *)
val nil_constructor : Types.constructor_info

(** Cons constructor for list type: Cons : 'a * 'a list -> 'a list *)
val cons_constructor : Types.constructor_info
