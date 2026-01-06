type level = int

val generic_level : level
val current_level : unit -> level
val enter_level : unit -> unit
val leave_level : unit -> unit
val reset_level : unit -> unit

type type_variable = {
  id : int;
  mutable level : level;
  mutable link : type_expression option;
}

and type_expression =
  | TypeVariable of type_variable
  | TypeConstructor of type_path * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of type_expression * type_expression

and type_path =
  | PathBuiltin of builtin_type
  | PathUser of string

and builtin_type =
  | BuiltinInt
  | BuiltinFloat
  | BuiltinString
  | BuiltinBool
  | BuiltinUnit

val new_type_variable : unit -> type_expression
val new_type_variable_at_level : level -> type_expression

val type_int : type_expression
val type_float : type_expression
val type_string : type_expression
val type_bool : type_expression
val type_unit : type_expression

val representative : type_expression -> type_expression

type type_scheme = {
  quantified_variables : type_variable list;
  body : type_expression;
}

val trivial_scheme : type_expression -> type_scheme
val generalize : type_expression -> type_scheme
val instantiate : type_scheme -> type_expression

type constructor_info = {
  constructor_name : string;
  constructor_argument_type : type_expression option;
  constructor_result_type : type_expression;
  constructor_type_parameters : type_variable list;
}

type type_declaration = {
  declaration_name : string;
  declaration_parameters : type_variable list;
  declaration_kind : type_declaration_kind;
}

and type_declaration_kind =
  | DeclarationAbstract
  | DeclarationVariant of constructor_info list

val pp_type_expression : Format.formatter -> type_expression -> unit
val pp_type_scheme : Format.formatter -> type_scheme -> unit
val type_expression_to_string : type_expression -> string
