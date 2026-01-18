(** Built-in types and operators.

    This module defines the built-in types (option, result), their constructors
    (None, Some, Ok, Error), and operators (+, -, *, /, etc.) that are available
    in every Lina program.

    The option type is fundamental for optional arguments - when a ?x:int
    parameter is omitted, it receives None; when provided with ~x:value,
    it receives Some value.

    The result type is used for error handling: Ok for success values,
    Error for failure values. *)

open Types

(** {1 Built-in Type Schemes} *)

(** Binary integer operation: int -> int -> int *)
let binary_int_op_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, type_int, type_int)))

(** Integer comparison: int -> int -> bool *)
let comparison_int_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, type_int, type_bool)))

(** String concatenation: string -> string -> string *)
let string_concat_type =
  trivial_scheme (TypeArrow (Nolabel, type_string, TypeArrow (Nolabel, type_string, type_string)))

(** Polymorphic print: 'a -> unit *)
let polymorphic_print_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, TypeVariable alpha, type_unit);
  }

(** Boolean negation: bool -> bool *)
let bool_not_type =
  trivial_scheme (TypeArrow (Nolabel, type_bool, type_bool))

(** {1 Option Type} *)

(** Built-in option type: type 'a option = None | Some of 'a *)
let option_type_declaration, none_constructor, some_constructor =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in

  let option_result_type = TypeConstructor (PathLocal "option", [TypeVariable alpha]) in

  let none_ctor = {
    constructor_name = "None";
    constructor_tag_index = 0;
    constructor_type_name = "option";
    constructor_argument_type = None;
    constructor_result_type = option_result_type;
    constructor_type_parameters = [alpha];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let some_ctor = {
    constructor_name = "Some";
    constructor_tag_index = 1;
    constructor_type_name = "option";
    constructor_argument_type = Some (TypeVariable alpha);
    constructor_result_type = option_result_type;
    constructor_type_parameters = [alpha];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let option_decl = {
    declaration_name = "option";
    declaration_parameters = [alpha];
    declaration_variances = [Covariant];
    declaration_injectivities = [true];
    declaration_manifest = None;
    declaration_kind = DeclarationVariant [none_ctor; some_ctor];
    declaration_private = false;
    declaration_constraints = [];
  } in

  (option_decl, none_ctor, some_ctor)

(** {1 Result Type} *)

(** Built-in result type: type ('a, 'e) result = Ok of 'a | Error of 'e *)
let result_type_declaration, ok_constructor, error_constructor =
  let ok_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in

  let err_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in

  let result_type = TypeConstructor (PathLocal "result", [TypeVariable ok_var; TypeVariable err_var]) in

  let ok_ctor = {
    constructor_name = "Ok";
    constructor_tag_index = 0;
    constructor_type_name = "result";
    constructor_argument_type = Some (TypeVariable ok_var);
    constructor_result_type = result_type;
    constructor_type_parameters = [ok_var; err_var];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let error_ctor = {
    constructor_name = "Error";
    constructor_tag_index = 1;
    constructor_type_name = "result";
    constructor_argument_type = Some (TypeVariable err_var);
    constructor_result_type = result_type;
    constructor_type_parameters = [ok_var; err_var];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let result_decl = {
    declaration_name = "result";
    declaration_parameters = [ok_var; err_var];
    declaration_variances = [Covariant; Covariant];
    declaration_injectivities = [true; true];
    declaration_manifest = None;
    declaration_kind = DeclarationVariant [ok_ctor; error_ctor];
    declaration_private = false;
    declaration_constraints = [];
  } in

  (result_decl, ok_ctor, error_ctor)
