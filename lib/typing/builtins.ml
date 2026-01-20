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

(** Binary float operation: float -> float -> float *)
let binary_float_op_type =
  trivial_scheme (TypeArrow (Nolabel, type_float, TypeArrow (Nolabel, type_float, type_float)))

(** Unary int negation: int -> int *)
let unary_int_neg_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, type_int))

(** Unary float negation: float -> float *)
let unary_float_neg_type =
  trivial_scheme (TypeArrow (Nolabel, type_float, type_float))

(** Integer comparison: int -> int -> bool *)
let comparison_int_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, type_int, type_bool)))

(** Polymorphic comparison: 'a -> 'a -> bool
    Used for operators like <, >, <=, >=, =, ==, !=, <> that work on any comparable type. *)
let polymorphic_comparison_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, TypeVariable alpha, TypeArrow (Nolabel, TypeVariable alpha, type_bool));
  }

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

(** Binary boolean operation: bool -> bool -> bool *)
let binary_bool_op_type =
  trivial_scheme (TypeArrow (Nolabel, type_bool, TypeArrow (Nolabel, type_bool, type_bool)))

(** List append: 'a list -> 'a list -> 'a list *)
let list_append_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let list_alpha = TypeConstructor (PathLocal "list", [TypeVariable alpha]) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, list_alpha, TypeArrow (Nolabel, list_alpha, list_alpha));
  }

(** {1 Array Primitives} *)

(** Array make: int -> 'a -> 'a array *)
let array_make_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let array_alpha = type_array (TypeVariable alpha) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, TypeVariable alpha, array_alpha));
  }

(** Array length: 'a array -> int *)
let array_length_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let array_alpha = type_array (TypeVariable alpha) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, array_alpha, type_int);
  }

(** Array unsafe get: 'a array -> int -> 'a (no bounds checking) *)
let array_unsafe_get_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let array_alpha = type_array (TypeVariable alpha) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, array_alpha, TypeArrow (Nolabel, type_int, TypeVariable alpha));
  }

(** Array unsafe set: 'a array -> int -> 'a -> unit (no bounds checking) *)
let array_unsafe_set_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let array_alpha = type_array (TypeVariable alpha) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, array_alpha,
             TypeArrow (Nolabel, type_int,
               TypeArrow (Nolabel, TypeVariable alpha, type_unit)));
  }

(** Create an empty array with polymorphic element type: unit -> 'a array *)
let array_empty_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let array_alpha = type_array (TypeVariable alpha) in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, type_unit, array_alpha);
  }

(** {1 Dict Primitives} *)

(** Dict empty: unit -> ('k, 'v) dict *)
let dict_empty_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, type_unit, dict_kv);
  }

(** Dict get: 'k -> ('k, 'v) dict -> 'v option *)
let dict_get_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  let option_v = TypeConstructor (PathLocal "option", [TypeVariable val_var]) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, TypeVariable key_var,
             TypeArrow (Nolabel, dict_kv, option_v));
  }

(** Dict set: 'k -> 'v -> ('k, 'v) dict -> ('k, 'v) dict *)
let dict_set_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, TypeVariable key_var,
             TypeArrow (Nolabel, TypeVariable val_var,
               TypeArrow (Nolabel, dict_kv, dict_kv)));
  }

(** Dict has: 'k -> ('k, 'v) dict -> bool *)
let dict_has_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, TypeVariable key_var,
             TypeArrow (Nolabel, dict_kv, type_bool));
  }

(** Dict remove: 'k -> ('k, 'v) dict -> ('k, 'v) dict *)
let dict_remove_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, TypeVariable key_var,
             TypeArrow (Nolabel, dict_kv, dict_kv));
  }

(** Dict size: ('k, 'v) dict -> int *)
let dict_size_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, dict_kv, type_int);
  }

(** Dict keys: ('k, 'v) dict -> 'k list *)
let dict_keys_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  let list_k = TypeConstructor (PathLocal "list", [TypeVariable key_var]) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, dict_kv, list_k);
  }

(** Dict entries: ('k, 'v) dict -> ('k * 'v) list *)
let dict_entries_type =
  let key_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let val_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let dict_kv = type_dict (TypeVariable key_var) (TypeVariable val_var) in
  let pair_kv = TypeTuple [TypeVariable key_var; TypeVariable val_var] in
  let list_pairs = TypeConstructor (PathLocal "list", [pair_kv]) in
  {
    quantified_variables = [key_var; val_var];
    body = TypeArrow (Nolabel, dict_kv, list_pairs);
  }

(** {1 Set Primitives} *)

(** Set empty: unit -> 'a set *)
let set_empty_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, type_unit, set_a);
  }

(** Set add: 'a -> 'a set -> 'a set *)
let set_add_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, TypeVariable elem_var,
             TypeArrow (Nolabel, set_a, set_a));
  }

(** Set remove: 'a -> 'a set -> 'a set *)
let set_remove_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, TypeVariable elem_var,
             TypeArrow (Nolabel, set_a, set_a));
  }

(** Set mem: 'a -> 'a set -> bool *)
let set_mem_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, TypeVariable elem_var,
             TypeArrow (Nolabel, set_a, type_bool));
  }

(** Set size: 'a set -> int *)
let set_size_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, set_a, type_int);
  }

(** Set elements: 'a set -> 'a list *)
let set_elements_type =
  let elem_var = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  let set_a = type_set (TypeVariable elem_var) in
  let list_a = TypeConstructor (PathLocal "list", [TypeVariable elem_var]) in
  {
    quantified_variables = [elem_var];
    body = TypeArrow (Nolabel, set_a, list_a);
  }

(** {1 Error Handling} *)

(** Error function: string -> 'a (raises a runtime error) *)
let error_type =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, type_string, TypeVariable alpha);
  }

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

(** {1 List Type} *)

(** Built-in list type: type 'a list = Nil | Cons of 'a * 'a list *)
let list_type_declaration, nil_constructor, cons_constructor =
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ ->
        Common.Compiler_error.internal_error
          "new_type_variable_at_level did not return TypeVariable"
  in

  let list_type = TypeConstructor (PathLocal "list", [TypeVariable alpha]) in

  let nil_ctor = {
    constructor_name = "Nil";
    constructor_tag_index = 0;
    constructor_type_name = "list";
    constructor_argument_type = None;
    constructor_result_type = list_type;
    constructor_type_parameters = [alpha];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let cons_arg_type = TypeTuple [TypeVariable alpha; list_type] in
  let cons_ctor = {
    constructor_name = "Cons";
    constructor_tag_index = 1;
    constructor_type_name = "list";
    constructor_argument_type = Some cons_arg_type;
    constructor_result_type = list_type;
    constructor_type_parameters = [alpha];
    constructor_is_gadt = false;
    constructor_existentials = [];
  } in

  let list_decl = {
    declaration_name = "list";
    declaration_parameters = [alpha];
    declaration_variances = [Covariant];
    declaration_injectivities = [true];
    declaration_manifest = None;
    declaration_kind = DeclarationVariant [nil_ctor; cons_ctor];
    declaration_private = false;
    declaration_constraints = [];
  } in

  (list_decl, nil_ctor, cons_ctor)
