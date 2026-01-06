open Common
open Types

module StringMap = Map.Make(String)

type t = {
  values : (Identifier.t * type_scheme) StringMap.t;
  types : type_declaration StringMap.t;
  constructors : constructor_info StringMap.t;
}

let empty = {
  values = StringMap.empty;
  types = StringMap.empty;
  constructors = StringMap.empty;
}

let add_value name id scheme env =
  { env with values = StringMap.add name (id, scheme) env.values }

let find_value name env =
  StringMap.find_opt name env.values

let add_type name decl env =
  { env with types = StringMap.add name decl env.types }

let find_type name env =
  StringMap.find_opt name env.types

let add_constructor name info env =
  { env with constructors = StringMap.add name info env.constructors }

let find_constructor name env =
  StringMap.find_opt name env.constructors

let binary_int_op_type =
  trivial_scheme (TypeArrow (type_int, TypeArrow (type_int, type_int)))

let comparison_int_type =
  trivial_scheme (TypeArrow (type_int, TypeArrow (type_int, type_bool)))

let add_builtin name scheme env =
  add_value name (Identifier.create name) scheme env

let initial =
  let env = empty in
  let env = add_builtin "+" binary_int_op_type env in
  let env = add_builtin "-" binary_int_op_type env in
  let env = add_builtin "*" binary_int_op_type env in
  let env = add_builtin "/" binary_int_op_type env in
  let env = add_builtin "<" comparison_int_type env in
  let env = add_builtin ">" comparison_int_type env in
  let env = add_builtin "<=" comparison_int_type env in
  let env = add_builtin ">=" comparison_int_type env in
  let env = add_builtin "==" comparison_int_type env in
  let env = add_builtin "!=" comparison_int_type env in
  let env = add_builtin "print" (trivial_scheme (TypeArrow (type_int, type_unit))) env in
  env
