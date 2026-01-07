open Common
open Types

module StringMap = Map.Make(String)

type t = {
  values : (Identifier.t * type_scheme) StringMap.t;
  types : type_declaration StringMap.t;
  constructors : constructor_info StringMap.t;
  (* Module system additions *)
  modules : Module_types.module_binding StringMap.t;
  module_types : Module_types.module_type option StringMap.t;  (* None = abstract *)
}

let empty = {
  values = StringMap.empty;
  types = StringMap.empty;
  constructors = StringMap.empty;
  modules = StringMap.empty;
  module_types = StringMap.empty;
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

let find_type_constructors type_name env =
  match find_type type_name env with
  | None -> None
  | Some decl ->
    match decl.declaration_kind with
    | DeclarationAbstract -> None
    | DeclarationVariant constructors -> Some constructors

let binary_int_op_type =
  trivial_scheme (TypeArrow (type_int, TypeArrow (type_int, type_int)))

let comparison_int_type =
  trivial_scheme (TypeArrow (type_int, TypeArrow (type_int, type_bool)))

let add_builtin name scheme env =
  add_value name (Identifier.create name) scheme env

(* Module operations *)

let add_module name binding env =
  { env with modules = StringMap.add name binding env.modules }

let find_module name env =
  StringMap.find_opt name env.modules

let add_module_type name mty_opt env =
  { env with module_types = StringMap.add name mty_opt env.module_types }

let find_module_type name env =
  StringMap.find_opt name env.module_types

(** Open a module: bring its signature contents into scope.
    Returns (updated_env, list of (value_name, identifier) pairs for opened values) *)
let open_module (sig_ : Module_types.signature) env =
  let env, value_bindings = List.fold_left (fun (env, bindings) item ->
    match item with
    | Module_types.SigValue (name, desc) ->
      let id = Identifier.create name in
      let env = add_value name id desc.val_type env in
      (env, (name, id) :: bindings)
    | Module_types.SigType (name, decl) ->
      (add_type name decl env, bindings)
    | Module_types.SigModule (name, mty) ->
      let id = Module_types.fresh_module_ident name in
      let binding = Module_types.{ mod_id = id; mod_type = mty; mod_alias = None } in
      (add_module name binding env, bindings)
    | Module_types.SigModuleType (name, mty_opt) ->
      (add_module_type name mty_opt env, bindings)
  ) (env, []) sig_ in
  (env, List.rev value_bindings)

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
