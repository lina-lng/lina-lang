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
    | DeclarationRecord _ -> None

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
      let env = add_value name id desc.value_type env in
      (env, (name, id) :: bindings)
    | Module_types.SigType (name, decl) ->
      (add_type name decl env, bindings)
    | Module_types.SigModule (name, mty) ->
      let id = Identifier.create name in
      let binding = Module_types.{ binding_name = name; binding_id = id; binding_type = mty; binding_alias = None } in
      (add_module name binding env, bindings)
    | Module_types.SigModuleType (name, mty_opt) ->
      (add_module_type name mty_opt env, bindings)
  ) (env, []) sig_ in
  (env, List.rev value_bindings)

(** Look up a type declaration by path *)
let rec find_type_by_path path env =
  match path with
  | Types.PathBuiltin _ ->
    (* Builtins like int, bool don't have declarations *)
    None
  | Types.PathLocal name ->
    find_type name env
  | Types.PathIdent _ ->
    (* Module identifiers don't have type declarations directly *)
    None
  | Types.PathDot (parent_path, name) ->
    (* Look up module at parent_path, then find type in its signature *)
    begin match find_module_by_path parent_path env with
    | None -> None
    | Some binding ->
      find_type_in_module_type name binding.Module_types.binding_type
    end
  | Types.PathApply _ ->
    (* Functor application - not supported yet *)
    None

and find_module_by_path path env =
  match path with
  | Types.PathIdent id ->
    (* Look up by identifier name in modules *)
    find_module (Identifier.name id) env
  | Types.PathLocal name ->
    find_module name env
  | Types.PathDot (parent_path, name) ->
    begin match find_module_by_path parent_path env with
    | None -> None
    | Some binding ->
      find_module_in_module_type name binding.Module_types.binding_type
    end
  | Types.PathBuiltin _ | Types.PathApply _ ->
    None

and find_type_in_module_type name mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.find_type_in_sig name sig_
  | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
    None

and find_module_in_module_type name mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    begin match Module_types.find_module_in_sig name sig_ with
    | Some inner_mty ->
      let id = Identifier.create name in
      Some Module_types.{ binding_name = name; binding_id = id; binding_type = inner_mty; binding_alias = None }
    | None -> None
    end
  | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
    None

(** Look up a module type definition by path.
    Returns [Some mty] if found and concrete, [None] if not found or abstract. *)
let find_module_type_by_path path env =
  match path with
  | Types.PathLocal name ->
    begin match find_module_type name env with
    | Some (Some mty) -> Some mty
    | _ -> None
    end
  | Types.PathIdent id ->
    begin match find_module_type (Identifier.name id) env with
    | Some (Some mty) -> Some mty
    | _ -> None
    end
  | Types.PathDot (parent_path, name) ->
    begin match find_module_by_path parent_path env with
    | Some binding ->
      begin match binding.Module_types.binding_type with
      | Module_types.ModTypeSig sig_ ->
        begin match Module_types.find_module_type_in_sig name sig_ with
        | Some (Some mty) -> Some mty
        | _ -> None
        end
      | _ -> None
      end
    | None -> None
    end
  | Types.PathBuiltin _ | Types.PathApply _ ->
    None

(* Iteration functions for LSP features *)

let fold_values f env acc =
  StringMap.fold (fun name (id, scheme) acc -> f name id scheme acc) env.values acc

let fold_types f env acc =
  StringMap.fold f env.types acc

let fold_constructors f env acc =
  StringMap.fold f env.constructors acc

let fold_modules f env acc =
  StringMap.fold f env.modules acc

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
