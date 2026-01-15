open Common
open Types

module StringMap = Map.Make(String)

(** Value binding with definition location for go-to-definition support. *)
type value_binding = {
  binding_id : Identifier.t;
  binding_scheme : type_scheme;
  binding_location : Location.t;
}

type t = {
  values : value_binding StringMap.t;
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

let add_value name id scheme location env =
  let binding = { binding_id = id; binding_scheme = scheme; binding_location = location } in
  { env with values = StringMap.add name binding env.values }

let find_value name env =
  StringMap.find_opt name env.values

(** Get all value bindings as a list of (name, identifier, scheme) tuples.
    Used for comparing environments in or-patterns. *)
let get_value_bindings env =
  StringMap.fold (fun name binding acc ->
    (name, binding.binding_id, binding.binding_scheme) :: acc
  ) env.values []

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
    | DeclarationExtensible -> None

let binary_int_op_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, type_int, type_int)))

let comparison_int_type =
  trivial_scheme (TypeArrow (Nolabel, type_int, TypeArrow (Nolabel, type_int, type_bool)))

let add_builtin name scheme env =
  add_value name (Identifier.create name) scheme Location.none env

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
      (* Use Location.none for opened values - they come from signatures without source locations *)
      let env = add_value name id desc.value_type Location.none env in
      (env, (name, id) :: bindings)
    | Module_types.SigType (name, decl) ->
      (add_type name decl env, bindings)
    | Module_types.SigModule (name, mty) ->
      let id = Identifier.create name in
      let binding = Module_types.{ binding_name = name; binding_id = id; binding_type = mty; binding_alias = None } in
      (add_module name binding env, bindings)
    | Module_types.SigModuleType (name, mty_opt) ->
      (add_module_type name mty_opt env, bindings)
    | Module_types.SigExtensionConstructor ctor ->
      (* Add extension constructor to environment when opening module *)
      (add_constructor ctor.Types.constructor_name ctor env, bindings)
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
  | Types.PathApply (func_path, arg_path) ->
    (* Functor application: F(M) - apply the functor to get result type *)
    begin match find_module_by_path func_path env with
    | None -> None
    | Some func_binding ->
      begin match func_binding.Module_types.binding_type with
      | Module_types.ModTypeFunctor (param, result_mty) ->
        begin match param with
        | Module_types.FunctorParamNamed { parameter_id; _ } ->
          (* Substitute the argument path for the parameter in the result type *)
          let param_path = Types.PathIdent parameter_id in
          let substituted_mty =
            Type_utils.substitute_path_in_module_type ~old_path:param_path ~new_path:arg_path result_mty
          in
          let id = Common.Identifier.create "<apply>" in
          Some Module_types.{ binding_name = "<apply>"; binding_id = id;
                              binding_type = substituted_mty; binding_alias = None }
        | Module_types.FunctorParamUnit ->
          (* Generative functor - just return the result *)
          let id = Common.Identifier.create "<apply>" in
          Some Module_types.{ binding_name = "<apply>"; binding_id = id;
                              binding_type = result_mty; binding_alias = None }
        end
      | _ -> None
      end
    end
  | Types.PathBuiltin _ ->
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

(** Find a constructor in a module signature by name. *)
let find_constructor_in_module_type name mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.find_constructor_in_sig name sig_
  | Module_types.ModTypeFunctor _ | Module_types.ModTypeIdent _ ->
    None

(** Look up a constructor by a qualified path (module path + constructor name).
    For example, ["M"; "Some"] looks for constructor Some in module M. *)
let find_constructor_by_path path_strings ctor_name env =
  match path_strings with
  | [] ->
    (* No module path - look up directly in environment *)
    find_constructor ctor_name env
  | first :: rest ->
    (* Qualified path - build internal path and look up module *)
    let path = List.fold_left
      (fun acc name -> Types.PathDot (acc, name))
      (Types.PathLocal first)
      rest
    in
    match find_module_by_path path env with
    | Some binding -> find_constructor_in_module_type ctor_name binding.Module_types.binding_type
    | None -> None

(* Iteration functions for LSP features *)

let fold_values f env acc =
  StringMap.fold (fun name binding acc -> f name binding.binding_id binding.binding_scheme acc) env.values acc

let fold_types f env acc =
  StringMap.fold f env.types acc

let fold_constructors f env acc =
  StringMap.fold f env.constructors acc

let fold_modules f env acc =
  StringMap.fold f env.modules acc

(** [polymorphic_print_type] creates the type scheme ['a -> unit] for print.
    The type variable is created at generic level so it can be instantiated
    to any type. *)
let polymorphic_print_type =
  (* Create a type variable at generic level for polymorphism *)
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ -> failwith "new_type_variable_at_level must return TypeVariable"
  in
  {
    quantified_variables = [alpha];
    body = TypeArrow (Nolabel, TypeVariable alpha, type_unit);
  }

(** Built-in option type: type 'a option = None | Some of 'a

    The option type is fundamental for optional arguments - when a ?x:int
    parameter is omitted, it receives None; when provided with ~x:value,
    it receives Some value. *)
let option_type_declaration, none_constructor, some_constructor =
  (* Create a type variable for 'a at generic level so it can be instantiated *)
  let alpha = match new_type_variable_at_level generic_level with
    | TypeVariable tv -> tv
    | _ -> failwith "new_type_variable_at_level must return TypeVariable"
  in

  (* Result type: 'a option *)
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

let initial =
  let env = empty in

  (* Add option type and its constructors *)
  let env = add_type "option" option_type_declaration env in
  let env = add_constructor "None" none_constructor env in
  let env = add_constructor "Some" some_constructor env in

  (* Add arithmetic operators *)
  let env = add_builtin "+" binary_int_op_type env in
  let env = add_builtin "-" binary_int_op_type env in
  let env = add_builtin "*" binary_int_op_type env in
  let env = add_builtin "/" binary_int_op_type env in

  (* Add string concatenation operator *)
  let string_concat_type =
    trivial_scheme (TypeArrow (Nolabel, type_string, TypeArrow (Nolabel, type_string, type_string)))
  in
  let env = add_builtin "^" string_concat_type env in

  (* Add comparison operators *)
  let env = add_builtin "<" comparison_int_type env in
  let env = add_builtin ">" comparison_int_type env in
  let env = add_builtin "<=" comparison_int_type env in
  let env = add_builtin ">=" comparison_int_type env in
  let env = add_builtin "=" comparison_int_type env in
  let env = add_builtin "==" comparison_int_type env in
  let env = add_builtin "!=" comparison_int_type env in
  let env = add_builtin "<>" comparison_int_type env in

  (* Add print function *)
  let env = add_builtin "print" polymorphic_print_type env in
  env
