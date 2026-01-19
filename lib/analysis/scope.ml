type binding_kind =
  | Variable
  | Function of { is_recursive : bool }
  | Parameter
  | PatternVar
  | ModuleAlias
  | TypeName
  | Constructor
  | RecordField
  | OpenedBinding
  | IncludedBinding
  | External
[@@deriving show, eq]

type access_kind =
  | Read
  | Write
  | ReadWrite
  | Pattern
  | Construct
  | Project
[@@deriving show, eq]

type reference = {
  ref_location : Common.Location.t;
  ref_access : access_kind;
}
[@@deriving show]

type binding = {
  bind_name : string;
  bind_id : Common.Identifier.t;
  mutable bind_kind : binding_kind;
  bind_location : Common.Location.t;
  bind_type : Typing.Types.type_expression option;
  mutable bind_references : reference list;
  mutable bind_exported : bool;
  bind_scope_id : int;
}
[@@deriving show]

type scope_kind =
  | ScopeModule of { name : string option }
  | ScopeFunction of { name : string option; is_recursive : bool }
  | ScopeLet
  | ScopeMatch
  | ScopeBlock
[@@deriving show, eq]

type scope = {
  scope_id : int;
  scope_kind : scope_kind;
  scope_parent : scope option;
  scope_bindings : (string, binding) Hashtbl.t;
  mutable scope_children : scope list;
}

type scope_tree = {
  tree_root : scope;
  tree_all_scopes : scope list;
  tree_all_bindings : binding list;
}

let next_scope_id = ref 0

let create_scope kind parent =
  let scope_id = !next_scope_id in
  incr next_scope_id;
  let scope = {
    scope_id;
    scope_kind = kind;
    scope_parent = parent;
    scope_bindings = Hashtbl.create 16;
    scope_children = [];
  } in

  Option.iter (fun p -> p.scope_children <- scope :: p.scope_children) parent;
  scope

let add_binding scope binding =
  Hashtbl.replace scope.scope_bindings binding.bind_name binding;
  binding

let rec find_binding scope name =
  match Hashtbl.find_opt scope.scope_bindings name with
  | Some binding -> Some binding
  | None ->
      match scope.scope_parent with
      | Some parent -> find_binding parent name
      | None -> None

let find_binding_local scope name =
  Hashtbl.find_opt scope.scope_bindings name

let add_reference binding ref =
  binding.bind_references <- ref :: binding.bind_references

let mark_exported binding =
  binding.bind_exported <- true

let has_reference_matching predicate binding =
  List.exists (fun r -> predicate r.ref_access) binding.bind_references

let has_read_references =
  has_reference_matching (function Read | ReadWrite -> true | _ -> false)

let has_write_references =
  has_reference_matching (function Write | ReadWrite -> true | _ -> false)

let has_pattern_references =
  has_reference_matching (function Pattern -> true | _ -> false)

let has_construct_references =
  has_reference_matching (function Construct -> true | _ -> false)

let has_project_references =
  has_reference_matching (function Project -> true | _ -> false)

let is_intentionally_unused binding =
  String.length binding.bind_name > 0 && binding.bind_name.[0] = '_'

let reference_count binding =
  List.length binding.bind_references
