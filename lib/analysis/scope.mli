(** Scope analysis data structures.

    This module provides the core types for tracking bindings and their usage
    throughout a program. Used by {!Scope_analyzer} to build scope information
    and by {!Unused_detector} to find unused bindings. *)

(** {1 Binding Information} *)

(** The kind of binding (variable, function, parameter, etc.). *)
type binding_kind =
  | Variable
      (** Regular let-bound variable. *)
  | Function of { is_recursive : bool }
      (** Function binding. [is_recursive] is true for [let rec]. *)
  | Parameter
      (** Function parameter. *)
  | PatternVar
      (** Variable bound in a pattern (match arm, let destructuring). *)
  | ModuleAlias
      (** Module alias binding [module M = ...]. *)
  | TypeName
      (** Type definition [type t = ...]. *)
  | Constructor
      (** Variant constructor [type t = Foo of ...]. *)
  | RecordField
      (** Record field [type t = { field : ... }]. *)
  | OpenedBinding
      (** Binding introduced via [open M]. *)
  | IncludedBinding
      (** Binding introduced via [include M]. *)
  | External
      (** External (FFI) binding. *)
[@@deriving show, eq]

(** How a binding is accessed. *)
type access_kind =
  | Read
      (** The binding's value is read. *)
  | Write
      (** The binding's value is written (assignment). *)
  | ReadWrite
      (** Both read and write. *)
  | Pattern
      (** Used in a pattern match (for constructors). *)
  | Construct
      (** Used to construct a value (for constructors). *)
  | Project
      (** Used to access a field (for record fields). *)
[@@deriving show, eq]

(** A reference to a binding. *)
type reference = {
  ref_location : Common.Location.t;
  ref_access : access_kind;
}
[@@deriving show]

(** A binding introduced in a scope. *)
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

(** {1 Scope Information} *)

(** The kind of scope (determines scoping rules). *)
type scope_kind =
  | ScopeModule of { name : string option }
      (** Module scope. *)
  | ScopeFunction of { name : string option; is_recursive : bool }
      (** Function body scope. *)
  | ScopeLet
      (** Let-expression scope. *)
  | ScopeMatch
      (** Match arm scope. *)
  | ScopeBlock
      (** Generic block scope. *)
[@@deriving show, eq]

(** A lexical scope containing bindings. *)
type scope = {
  scope_id : int;
  scope_kind : scope_kind;
  scope_parent : scope option;
  scope_bindings : (string, binding) Hashtbl.t;
  mutable scope_children : scope list;
}

(** {1 Scope Tree} *)

(** The complete scope tree for a compilation unit. *)
type scope_tree = {
  tree_root : scope;
  tree_all_scopes : scope list;
  tree_all_bindings : binding list;
}

(** {1 Scope Operations} *)

(** [create_scope kind parent] creates a new scope with the given kind and parent. *)
val create_scope : scope_kind -> scope option -> scope

(** [add_binding scope binding] adds a binding to the scope.
    Returns the binding for convenience. *)
val add_binding : scope -> binding -> binding

(** [find_binding scope name] looks up a binding by name in the scope
    and its ancestors. *)
val find_binding : scope -> string -> binding option

(** [find_binding_local scope name] looks up a binding only in the
    immediate scope (not ancestors). *)
val find_binding_local : scope -> string -> binding option

(** [add_reference binding ref] adds a reference to a binding. *)
val add_reference : binding -> reference -> unit

(** [mark_exported binding] marks a binding as exported. *)
val mark_exported : binding -> unit

(** {1 Query Functions} *)

(** [has_read_references binding] returns true if the binding has any
    Read or ReadWrite references. *)
val has_read_references : binding -> bool

(** [has_write_references binding] returns true if the binding has any
    Write or ReadWrite references. *)
val has_write_references : binding -> bool

(** [has_pattern_references binding] returns true if the binding is
    used in a pattern match (for constructors). *)
val has_pattern_references : binding -> bool

(** [has_construct_references binding] returns true if the binding is
    used to construct values (for constructors). *)
val has_construct_references : binding -> bool

(** [has_project_references binding] returns true if the binding is
    used for field access (for record fields). *)
val has_project_references : binding -> bool

(** [is_intentionally_unused binding] returns true if the binding's name
    starts with underscore (convention for unused bindings). *)
val is_intentionally_unused : binding -> bool

(** [reference_count binding] returns the total number of references. *)
val reference_count : binding -> int
