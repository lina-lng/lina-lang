(** Type unification for Hindley-Milner inference.

    This module implements Robinson's unification algorithm with extensions for:
    - Occurs check to prevent infinite types
    - Row unification for structural records
    - Type alias expansion

    Unification works by making two types equal through substitution.
    Given types [t1] and [t2], unification either:
    - Succeeds: links type variables to make [t1 = t2]
    - Fails: raises {!Unification_error} with diagnostic information

    {2 Example}

    Unifying ['a -> int] with [bool -> 'b]:
    1. Unify ['a] with [bool]: link ['a] to [bool]
    2. Unify [int] with ['b]: link ['b] to [int]
    3. Result: both types now equal [bool -> int] *)

open Common

(** {1 Errors} *)

(** Raised when unification fails.
    Contains both types and a human-readable message. *)
exception Unification_error of {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Location.t;
  message : string;
}

(** {1 Configuration} *)

(** Type lookup function signature *)
type type_lookup = Types.path -> Types.type_declaration option

(** Type variable factory function signature.
    Used to create fresh type variables during row unification.
    Allows callers to control how type variables are allocated. *)
type fresh_type_var = unit -> Types.type_expression

(** Default type variable factory using global state.
    Creates a fresh type variable at the current level. *)
val default_fresh_type_var : fresh_type_var

(** {1 Type Operations} *)

(** [expand_type ~type_lookup ty] expands type aliases to their definitions.

    Recursively expands type constructors that are aliases until reaching
    a non-alias type. Does not expand types within the result.

    @param type_lookup Function to resolve type aliases
    @param ty The type to expand
    @return The expanded type, or [ty] if not an alias *)
val expand_type :
  type_lookup:type_lookup -> Types.type_expression -> Types.type_expression

(** {1 Unification} *)

(** [unify ~type_lookup loc ty1 ty2] unifies two types.

    Attempts to make [ty1] and [ty2] equal by linking type variables.
    On success, both types will represent the same type.
    On failure, raises {!Unification_error}.

    {3 Algorithm}

    1. Follow links to get representatives via {!Types.representative}
    2. If either is a variable, link it to the other (with occurs check)
    3. If both are constructors, unify component types pairwise
    4. If both are arrows/tuples/records, unify recursively
    5. Otherwise, fail with type mismatch

    {3 Occurs Check}

    Before linking a type variable ['a] to a type [t], checks that ['a]
    does not occur in [t]. This prevents creating infinite types like
    ['a = 'a -> 'a].

    {3 Row Type Unification}

    Records use row polymorphism. When unifying rows:
    - Fields present in both rows: types must unify
    - Fields present in one row only: added to the other row's extension
    - Both rows closed ([TypeRowEmpty]): must have same fields
    - One row open (ends in variable): can absorb extra fields

    Example: [{x:int; ..}] unifies with [{x:int; y:bool}] by binding
    the row variable to [{y:bool}].

    {3 Type Alias Expansion}

    If unification fails and either type is an alias, the types are
    expanded and retried. This allows [type t = int] to unify with [int].
    Cyclic type alias expansion is detected and reported.

    @param type_lookup Function to resolve type aliases
    @param loc Location for error messages
    @param ty1 First type (typically "expected")
    @param ty2 Second type (typically "actual")
    @raise Unification_error when types cannot be unified *)
val unify :
  type_lookup:type_lookup ->
  Location.t -> Types.type_expression -> Types.type_expression -> unit

(** [unify_full ~type_lookup ~fresh_type_var loc ty1 ty2] unifies two types
    with full control over type variable creation.

    This is the most flexible unification function. It allows callers to provide
    their own type variable factory, enabling context-based type variable creation.

    @param type_lookup Function to resolve type aliases
    @param fresh_type_var Function to create fresh type variables during row unification
    @param loc Location for error messages
    @param ty1 First type (typically "expected")
    @param ty2 Second type (typically "actual")
    @raise Unification_error when types cannot be unified *)
val unify_full :
  type_lookup:type_lookup ->
  fresh_type_var:fresh_type_var ->
  Location.t -> Types.type_expression -> Types.type_expression -> unit
