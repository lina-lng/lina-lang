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

(** [set_type_lookup lookup] sets the function used to resolve type aliases.

    Must be called before unification to enable type alias support.
    The lookup function should return [Some decl] for type aliases,
    [None] for abstract or unknown types.

    @param lookup A function from type path to type declaration *)
val set_type_lookup : (Types.path -> Types.type_declaration option) -> unit

(** {1 Type Operations} *)

(** [expand_type ty] expands type aliases to their definitions.

    Recursively expands type constructors that are aliases until reaching
    a non-alias type. Does not expand types within the result.

    @param ty The type to expand
    @return The expanded type, or [ty] if not an alias *)
val expand_type : Types.type_expression -> Types.type_expression

(** {1 Unification} *)

(** [unify loc ty1 ty2] unifies two types.

    Attempts to make [ty1] and [ty2] equal by linking type variables.
    On success, both types will represent the same type.
    On failure, raises {!Unification_error}.

    The unification algorithm:
    1. Follow links to get representatives
    2. If either is a variable, link it to the other (with occurs check)
    3. If both are constructors, unify component types
    4. If both are arrows/tuples/records, unify recursively
    5. Otherwise, fail with type mismatch

    @param loc Location for error messages
    @param ty1 First type (typically "expected")
    @param ty2 Second type (typically "actual")
    @raise Unification_error when types cannot be unified *)
val unify : Location.t -> Types.type_expression -> Types.type_expression -> unit
