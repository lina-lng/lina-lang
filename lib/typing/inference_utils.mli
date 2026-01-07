(** Shared utilities for type inference.

    This module provides common functions used by multiple inference modules,
    eliminating duplicate code in expression and pattern inference.

    {2 Type of Constants}

    The [type_of_constant] function maps constant literals to their types:
    - Integers map to [int]
    - Floats map to [float]
    - Strings map to [string]
    - Booleans map to [bool]
    - Unit maps to [unit]

    {2 Unification with Environment}

    The [unify_with_env] function sets up the type lookup function from the
    environment before performing unification. This enables type alias expansion
    during unification. *)

open Common

(** [type_of_constant c] returns the type of constant literal [c].

    @param c The constant literal
    @return The type expression for this constant *)
val type_of_constant : Parsing.Syntax_tree.constant -> Types.type_expression

(** [unify_with_env env loc ty1 ty2] unifies two types with environment-aware
    type alias expansion.

    Sets up the type lookup function from [env] before calling [Unification.unify],
    enabling transparent expansion of type aliases during unification.

    @param env The typing environment for type lookups
    @param loc The source location for error reporting
    @param ty1 The first type to unify
    @param ty2 The second type to unify
    @raise Type_error if types cannot be unified *)
val unify_with_env :
  Environment.t -> Location.t -> Types.type_expression -> Types.type_expression -> unit
