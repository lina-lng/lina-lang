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
    during unification.

    {2 Constructor Arity}

    The [check_constructor_arity] function validates that constructor arguments
    match expectations. This is used by both expression and pattern inference.

    {2 Module Access}

    The [ensure_module_accessible] function checks that a module type allows
    value access (not a functor or abstract module type).

    {2 Value Restriction}

    The [compute_binding_scheme] function applies the value restriction with
    Garrigue's relaxation when generalizing bindings. *)

open Common

(** {1 Type of Constants} *)

(** [type_of_constant c] returns the type of constant literal [c].

    @param c The constant literal
    @return The type expression for this constant *)
val type_of_constant : Parsing.Syntax_tree.constant -> Types.type_expression

(** {1 Unification} *)

(** [unify_with_env env loc ty1 ty2] unifies two types with environment-aware
    type alias expansion.

    Uses the environment to look up type declarations for alias expansion
    during unification.

    @param env The typing environment for type lookups
    @param loc The source location for error reporting
    @param ty1 The first type to unify
    @param ty2 The second type to unify
    @raise Type_error if types cannot be unified *)
val unify_with_env :
  Environment.t -> Location.t -> Types.type_expression -> Types.type_expression -> unit

(** {1 Constructor Arity Checking} *)

(** [check_constructor_arity loc name ~has_arg ~expects_arg] validates that
    constructor argument presence matches expectations.

    @param loc Source location for error reporting
    @param name Constructor name for error messages
    @param has_arg Whether an argument was provided
    @param expects_arg Whether an argument is expected
    @raise Type_error if argument presence doesn't match expectation *)
val check_constructor_arity :
  Location.t -> string -> has_arg:bool -> expects_arg:bool -> unit

(** {1 Module Access Validation} *)

(** [ensure_module_accessible loc mty] checks that [mty] allows value access.

    Functors and abstract module types cannot have their values accessed directly.

    @param loc Source location for error reporting
    @param mty The module type to check
    @raise Type_error if module type is a functor or abstract *)
val ensure_module_accessible : Location.t -> Module_types.module_type -> unit

(** {1 Value Restriction and Generalization} *)

(** [compute_binding_scheme ~level typed_expr ty] computes a type scheme for a binding
    with value restriction.

    Applies Garrigue's relaxed value restriction:
    - For syntactic values: full generalization
    - For non-values: only covariant type variables are generalized

    @param level The current binding level (from {!Typing_context.current_level})
    @param typed_expr The typed expression (to check if it's a value)
    @param ty The type to generalize
    @return A type scheme with appropriately generalized variables *)
val compute_binding_scheme :
  level:int -> Typed_tree.typed_expression -> Types.type_expression -> Types.type_scheme

(** {1 Error Helpers} *)

(** [error_unbound_variable loc name] raises a type error for unbound variable.

    @param loc Source location for error reporting
    @param name The unbound variable name
    @raise Type_error always *)
val error_unbound_variable : Location.t -> string -> 'a

(** [error_unbound_constructor loc name] raises a type error for unbound constructor.

    @param loc Source location for error reporting
    @param name The unbound constructor name
    @raise Type_error always *)
val error_unbound_constructor : Location.t -> string -> 'a

(** [error_unbound_module loc name] raises a type error for unbound module.

    @param loc Source location for error reporting
    @param name The unbound module name
    @raise Type_error always *)
val error_unbound_module : Location.t -> string -> 'a

(** [error_unbound_type loc name] raises a type error for unbound type.

    @param loc Source location for error reporting
    @param name The unbound type name
    @raise Type_error always *)
val error_unbound_type : Location.t -> string -> 'a

(** [error_unbound_module_type loc name] raises a type error for unbound module type.

    @param loc Source location for error reporting
    @param name The unbound module type name
    @raise Type_error always *)
val error_unbound_module_type : Location.t -> string -> 'a
