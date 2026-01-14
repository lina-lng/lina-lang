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

    Note: This version doesn't look up declared variances from type definitions.
    Use [compute_binding_scheme_with_env] for full accuracy with user-defined types.

    @param level The current binding level (from {!Typing_context.current_level})
    @param typed_expr The typed expression (to check if it's a value)
    @param ty The type to generalize
    @return A type scheme with appropriately generalized variables *)
val compute_binding_scheme :
  level:int -> Typed_tree.typed_expression -> Types.type_expression -> Types.type_scheme

(** [compute_binding_scheme_with_env ~level ~env typed_expr ty] is like
    [compute_binding_scheme] but uses the environment to look up declared
    variances for user-defined type constructors.

    This correctly handles types with non-covariant parameters, e.g.:
    - [type 'a consumer = Consumer of ('a -> unit)] - 'a is contravariant

    @param level The current binding level
    @param env The typing environment for type lookup
    @param typed_expr The typed expression (to check if it's a value)
    @param ty The type to generalize
    @return A type scheme with appropriately generalized variables *)
val compute_binding_scheme_with_env :
  level:int ->
  env:Environment.t ->
  Typed_tree.typed_expression ->
  Types.type_expression ->
  Types.type_scheme

(** {1 Module Path Extraction} *)

(** [extract_typed_module_path mexpr] extracts a path from a typed module expression
    if it's a simple path reference.

    This is useful for applicative functor semantics where we need to track
    the paths of functor and argument to build [F(A).t] paths.

    @param mexpr The typed module expression
    @return [Some path] if the expression is a simple module path, [None] otherwise *)
val extract_typed_module_path : Typed_tree.typed_module_expression -> Types.path option

(** {1 Constructor Instantiation} *)

(** [instantiate_constructor_with_ctx ctx ctor] instantiates a constructor
    using the context's current level for fresh type variables.

    This is a convenience wrapper around {!Type_utils.instantiate_constructor}
    that uses the typing context's level instead of requiring a fresh_var function.

    @param ctx The typing context (used for level)
    @param ctor The constructor to instantiate
    @return Tuple of (argument_type option, result_type) *)
val instantiate_constructor_with_ctx :
  Typing_context.t ->
  Types.constructor_info ->
  Types.type_expression option * Types.type_expression

(** {1 Constructor Lookup} *)

(** Result of looking up and instantiating a constructor. *)
type constructor_result = {
  constructor_info : Types.constructor_info;
  expected_arg_type : Types.type_expression option;
  result_type : Types.type_expression;
}

(** [lookup_constructor ctx loc name] looks up a constructor by name and instantiates it.

    Combines constructor lookup with instantiation in a single operation.
    This is used by both expression and pattern inference.

    @param ctx The typing context
    @param loc Source location for error messages
    @param name The constructor name
    @return Constructor info with instantiated argument and result types
    @raise Type_error if constructor is not found *)
val lookup_constructor :
  Typing_context.t -> Location.t -> string -> constructor_result

(** [check_private_type ctx loc ctor_info] checks that a constructor's type is not private.

    Private types cannot be constructed directly (but can be pattern matched).
    This check is only needed for expressions, not patterns.

    @param ctx The typing context
    @param loc Source location for error messages
    @param ctor_info The constructor info to check
    @raise Type_error if the type is private *)
val check_private_type :
  Typing_context.t -> Location.t -> Types.constructor_info -> unit

(** {1 Signature Match Context Creation} *)

(** [make_module_type_lookup ctx] creates a function that looks up module types
    from the context's environment.

    Handles both simple paths (PathIdent) and qualified paths (PathDot, PathLocal).
    This is used when creating signature matching contexts.

    @param ctx The typing context
    @return A lookup function for module types by path *)
val make_module_type_lookup :
  Typing_context.t -> Types.path -> Module_types.module_type option

(** [make_match_context ctx] creates a signature matching context from a typing context.

    The context provides lookup functions needed for signature matching:
    - Type lookup for alias expansion during unification
    - Module type lookup for named module type resolution

    @param ctx The typing context
    @return A match context for use with {!Signature_match} functions *)
val make_match_context : Typing_context.t -> Signature_match.match_context

(** {1 Tolerant Inference Error Types} *)

(** Unification error details for tolerant inference.
    Used by LSP features that need to continue after errors. *)
type unification_error_details = {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Common.Location.t;
  message : string;
}

(** Error information from tolerant inference.
    Captures both compiler errors and unification failures. *)
type inference_error =
  | CompilerError of Common.Compiler_error.t
  | UnificationError of unification_error_details

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
