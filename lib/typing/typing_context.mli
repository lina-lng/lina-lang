(** Typing context for type inference.

    This module encapsulates the mutable state needed during type inference,
    providing a cleaner interface than global references. The context holds:
    - The typing environment
    - Current generalization level for let-polymorphism
    - Counter for generating unique type variable IDs
    - Lookup functions for type aliases and module types

    {2 Usage}

    Create a context at the start of type inference:
    {[
      let ctx = Typing_context.create Environment.initial in
      let (result, ctx') = infer_expression ctx expr in
      ...
    ]}

    Threading context through inference ensures:
    - No global mutable state
    - Multiple type checks can run concurrently
    - Easier testing with isolated contexts *)

(** The typing context type. *)
type t

(** {1 Context Creation} *)

val create : Environment.t -> t
(** [create env] creates a fresh context with the given environment,
    level set to 1. *)

val with_environment : Environment.t -> t -> t
(** [with_environment env ctx] returns a new context with the given
    environment but preserving level and type variable counter. *)

(** {1 Environment Access} *)

val environment : t -> Environment.t
(** [environment ctx] returns the current environment. *)

(** {1 Level Management}

    Levels implement the generalization algorithm for let-polymorphism.
    Type variables created at a higher level than the current level
    can be generalized when leaving a let-binding. *)

val current_level : t -> int
(** [current_level ctx] returns the current generalization level. *)

val enter_level : t -> t
(** [enter_level ctx] increments the level, used when entering a let binding.
    Returns a new context with level + 1. *)

val leave_level : t -> t
(** [leave_level ctx] decrements the level, used when leaving a let binding.
    Returns a new context with level - 1. *)

(** {1 Type Variable Generation} *)

val fresh_type_variable_id : t -> int * t
(** [fresh_type_variable_id ctx] generates a unique type variable ID.
    Returns the ID and a context with incremented counter. *)

val new_type_variable : t -> Types.type_expression * t
(** [new_type_variable ctx] creates a fresh unbound type variable at the
    current level. Returns the type variable and updated context. *)

val new_type_variable_at_level : t -> int -> Types.type_expression * t
(** [new_type_variable_at_level ctx level] creates a fresh type variable
    at the specified level. *)

val new_rigid_type_variable : t -> Types.type_expression * t
(** [new_rigid_type_variable ctx] creates a rigid type variable for locally
    abstract types. Rigid variables don't unify globally during GADT pattern
    matching - instead, equations are extracted and applied locally. *)

val new_type_variables : t -> int -> Types.type_variable list * t
(** [new_type_variables ctx count] creates [count] fresh type variables.

    Returns the type_variable records (not type_expression wrappers) and the
    updated context. This is a convenience function for creating multiple
    type parameters at once.

    @param ctx The typing context
    @param count Number of type variables to create
    @return A pair [(type_variables, updated_ctx)] *)

(** {1 Type Lookup}

    These functions look up type and module type definitions by path,
    delegating to the environment. Used for type alias expansion and
    module type identity resolution. *)

val type_lookup : t -> Types.path -> Types.type_declaration option
(** [type_lookup ctx path] looks up a type declaration by path.
    Returns [None] for builtin types or unknown paths. *)

val module_type_lookup : t -> Types.path -> Module_types.module_type option
(** [module_type_lookup ctx path] looks up a module type by path.
    Used for expanding [ModTypeIdent] references. *)

(** {1 Environment Modification Helpers}

    These convenience functions combine environment lookup and context update
    in a single call, reducing the common 3-line pattern to 1 line. *)

val add_value :
  string -> Common.Identifier.t -> Types.type_scheme -> Common.Location.t -> t -> t
(** [add_value name id scheme loc ctx] adds a value binding to the environment
    and returns the updated context. *)

val add_type : string -> Types.type_declaration -> t -> t
(** [add_type name decl ctx] adds a type declaration to the environment
    and returns the updated context. *)

val add_module : string -> Module_types.module_binding -> t -> t
(** [add_module name binding ctx] adds a module binding to the environment
    and returns the updated context. *)

val add_module_type : string -> Module_types.module_type option -> t -> t
(** [add_module_type name mty_opt ctx] adds a module type definition to the
    environment and returns the updated context. *)

(** {1 Type Scheme Operations} *)

val generalize : t -> Types.type_expression -> Types.type_scheme
(** [generalize ctx ty] generalizes a type expression, quantifying over
    type variables whose level is higher than the current context level. *)

val instantiate : t -> Types.type_scheme -> Types.type_expression * t
(** [instantiate ctx scheme] creates fresh type variables for all
    quantified variables in the scheme. *)

