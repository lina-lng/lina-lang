(** Variance operations for type parameters.

    This module provides operations for manipulating type parameter variances.
    Variance describes how a type constructor relates to subtyping:
    - Covariant (+): parameter appears in output positions only
    - Contravariant (-): parameter appears in input positions only
    - Invariant: parameter appears in both positions
    - Bivariant: parameter doesn't appear (phantom type)

    {2 Variance Composition Rules}

    When type constructors are nested, variances compose:
    {v
    compose   | +    -    inv  biv
    ----------|--------------------
    +         | +    -    inv  biv
    -         | -    +    inv  biv
    inv       | inv  inv  inv  inv
    biv       | biv  biv  inv  biv
    v}

    {2 Variance Combination Rules}

    When a type variable appears in multiple positions:
    {v
    combine   | +    -    inv  biv
    ----------|--------------------
    +         | +    inv  inv  +
    -         | inv  -    inv  -
    inv       | inv  inv  inv  inv
    biv       | +    -    inv  biv
    v} *)

(** Re-export the variance type from Types for convenience. *)
type t = Types.variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

(** {1 Variance Operations} *)

(** [flip variance] returns the flipped variance.
    Used when entering a contravariant position (e.g., function argument).

    - [flip Covariant = Contravariant]
    - [flip Contravariant = Covariant]
    - [flip Invariant = Invariant]
    - [flip Bivariant = Bivariant] *)
val flip : t -> t

(** [combine v1 v2] combines two variances when a variable appears
    in multiple positions.

    The result is:
    - Covariant if both are covariant or one is bivariant and other is covariant
    - Contravariant if both are contravariant or one is bivariant and other is contra
    - Invariant if variances conflict (one covariant, one contravariant)
    - Bivariant only if both are bivariant *)
val combine : t -> t -> t

(** [combine_opt v1_opt v2_opt] combines two optional variances.
    This is the monoid operation for [t option] used during variance inference:
    - [combine_opt None v = v]
    - [combine_opt v None = v]
    - [combine_opt (Some v1) (Some v2) = Some (combine v1 v2)] *)
val combine_opt : t option -> t option -> t option

(** [compose context position] applies context variance to position variance.
    This is used when a type variable appears inside a type constructor
    whose parameter has a declared variance.

    For example, if we have [type 'a t = ... 'a list ...] where [list] is
    covariant, and we're checking variance in a contravariant context,
    the effective variance is [compose Contravariant Covariant = Contravariant]. *)
val compose : t -> t -> t

(** {1 Variance Compatibility} *)

(** [compatible ~impl ~decl] checks if implementation variance is compatible
    with declared variance.

    The implementation variance must be at least as restrictive as declared:
    - Bivariant (impl) is compatible with any declaration (parameter unused)
    - Any declaration of Bivariant accepts any implementation
    - Covariant impl only compatible with Covariant or Bivariant decl
    - Contravariant impl only compatible with Contravariant or Bivariant decl
    - Invariant impl is compatible with any declaration *)
val compatible : impl:t -> decl:t -> bool

(** {1 Pretty Printing} *)

(** [to_string v] returns the variance as a string annotation.
    - Covariant: "+"
    - Contravariant: "-"
    - Invariant: "" (no annotation)
    - Bivariant: "_" *)
val to_string : t -> string

(** [pp fmt v] pretty-prints a variance. *)
val pp : Format.formatter -> t -> unit

(** [to_annotation v] returns the variance as it would appear in source code.
    Same as [to_string] but with descriptions for documentation. *)
val to_annotation : t -> string

(** {1 Constructor Variances} *)

(** [get_constructor_variances path param_count] returns the variances for
    a type constructor's parameters.

    Built-in types have known variances:
    - [ref]: Invariant (can read and write)
    - Other builtins: no type parameters

    User-defined types are conservatively assumed covariant.
    A more complete implementation would look up declared variances.

    @param path The type constructor path
    @param param_count Number of type parameters
    @return List of variances, one per parameter *)
val get_constructor_variances : Types.path -> int -> t list

(** {1 Variance Checking in Types}

    These functions check how a type variable occurs within a type expression.
    Used for relaxed value restriction. *)

(** Type lookup function for finding type declarations *)
type type_lookup = Types.path -> Types.type_declaration option

(** [set_type_lookup lookup] sets the global type lookup function.
    Should be called before variance checking to enable looking up
    declared variances from type definitions. *)
val set_type_lookup : type_lookup -> unit

(** [check_in_type target_var ty] checks the variance of a type variable
    in a type expression. Entry point that starts in a covariant context.

    @param target_var The type variable to check
    @param ty The type expression to search
    @return The variance of target_var in ty *)
val check_in_type : Types.type_variable -> Types.type_expression -> t

(** [check_in_type_with_lookup ~type_lookup target_var ty] checks variance
    using a specific type lookup function. Restores the previous lookup
    after completion.

    @param type_lookup Function to look up type declarations
    @param target_var The type variable to check
    @param ty The type expression to search
    @return The variance of target_var in ty *)
val check_in_type_with_lookup :
  type_lookup:type_lookup ->
  Types.type_variable ->
  Types.type_expression ->
  t

(** [check_in_context target_var context_variance ty] checks the variance
    of a type variable within a specific context variance.

    This is the general function that recursively traverses the type,
    flipping variance at contravariant positions (function arguments).

    @param target_var The type variable to check
    @param context_variance The current context variance
    @param ty The type expression to search
    @return The variance of target_var in ty *)
val check_in_context : Types.type_variable -> t -> Types.type_expression -> t
