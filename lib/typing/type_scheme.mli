(** Type scheme operations: generalize, instantiate, mark_as_weak.

    These operations are separated from {!Types} to avoid circular dependency
    with {!Type_traversal}. All functions take explicit parameters rather than
    using global state.

    {2 Usage}

    For generalization with value restriction:
    {[
      let scheme =
        if is_value expr then
          Type_scheme.generalize ~level ty
        else
          Type_scheme.generalize_with_filter ~level can_generalize_relaxed ty
    ]}

    For instantiation:
    {[
      let fresh_ty =
        Type_scheme.instantiate ~fresh_var:(fun () -> new_type_variable ()) scheme
    ]} *)

(** {1 Type Variable Marking} *)

val mark_as_weak : level:int -> Types.type_expression -> unit
(** [mark_as_weak ~level ty] marks type variables above the given level as weak
    (non-generalizable). Used for value restriction.

    Weak variables are printed as ['_a] instead of ['a] and will not be
    generalized even if they satisfy other conditions. *)

(** {1 Generalization} *)

val generalize : level:int -> Types.type_expression -> Types.type_scheme
(** [generalize ~level ty] generalizes a type at the given level.
    Type variables with level > [level] and not marked weak become
    universally quantified.

    @param level The current binding level (from {!Typing_context.current_level})
    @param ty The type to generalize
    @return A type scheme with quantified variables *)

val generalize_with_filter :
  level:int ->
  (Types.type_variable -> Types.type_expression -> bool) ->
  Types.type_expression ->
  Types.type_scheme
(** [generalize_with_filter ~level predicate ty] generalizes only variables
    that pass the [predicate] test. Variables that fail the predicate are
    marked as weak instead.

    This is used for relaxed value restriction (Garrigue 2004), where only
    covariant type variables can be generalized for non-values.

    @param level The current binding level
    @param predicate [fun tv ty -> bool] returns true if [tv] can be generalized
    @param ty The type to generalize
    @return A type scheme with quantified variables *)

(** {1 Instantiation} *)

val instantiate :
  fresh_var:(unit -> Types.type_expression) ->
  Types.type_scheme ->
  Types.type_expression
(** [instantiate ~fresh_var scheme] creates fresh type variables for all
    quantified variables in the scheme.

    @param fresh_var Function that creates a fresh type variable at current level
    @param scheme The type scheme to instantiate
    @return The instantiated type expression *)
