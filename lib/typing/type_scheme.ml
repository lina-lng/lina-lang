(** Type scheme operations: generalize, instantiate, mark_as_weak.

    These operations are separated from {!Types} to avoid circular dependency
    with {!Type_traversal}. All functions take explicit parameters rather than
    using global state. *)

open Types

(** {1 Type Variable Marking} *)

(** [mark_as_weak ~level ty] marks type variables above the given level as weak
    (non-generalizable). Used for value restriction. *)
let mark_as_weak ~level ty =
  Type_traversal.iter (fun t ->
    match representative t with
    | TypeVariable tv when tv.level > level -> tv.weak <- true
    | _ -> ()
  ) ty

(** {1 Generalization} *)

(** [generalize ~level ty] generalizes a type at the given level.
    Type variables with level > [level] and not marked weak become
    universally quantified.

    @param level The current binding level
    @param ty The type to generalize
    @return A type scheme with quantified variables *)
let generalize ~level ty =
  let seen = Hashtbl.create 16 in
  let generalized_vars = ref [] in
  Type_traversal.iter (fun t ->
    match representative t with
    | TypeVariable tv when tv.level > level && not tv.weak ->
      if not (Hashtbl.mem seen tv.id) then begin
        Hashtbl.add seen tv.id ();
        tv.level <- generic_level;
        generalized_vars := tv :: !generalized_vars
      end
    | _ -> ()
  ) ty;
  { quantified_variables = !generalized_vars; body = ty }

(** [generalize_with_filter ~level predicate ty] generalizes only variables
    that pass the [predicate] test. Variables that fail the predicate are
    marked as weak instead.

    This is used for relaxed value restriction (Garrigue 2004), where only
    covariant type variables can be generalized for non-values.

    @param level The current binding level
    @param predicate [fun tv ty -> bool] returns true if [tv] can be generalized
    @param ty The type to generalize
    @return A type scheme with quantified variables *)
let generalize_with_filter ~level predicate ty =
  let seen = Hashtbl.create 16 in
  let generalized_vars = ref [] in
  Type_traversal.iter (fun t ->
    match representative t with
    | TypeVariable tv when tv.level > level && not tv.weak ->
      if not (Hashtbl.mem seen tv.id) then begin
        Hashtbl.add seen tv.id ();
        if predicate tv ty then begin
          tv.level <- generic_level;
          generalized_vars := tv :: !generalized_vars
        end else
          tv.weak <- true
      end
    | _ -> ()
  ) ty;
  { quantified_variables = !generalized_vars; body = ty }

(** {1 Instantiation} *)

(** [instantiate ~fresh_var scheme] creates fresh type variables for all
    quantified variables in the scheme.

    @param fresh_var Function that creates a fresh type variable
    @param scheme The type scheme to instantiate
    @return The instantiated type expression *)
let instantiate ~fresh_var scheme =
  if scheme.quantified_variables = [] then scheme.body
  else begin
    let substitution =
      List.map (fun tv -> (tv.id, fresh_var ())) scheme.quantified_variables
    in
    Type_traversal.map (fun ty ->
      match representative ty with
      | TypeVariable tv ->
        begin match List.assoc_opt tv.id substitution with
        | Some fresh_ty -> fresh_ty
        | None -> ty
        end
      | _ -> ty
    ) scheme.body
  end
