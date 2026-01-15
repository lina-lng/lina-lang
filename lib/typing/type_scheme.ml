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

(** Internal generalization with configurable predicate.
    Type variables with level > [level] and not marked weak are candidates.
    Those passing [should_generalize] become quantified; others are marked weak.

    Clears the rigid flag on generalized variables - it was only needed during
    initial type-checking for GADT equation extraction. Once generalized, the
    variable becomes a normal polymorphic variable that can be instantiated freely. *)
let generalize_impl ~level ~should_generalize ty =
  let seen = Hashtbl.create 16 in
  let generalized_vars = ref [] in
  Type_traversal.iter (fun t ->
    match representative t with
    | TypeVariable tv when tv.level > level && not tv.weak ->
      if not (Hashtbl.mem seen tv.id) then begin
        Hashtbl.add seen tv.id ();
        if should_generalize tv then begin
          tv.level <- generic_level;
          tv.rigid <- false;
          generalized_vars := tv :: !generalized_vars
        end else
          tv.weak <- true
      end
    | _ -> ()
  ) ty;
  { quantified_variables = !generalized_vars; body = ty }

(** [generalize ~level ty] generalizes a type at the given level.
    Type variables with level > [level] and not marked weak become
    universally quantified.

    @param level The current binding level
    @param ty The type to generalize
    @return A type scheme with quantified variables *)
let generalize ~level ty =
  generalize_impl ~level ~should_generalize:(fun _ -> true) ty

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
  generalize_impl ~level ~should_generalize:(fun tv -> predicate tv ty) ty

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
    (* Use a recursive function that handles the substitution properly.
       The complexity arises from two different link scenarios:
       1. Normal unification links: t1 -> t0 where generalization collected t0
       2. GADT refinement links: rigid_a -> int (temporary during branch)

       For case 1, we need to follow links to find the quantified variable.
       For case 2, we need to NOT follow links and substitute the rigid variable directly.

       We handle this by using map_raw but recursively substituting when we
       encounter a linked variable whose representative is a complex type. *)
    let rec substitute ty =
      match ty with
      | TypeVariable tv ->
        (* First check if this exact variable is in the substitution *)
        begin match List.assoc_opt tv.id substitution with
        | Some fresh_ty -> fresh_ty
        | None ->
          (* Check the representative *)
          let repr = representative ty in
          begin match repr with
          | TypeVariable repr_tv ->
            (* Representative is also a variable - check if IT's in the substitution *)
            begin match List.assoc_opt repr_tv.id substitution with
            | Some fresh_ty -> fresh_ty
            | None -> repr  (* Neither in substitution, return representative *)
            end
          | _ ->
            (* Representative is a complex type (e.g., TypeArrow).
               This happens when the body is a linked variable.
               Recursively substitute within the representative. *)
            substitute_in_type repr
          end
        end
      | _ -> ty
    and substitute_in_type ty =
      match ty with
      | TypeVariable _ -> substitute ty
      | TypeConstructor (path, args) ->
        TypeConstructor (path, List.map substitute_in_type args)
      | TypeTuple elements ->
        TypeTuple (List.map substitute_in_type elements)
      | TypeArrow (label, arg, result) ->
        TypeArrow (label, substitute_in_type arg, substitute_in_type result)
      | TypeRecord row ->
        TypeRecord (substitute_in_row row)
      | TypePolyVariant pv_row ->
        TypePolyVariant (substitute_in_pv_row pv_row)
      | TypePackage pkg ->
        TypePackage { pkg with package_signature =
          List.map (fun (n, ty) -> (n, substitute_in_type ty)) pkg.package_signature }
      | TypeRowEmpty -> TypeRowEmpty
    and substitute_in_row row = {
      Types.row_fields = List.map (fun (name, field) ->
        (name, match field with
          | Types.RowFieldPresent ty -> Types.RowFieldPresent (substitute_in_type ty))
      ) row.Types.row_fields;
      row_more = substitute_in_type row.Types.row_more;
    }
    and substitute_in_pv_row pv_row = {
      Types.pv_fields = List.map (fun (name, field) ->
        (name, match field with
          | Types.PVFieldPresent (Some ty) -> Types.PVFieldPresent (Some (substitute_in_type ty))
          | Types.PVFieldPresent None -> Types.PVFieldPresent None
          | Types.PVFieldAbsent -> Types.PVFieldAbsent)
      ) pv_row.Types.pv_fields;
      pv_more = substitute_in_type pv_row.Types.pv_more;
      pv_closed = pv_row.Types.pv_closed;
    }
    in
    substitute_in_type scheme.body
  end

(** [instantiate_all_fresh ~fresh_var ty] replaces ALL type variables in [ty]
    with fresh type variables. This is useful for constraint checking where
    we need fresh copies of constraint types to avoid pollution between uses.

    @param fresh_var Function that creates a fresh type variable
    @param ty The type expression to instantiate
    @return A copy of [ty] with all type variables replaced by fresh ones *)
let instantiate_all_fresh ~fresh_var ty =
  let substitution = Hashtbl.create 16 in
  Type_traversal.map (fun t ->
    match representative t with
    | TypeVariable tv ->
      begin match Hashtbl.find_opt substitution tv.id with
      | Some fresh_ty -> fresh_ty
      | None ->
        let fresh_ty = fresh_var () in
        Hashtbl.add substitution tv.id fresh_ty;
        fresh_ty
      end
    | _ -> t
  ) ty
