(** Generic type traversal utilities.

    See {!Type_traversal} for documentation.

    {2 Internal Design}

    This module uses parameterized internal helpers to reduce duplication:
    - [map_impl ~follow_links] handles both [map] and [map_raw]
    - Row traversals are kept separate for type safety but share structure *)

open Types

(** {1 Iteration} *)

let rec iter f ty =
  let ty = representative ty in
  f ty;

  match ty with
  | TypeVariable _ -> ()
  | TypeConstructor (_, args) -> List.iter (iter f) args
  | TypeTuple elements -> List.iter (iter f) elements
  | TypeArrow (_, arg, result) ->
    iter f arg;
    iter f result
  | TypeRecord row -> iter_row f row
  | TypePolyVariant pv_row -> iter_poly_variant_row f pv_row
  | TypePackage pkg -> List.iter (fun (_, ty) -> iter f ty) pkg.package_signature
  | TypeRowEmpty -> ()

and iter_row f row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> iter f ty
  ) row.row_fields;

  iter f row.row_more

and iter_poly_variant_row f pv_row =
  List.iter (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> iter f ty
    | PVFieldPresent None | PVFieldAbsent -> ()
  ) pv_row.pv_fields;

  iter f pv_row.pv_more

(** {1 Mapping - Internal Implementation}

    The [map_impl] functions parameterize over [~follow_links] to avoid
    duplication between [map] (follows links) and [map_raw] (preserves links).

    When [follow_links] is true, we call [representative] first.
    When false, we preserve linked variables as-is (needed for GADT instantiation). *)

let rec map_impl ~follow_links f ty =
  let ty = if follow_links then representative ty else ty in
  let mapped = match ty with
    | TypeVariable _ -> ty
    | TypeConstructor (path, args) ->
      TypeConstructor (path, List.map (map_impl ~follow_links f) args)
    | TypeTuple elements ->
      TypeTuple (List.map (map_impl ~follow_links f) elements)
    | TypeArrow (label, arg, result) ->
      TypeArrow (label, map_impl ~follow_links f arg, map_impl ~follow_links f result)
    | TypeRecord row ->
      TypeRecord (map_row_impl ~follow_links f row)
    | TypePolyVariant pv_row ->
      TypePolyVariant (map_poly_variant_row_impl ~follow_links f pv_row)
    | TypePackage pkg ->
      TypePackage { pkg with package_signature =
        List.map (fun (name, field_ty) -> (name, map_impl ~follow_links f field_ty)) pkg.package_signature }
    | TypeRowEmpty ->
      TypeRowEmpty
  in
  f mapped

and map_row_impl ~follow_links f row = {
  row_fields = List.map (fun (name, field) ->
    (name, match field with
      | RowFieldPresent ty -> RowFieldPresent (map_impl ~follow_links f ty))
  ) row.row_fields;
  row_more = map_impl ~follow_links f row.row_more;
}

and map_poly_variant_row_impl ~follow_links f pv_row = {
  pv_fields = List.map (fun (name, field) ->
    (name, match field with
      | PVFieldPresent (Some ty) -> PVFieldPresent (Some (map_impl ~follow_links f ty))
      | PVFieldPresent None -> PVFieldPresent None
      | PVFieldAbsent -> PVFieldAbsent)
  ) pv_row.pv_fields;
  pv_more = map_impl ~follow_links f pv_row.pv_more;
  pv_closed = pv_row.pv_closed;
}

(** {1 Mapping - Public API} *)

let map f ty = map_impl ~follow_links:true f ty

let map_row f row = map_row_impl ~follow_links:true f row

(** [map_raw f ty] maps [f] over type [ty] without following links.
    Unlike [map], this preserves linked type variables as-is, which is
    essential for scheme instantiation during GADT pattern matching.
    When a rigid type variable is temporarily linked (for GADT refinement),
    we still want to treat it as a type variable during instantiation. *)
let map_raw f ty = map_impl ~follow_links:false f ty

(** {1 Folding} *)

let rec fold f acc ty =
  let ty = representative ty in
  let acc = f acc ty in

  match ty with
  | TypeVariable _ -> acc
  | TypeConstructor (_, args) -> List.fold_left (fold f) acc args
  | TypeTuple elements -> List.fold_left (fold f) acc elements
  | TypeArrow (_, arg, result) ->
    let acc = fold f acc arg in
    fold f acc result
  | TypeRecord row -> fold_row f acc row
  | TypePolyVariant pv_row -> fold_poly_variant_row f acc pv_row
  | TypePackage pkg -> List.fold_left (fun acc (_, ty) -> fold f acc ty) acc pkg.package_signature
  | TypeRowEmpty -> acc

and fold_row f acc row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> fold f acc ty
  ) acc row.row_fields in

  fold f acc row.row_more

and fold_poly_variant_row f acc pv_row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> fold f acc ty
    | PVFieldPresent None | PVFieldAbsent -> acc
  ) acc pv_row.pv_fields in

  fold f acc pv_row.pv_more

(** {1 Context-Aware Folding}

    Like [fold], but threads a context value through the traversal.
    The context can be transformed at each node (e.g., for variance tracking). *)

let rec fold_with_context f ctx acc ty =
  let ty = representative ty in
  let ctx, acc = f ctx ty acc in

  match ty with
  | TypeVariable _ -> acc
  | TypeConstructor (_, args) ->
    List.fold_left (fun acc arg -> fold_with_context f ctx acc arg) acc args
  | TypeTuple elements ->
    List.fold_left (fun acc elem -> fold_with_context f ctx acc elem) acc elements
  | TypeArrow (_, arg, result) ->
    let acc = fold_with_context f ctx acc arg in
    fold_with_context f ctx acc result
  | TypeRecord row -> fold_row_with_context f ctx acc row
  | TypePolyVariant pv_row -> fold_poly_variant_row_with_context f ctx acc pv_row
  | TypePackage pkg -> List.fold_left (fun acc (_, ty) -> fold_with_context f ctx acc ty) acc pkg.package_signature
  | TypeRowEmpty -> acc

and fold_row_with_context f ctx acc row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> fold_with_context f ctx acc ty
  ) acc row.row_fields in

  fold_with_context f ctx acc row.row_more

and fold_poly_variant_row_with_context f ctx acc pv_row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> fold_with_context f ctx acc ty
    | PVFieldPresent None | PVFieldAbsent -> acc
  ) acc pv_row.pv_fields in

  fold_with_context f ctx acc pv_row.pv_more

(** {1 Type Variable Operations} *)

let collect_variables predicate ty =
  let seen = Hashtbl.create 16 in
  let vars = ref [] in
  iter (function
    | TypeVariable tv when predicate tv ->
      if not (Hashtbl.mem seen tv.id) then begin
        Hashtbl.add seen tv.id ();
        vars := tv :: !vars
      end
    | _ -> ()
  ) ty;
  List.rev !vars

let exists_variable predicate ty =
  let found = ref false in
  try
    iter (function
      | TypeVariable tv when predicate tv ->
        found := true;
        raise Exit
      | _ -> ()
    ) ty;
    false
  with Exit -> true

let for_all_variables predicate ty =
  not (exists_variable (fun tv -> not (predicate tv)) ty)

(** {1 Type Variable Collection with Deduplication} *)

let free_type_variables ty =
  collect_variables (fun _ -> true) ty

let free_type_variables_above_level level ty =
  collect_variables (fun tv -> tv.level > level) ty

(** {1 Weak Variable Detection} *)

let has_weak_variables ty =
  exists_variable (fun tv -> tv.weak) ty

(** {1 Path Qualification} *)

let qualify_path_local ~type_names ~prefix ty =
  map (function
    | TypeConstructor (PathLocal name, args) when List.mem name type_names ->
      TypeConstructor (PathDot (prefix, name), args)
    | ty -> ty
  ) ty
