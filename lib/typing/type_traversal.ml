(** Generic type traversal utilities.

    See {!Type_traversal} for documentation. *)

open Types

let rec iter f ty =
  let ty = representative ty in
  f ty;
  match ty with
  | TypeVariable _ -> ()
  | TypeConstructor (_, args) -> List.iter (iter f) args
  | TypeTuple elements -> List.iter (iter f) elements
  | TypeArrow (arg, result) ->
    iter f arg;
    iter f result
  | TypeRecord row -> iter_row f row
  | TypeRowEmpty -> ()

and iter_row f row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> iter f ty
  ) row.row_fields;
  iter f row.row_more

let rec map f ty =
  let ty = representative ty in
  let mapped = match ty with
    | TypeVariable _ -> ty
    | TypeConstructor (path, args) ->
      TypeConstructor (path, List.map (map f) args)
    | TypeTuple elements ->
      TypeTuple (List.map (map f) elements)
    | TypeArrow (arg, result) ->
      TypeArrow (map f arg, map f result)
    | TypeRecord row ->
      TypeRecord (map_row f row)
    | TypeRowEmpty ->
      TypeRowEmpty
  in
  f mapped

and map_row f row = {
  row_fields = List.map (fun (name, field) ->
    (name, match field with
      | RowFieldPresent ty -> RowFieldPresent (map f ty))
  ) row.row_fields;
  row_more = map f row.row_more;
}

let rec fold f acc ty =
  let ty = representative ty in
  let acc = f acc ty in
  match ty with
  | TypeVariable _ -> acc
  | TypeConstructor (_, args) ->
    List.fold_left (fold f) acc args
  | TypeTuple elements ->
    List.fold_left (fold f) acc elements
  | TypeArrow (arg, result) ->
    let acc = fold f acc arg in
    fold f acc result
  | TypeRecord row -> fold_row f acc row
  | TypeRowEmpty -> acc

and fold_row f acc row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> fold f acc ty
  ) acc row.row_fields in
  fold f acc row.row_more

(** {1 Context-Aware Traversal} *)

let rec fold_with_context f ctx acc ty =
  let ty = representative ty in
  let ctx, acc = f ctx ty acc in
  match ty with
  | TypeVariable _ -> acc
  | TypeConstructor (_, args) ->
    List.fold_left (fun acc arg -> fold_with_context f ctx acc arg) acc args
  | TypeTuple elements ->
    List.fold_left (fun acc elem -> fold_with_context f ctx acc elem) acc elements
  | TypeArrow (arg, result) ->
    let acc = fold_with_context f ctx acc arg in
    fold_with_context f ctx acc result
  | TypeRecord row -> fold_row_with_context f ctx acc row
  | TypeRowEmpty -> acc

and fold_row_with_context f ctx acc row =
  let acc = List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> fold_with_context f ctx acc ty
  ) acc row.row_fields in
  fold_with_context f ctx acc row.row_more

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
