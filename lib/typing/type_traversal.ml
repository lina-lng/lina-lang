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
