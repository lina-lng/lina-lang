(** Row type utilities.

    See {!Row_utils} for documentation. *)

open Types

(* Record row operations *)

let map_fields f row =
  let changed = ref false in
  let fields' = List.map (fun (name, field) ->
    match field with
    | RowFieldPresent ty ->
      let ty' = f ty in
      if ty != ty' then changed := true;
      (name, RowFieldPresent ty')
  ) row.row_fields in
  let more' = f row.row_more in
  if more' != row.row_more then changed := true;
  if !changed then { row_fields = fields'; row_more = more' }
  else row

let fold_fields f acc row =
  List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> f acc ty
  ) acc row.row_fields

let fold_all f acc row =
  let acc = fold_fields f acc row in
  f acc row.row_more

let iter_fields f row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> f ty
  ) row.row_fields

let iter_all f row =
  iter_fields f row;
  f row.row_more

let exists_in_fields pred row =
  List.exists (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> pred ty
  ) row.row_fields

let exists_in_all pred row =
  exists_in_fields pred row || pred row.row_more

let is_closed row =
  match representative row.row_more with
  | TypeRowEmpty -> true
  | _ -> false

let is_open row = not (is_closed row)

let close row =
  { row with row_more = TypeRowEmpty }

let extend ~name ~ty row =
  { row with row_fields = (name, RowFieldPresent ty) :: row.row_fields }

let get_field name row =
  match List.assoc_opt name row.row_fields with
  | Some (RowFieldPresent ty) -> Some ty
  | None -> None

let field_names row =
  List.map fst row.row_fields

(* Poly-variant row operations *)

let pv_map_fields f pv_row =
  let changed = ref false in
  let fields' = List.map (fun (name, field) ->
    let field' = match field with
      | PVFieldPresent (Some ty) ->
        let ty' = f ty in
        if ty != ty' then changed := true;
        PVFieldPresent (Some ty')
      | PVFieldPresent None -> PVFieldPresent None
      | PVFieldAbsent -> PVFieldAbsent
    in
    (name, field')
  ) pv_row.pv_fields in
  if !changed then { pv_row with pv_fields = fields' }
  else pv_row

let pv_fold_fields f acc pv_row =
  List.fold_left (fun acc (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> f acc ty
    | PVFieldPresent None | PVFieldAbsent -> acc
  ) acc pv_row.pv_fields

let pv_exists_in_fields pred pv_row =
  List.exists (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> pred ty
    | PVFieldPresent None | PVFieldAbsent -> false
  ) pv_row.pv_fields
