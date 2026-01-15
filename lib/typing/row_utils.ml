(** Row type utilities.

    This module provides traversal and utility functions for both record rows
    and poly-variant rows. The operations parallel each other but operate on
    different field types:

    - Record rows: [row] with [RowFieldPresent ty] fields
    - Poly-variant rows: [poly_variant_row] with [PVFieldPresent (Some ty)]
      [PVFieldPresent None], or [PVFieldAbsent] fields

    {2 Record Row Operations}

    All operations on record [row] work with field types. *)

open Types

(** {1 Record Row Traversal} *)

(** [map_fields f row] transforms field types in a row.
    Uses physical equality to detect changes for efficiency. *)
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

(** [fold_fields f acc row] folds over field types only (not row_more). *)
let fold_fields f acc row =
  List.fold_left (fun acc (_, field) ->
    match field with
    | RowFieldPresent ty -> f acc ty
  ) acc row.row_fields

(** [fold_all f acc row] folds over field types and row_more. *)
let fold_all f acc row =
  let acc = fold_fields f acc row in
  f acc row.row_more

(** [iter_fields f row] applies f to each field type (not row_more). *)
let iter_fields f row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> f ty
  ) row.row_fields

(** [iter_all f row] applies f to field types and row_more. *)
let iter_all f row =
  iter_fields f row;
  f row.row_more

(** [exists_in_fields pred row] checks if any field type satisfies [pred]. *)
let exists_in_fields pred row =
  List.exists (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> pred ty
  ) row.row_fields

(** [exists_in_all pred row] checks fields and row_more. *)
let exists_in_all pred row =
  exists_in_fields pred row || pred row.row_more

(** {1 Record Row Utilities} *)

(** [is_closed row] returns true if row_more is [TypeRowEmpty].
    A closed row has a fixed set of fields with no extension variable. *)
let is_closed row =
  match representative row.row_more with
  | TypeRowEmpty -> true
  | _ -> false

(** [is_open row] returns true if the row can be extended (has a variable tail). *)
let is_open row = not (is_closed row)

(** [close row] returns a copy of the row with [row_more] set to [TypeRowEmpty]. *)
let close row =
  { row with row_more = TypeRowEmpty }

(** [extend ~name ~ty row] adds a new field to the front of the row. *)
let extend ~name ~ty row =
  { row with row_fields = (name, RowFieldPresent ty) :: row.row_fields }

(** [get_field name row] looks up a field by name, returning [Some ty] if present. *)
let get_field name row =
  match List.assoc_opt name row.row_fields with
  | Some (RowFieldPresent ty) -> Some ty
  | None -> None

(** [field_names row] returns the list of field names in the row. *)
let field_names row =
  List.map fst row.row_fields

(** {1 Poly-Variant Row Operations}

    These operations mirror the record row operations above but work on
    poly-variant rows. The key difference is that poly-variant fields can be:
    - [PVFieldPresent (Some ty)] - constructor with payload
    - [PVFieldPresent None] - constructor without payload
    - [PVFieldAbsent] - explicitly absent (for row subtraction) *)

(** [pv_map_fields f pv_row] transforms field types in a poly-variant row.
    Only applies [f] to [PVFieldPresent (Some ty)] fields; other fields pass through.
    Uses physical equality to detect changes for efficiency. *)
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

(** [pv_fold_fields f acc pv_row] folds over field types only (not pv_more).
    Skips [PVFieldPresent None] and [PVFieldAbsent] fields. *)
let pv_fold_fields f acc pv_row =
  List.fold_left (fun acc (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> f acc ty
    | PVFieldPresent None | PVFieldAbsent -> acc
  ) acc pv_row.pv_fields

(** [pv_exists_in_fields pred pv_row] checks if any field type satisfies [pred].
    Skips [PVFieldPresent None] and [PVFieldAbsent] fields. *)
let pv_exists_in_fields pred pv_row =
  List.exists (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> pred ty
    | PVFieldPresent None | PVFieldAbsent -> false
  ) pv_row.pv_fields
