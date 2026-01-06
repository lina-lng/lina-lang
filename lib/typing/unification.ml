open Common
open Types

exception Unification_error of {
  expected : type_expression;
  actual : type_expression;
  location : Location.t;
  message : string;
}

let unification_error location expected actual message =
  raise (Unification_error { expected; actual; location; message })

let rec occurs_check location tv ty =
  match representative ty with
  | TypeVariable tv' ->
    if tv.id = tv'.id then
      unification_error location
        (TypeVariable tv) ty
        "Infinite type: type variable occurs in its own definition"
    else
      if tv'.level > tv.level then tv'.level <- tv.level
  | TypeConstructor (_, args) ->
    List.iter (occurs_check location tv) args
  | TypeTuple elements ->
    List.iter (occurs_check location tv) elements
  | TypeArrow (arg, result) ->
    occurs_check location tv arg;
    occurs_check location tv result
  | TypeRecord row ->
    occurs_check_row location tv row
  | TypeRowEmpty ->
    ()

and occurs_check_row location tv row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> occurs_check location tv ty
  ) row.row_fields;
  occurs_check location tv row.row_more

let rec unify location ty1 ty2 =
  let ty1 = representative ty1 in
  let ty2 = representative ty2 in
  if ty1 == ty2 then ()
  else match ty1, ty2 with
  | TypeVariable tv1, TypeVariable tv2 ->
    if tv1.level < tv2.level then
      tv2.link <- Some ty1
    else
      tv1.link <- Some ty2

  | TypeVariable tv, ty | ty, TypeVariable tv ->
    occurs_check location tv ty;
    tv.link <- Some ty

  | TypeConstructor (path1, args1), TypeConstructor (path2, args2) ->
    if path1 <> path2 then
      unification_error location ty1 ty2
        (Printf.sprintf "Type mismatch: expected %s, got %s"
          (type_expression_to_string ty1)
          (type_expression_to_string ty2));
    if List.length args1 <> List.length args2 then
      unification_error location ty1 ty2
        "Type constructor arity mismatch";
    List.iter2 (unify location) args1 args2

  | TypeTuple elems1, TypeTuple elems2 ->
    if List.length elems1 <> List.length elems2 then
      unification_error location ty1 ty2
        (Printf.sprintf "Tuple size mismatch: expected %d elements, got %d"
          (List.length elems1) (List.length elems2));
    List.iter2 (unify location) elems1 elems2

  | TypeArrow (arg1, res1), TypeArrow (arg2, res2) ->
    unify location arg1 arg2;
    unify location res1 res2

  | TypeRecord row1, TypeRecord row2 ->
    unify_rows location row1 row2

  | TypeRowEmpty, TypeRowEmpty ->
    ()

  | _ ->
    unification_error location ty1 ty2
      (Printf.sprintf "Type mismatch: expected %s, got %s"
        (type_expression_to_string ty1)
        (type_expression_to_string ty2))

(* Row unification using Leijen's row rewriting algorithm *)
and unify_rows location row1 row2 =
  (* Collect all labels from both rows *)
  let labels1 = List.map fst row1.row_fields in
  let labels2 = List.map fst row2.row_fields in
  let all_labels = List.sort_uniq compare (labels1 @ labels2) in

  (* For each label, find field in both rows and unify *)
  List.iter (fun label ->
    let field1 = List.assoc_opt label row1.row_fields in
    let field2 = List.assoc_opt label row2.row_fields in
    match field1, field2 with
    | Some (RowFieldPresent ty1), Some (RowFieldPresent ty2) ->
      unify location ty1 ty2
    | Some (RowFieldPresent ty), None ->
      (* Label in row1 but not row2 - unify row2.row_more to include it *)
      unify_row_with_field location row2.row_more label ty
    | None, Some (RowFieldPresent ty) ->
      (* Label in row2 but not row1 - unify row1.row_more to include it *)
      unify_row_with_field location row1.row_more label ty
    | None, None ->
      ()  (* Should not happen if we built all_labels correctly *)
  ) all_labels;

  (* Unify the remaining row tails *)
  let remaining1 = List.filter (fun (l, _) -> not (List.mem l labels2)) row1.row_fields in
  let remaining2 = List.filter (fun (l, _) -> not (List.mem l labels1)) row2.row_fields in

  match remaining1, remaining2 with
  | [], [] ->
    (* All fields matched, unify tails *)
    unify location row1.row_more row2.row_more
  | _, [] when remaining1 <> [] ->
    (* row1 has extra fields, unify row2.row_more with a record containing them *)
    unify location row2.row_more (TypeRecord { row_fields = remaining1; row_more = row1.row_more })
  | [], _ when remaining2 <> [] ->
    (* row2 has extra fields, unify row1.row_more with a record containing them *)
    unify location row1.row_more (TypeRecord { row_fields = remaining2; row_more = row2.row_more })
  | _, _ ->
    (* Both have extra fields - create fresh row variable and unify both tails *)
    let fresh_more = new_type_variable () in
    unify location row1.row_more (TypeRecord { row_fields = remaining2; row_more = fresh_more });
    unify location row2.row_more (TypeRecord { row_fields = remaining1; row_more = fresh_more })

(* Unify a row tail with a field that should be present *)
and unify_row_with_field location row_more label field_ty =
  match representative row_more with
  | TypeRowEmpty ->
    unification_error location TypeRowEmpty
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      (Printf.sprintf "Missing field '%s' in closed record" label)
  | TypeVariable tv ->
    (* Instantiate the row variable to include this field *)
    let fresh_more = new_type_variable () in
    let new_row = TypeRecord {
      row_fields = [(label, RowFieldPresent field_ty)];
      row_more = fresh_more;
    } in
    occurs_check location tv new_row;
    tv.link <- Some new_row
  | TypeRecord inner_row ->
    (* Check if field is in inner row *)
    begin match List.assoc_opt label inner_row.row_fields with
    | Some (RowFieldPresent inner_ty) ->
      unify location field_ty inner_ty
    | None ->
      unify_row_with_field location inner_row.row_more label field_ty
    end
  | _ ->
    unification_error location row_more
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      "Expected row type"
