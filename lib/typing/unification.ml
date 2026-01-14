open Common
open Types

(** String set for efficient label membership testing *)
module StringSet = Set.Make(String)

exception Unification_error of {
  expected : type_expression;
  actual : type_expression;
  location : Location.t;
  message : string;
}

let unification_error location expected actual message =
  raise (Unification_error { expected; actual; location; message })

(** Type expansion: expand type aliases to their definitions *)

(** Type lookup function type *)
type type_lookup = path -> type_declaration option

(** Set of paths, used for cycle detection during type alias expansion *)
module PathSet = Set.Make(struct
  type t = path
  let compare = compare  (* Use structural comparison for paths *)
end)

exception Cyclic_type_alias of path

(* Expand a type by following aliases. Returns (expanded_ty, did_expand).
   The 'visiting' parameter tracks paths being expanded to detect cycles. *)
let rec expand_type_aux ~type_lookup visiting ty =
  match representative ty with
  | TypeConstructor (path, args) ->
    begin match type_lookup path with
    | Some decl when Option.is_some decl.declaration_manifest ->
      (* Check for cycle: if we're already expanding this path, it's cyclic *)
      if PathSet.mem path visiting then
        raise (Cyclic_type_alias path)
      else begin
        let visiting' = PathSet.add path visiting in
        let manifest = Option.get decl.declaration_manifest in
        let expanded = Type_utils.substitute_type_params decl.declaration_parameters args manifest in
        let (final, _) = expand_type_aux ~type_lookup visiting' expanded in
        (final, true)
      end
    | _ ->
      (* Not an alias - recursively expand type arguments *)
      let results = List.map (expand_type_aux ~type_lookup visiting) args in
      let any_expanded = List.exists snd results in
      if any_expanded then
        (TypeConstructor (path, List.map fst results), true)
      else
        (ty, false)
    end
  | TypeArrow (arg, result) ->
    let (arg', arg_exp) = expand_type_aux ~type_lookup visiting arg in
    let (result', result_exp) = expand_type_aux ~type_lookup visiting result in
    if arg_exp || result_exp then
      (TypeArrow (arg', result'), true)
    else
      (ty, false)
  | TypeTuple elements ->
    let results = List.map (expand_type_aux ~type_lookup visiting) elements in
    let any_expanded = List.exists snd results in
    if any_expanded then
      (TypeTuple (List.map fst results), true)
    else
      (ty, false)
  | TypeRecord row ->
    let (row', expanded) = expand_row_aux ~type_lookup visiting row in
    if expanded then (TypeRecord row', true) else (ty, false)
  | ty -> (ty, false)

and expand_row_aux ~type_lookup visiting row =
  let results = List.map (fun (name, field) ->
    match field with
    | RowFieldPresent ty ->
      let (ty', exp) = expand_type_aux ~type_lookup visiting ty in
      ((name, RowFieldPresent ty'), exp)
  ) row.row_fields in
  let (row_more', more_exp) = expand_type_aux ~type_lookup visiting row.row_more in
  let any_expanded = List.exists snd results || more_exp in
  if any_expanded then
    ({ row_fields = List.map fst results; row_more = row_more' }, true)
  else
    (row, false)

(** Expand type by following type aliases.

    @param type_lookup Function to look up type declarations for alias expansion
    @param ty The type to expand *)
let expand_type ~type_lookup ty = fst (expand_type_aux ~type_lookup PathSet.empty ty)

(** Enhanced occurs check with path tracking for better error messages.

    When a cycle is detected, the path shows which type constructors
    lead to the cycle, making it easier to understand the error.

    @param location Source location for error messages
    @param tv The type variable we're checking for
    @param ty The type expression to search
    @param path Stack of type expressions leading to current position *)
let rec occurs_check_with_path location tv ty path =
  match representative ty with
  | TypeVariable tv' ->
    if tv.id = tv'.id then begin
      (* Build a readable path string showing the cycle *)
      let path_str = String.concat " contains " (List.rev_map (fun path_ty ->
        type_expression_to_string path_ty
      ) path) in
      let var_name = Printf.sprintf "'t%d" tv.id in
      let message =
        if path = [] then
          Printf.sprintf
            "Infinite type: %s occurs in its own definition"
            var_name
        else
          Printf.sprintf
            "Infinite type: %s occurs within %s\n\n\
             This would create an infinite type. Consider using an explicit \
             recursive type with a data constructor."
            var_name path_str
      in
      unification_error location (TypeVariable tv) ty message
    end else begin
      (* Level adjustment for generalization *)
      if tv'.level > tv.level then tv'.level <- tv.level
    end

  | TypeConstructor (_, args) ->
    let new_path = ty :: path in
    List.iter (fun arg -> occurs_check_with_path location tv arg new_path) args

  | TypeTuple elements ->
    let new_path = ty :: path in
    List.iter (fun elem -> occurs_check_with_path location tv elem new_path) elements

  | TypeArrow (arg, result) ->
    let new_path = ty :: path in
    occurs_check_with_path location tv arg new_path;
    occurs_check_with_path location tv result new_path

  | TypeRecord row ->
    let new_path = ty :: path in
    occurs_check_row_with_path location tv row new_path

  | TypePolyVariant pv_row ->
    let new_path = ty :: path in
    occurs_check_poly_variant_row_with_path location tv pv_row new_path

  | TypeRowEmpty ->
    ()

and occurs_check_row_with_path location tv row path =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> occurs_check_with_path location tv ty path
  ) row.row_fields;
  occurs_check_with_path location tv row.row_more path

and occurs_check_poly_variant_row_with_path location tv pv_row path =
  List.iter (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> occurs_check_with_path location tv ty path
    | PVFieldPresent None | PVFieldAbsent -> ()
  ) pv_row.pv_fields;
  occurs_check_with_path location tv pv_row.pv_more path

(** Original occurs check interface - delegates to enhanced version. *)
let occurs_check location tv ty =
  occurs_check_with_path location tv ty []

(** Row-specific occurs check for backward compatibility. *)
let _occurs_check_row location tv row =
  occurs_check_row_with_path location tv row []

(** Type variable factory function type *)
type fresh_type_var = unit -> type_expression

(** Default type variable factory for row unification.
    Uses generic_level since these are free variables during unification. *)
let default_fresh_type_var () =
  new_type_variable_at_level generic_level

(** Unify two types with explicit type lookup and type variable factory.
    This is the primary unification function - all unification should go through here.

    @param type_lookup Function to look up type declarations for alias expansion
    @param fresh_type_var Function to create fresh type variables (for row unification)
    @param location Source location for error messages
    @param ty1 First type to unify
    @param ty2 Second type to unify *)
let rec unify_full ~type_lookup ~fresh_type_var location ty1 ty2 =
  let ty1 = representative ty1 in
  let ty2 = representative ty2 in
  if ty1 == ty2 then ()
  else match ty1, ty2 with
  | TypeVariable tv1, TypeVariable tv2 ->
    (* Handle rigid type variables carefully.
       Rigid variables come from locally abstract types: (type a).
       They must not escape or unify with concrete types. *)
    if tv1.rigid && tv2.rigid then begin
      (* Both rigid - must be the same variable *)
      if tv1.id <> tv2.id then
        unification_error location ty1 ty2
          "Cannot unify different rigid type variables"
    end else if tv1.rigid then begin
      (* tv1 is rigid - don't link it, link tv2 instead if not rigid *)
      if not tv2.rigid then tv2.link <- Some ty1
    end else if tv2.rigid then begin
      (* tv2 is rigid - don't link it *)
      tv1.link <- Some ty2
    end else begin
      (* Neither is rigid - normal unification *)
      if tv1.level < tv2.level then
        tv2.link <- Some ty1
      else
        tv1.link <- Some ty2
    end

  | TypeVariable tv, ty | ty, TypeVariable tv ->
    if tv.rigid then
      (* Rigid variable: cannot unify with concrete types.
         This ensures (type a) in function parameters stays abstract.
         Only type variables can be unified with rigid variables. *)
      begin match ty with
      | TypeVariable tv2 when not tv2.rigid ->
        (* Non-rigid variable can be linked to rigid variable *)
        tv2.link <- Some (TypeVariable tv)
      | TypeVariable tv2 when tv2.rigid && tv.id = tv2.id ->
        (* Same rigid variable - OK *)
        ()
      | TypeVariable tv2 when tv2.rigid ->
        (* Different rigid variables - error *)
        unification_error location (TypeVariable tv) ty
          "Cannot unify different rigid type variables"
      | _ ->
        (* Rigid variable vs concrete type - error *)
        unification_error location (TypeVariable tv) ty
          (Printf.sprintf "The type %s is abstract and cannot be unified with %s"
            (type_expression_to_string (TypeVariable tv))
            (type_expression_to_string ty))
      end
    else begin
      occurs_check location tv ty;
      tv.link <- Some ty
    end

  | TypeConstructor (path1, args1), TypeConstructor (path2, args2) ->
    if path_equal path1 path2 then begin
      (* Same type constructor - unify arguments *)
      if List.length args1 <> List.length args2 then
        unification_error location ty1 ty2
          "Type constructor arity mismatch";
      List.iter2 (unify_full ~type_lookup ~fresh_type_var location) args1 args2
    end else begin
      (* Different constructors - try expanding aliases *)
      try
        let (expanded1, did_expand1) = expand_type_aux ~type_lookup PathSet.empty ty1 in
        let (expanded2, did_expand2) = expand_type_aux ~type_lookup PathSet.empty ty2 in
        (* Only retry if expansion changed something *)
        if did_expand1 || did_expand2 then
          unify_full ~type_lookup ~fresh_type_var location expanded1 expanded2
        else
          unification_error location ty1 ty2
            (Printf.sprintf "Type mismatch: expected %s, got %s"
              (type_expression_to_string ty1)
              (type_expression_to_string ty2))
      with Cyclic_type_alias path ->
        unification_error location ty1 ty2
          (Printf.sprintf "Cyclic type alias detected: %s" (path_to_string path))
    end

  | TypeTuple elems1, TypeTuple elems2 ->
    if List.length elems1 <> List.length elems2 then
      unification_error location ty1 ty2
        (Printf.sprintf "Tuple size mismatch: expected %d elements, got %d"
          (List.length elems1) (List.length elems2));
    List.iter2 (unify_full ~type_lookup ~fresh_type_var location) elems1 elems2

  | TypeArrow (arg1, res1), TypeArrow (arg2, res2) ->
    unify_full ~type_lookup ~fresh_type_var location arg1 arg2;
    unify_full ~type_lookup ~fresh_type_var location res1 res2

  | TypeRecord row1, TypeRecord row2 ->
    unify_rows_full ~type_lookup ~fresh_type_var location row1 row2

  | TypePolyVariant pv_row1, TypePolyVariant pv_row2 ->
    unify_poly_variant_rows_full ~type_lookup ~fresh_type_var location pv_row1 pv_row2

  | TypeRowEmpty, TypeRowEmpty ->
    ()

  | _ ->
    unification_error location ty1 ty2
      (Printf.sprintf "Type mismatch: expected %s, got %s"
        (type_expression_to_string ty1)
        (type_expression_to_string ty2))

(* Row unification using Leijen's row rewriting algorithm *)
and unify_rows_full ~type_lookup ~fresh_type_var location row1 row2 =
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
      unify_full ~type_lookup ~fresh_type_var location ty1 ty2
    | Some (RowFieldPresent ty), None ->
      (* Label in row1 but not row2 - unify row2.row_more to include it *)
      unify_row_with_field_full ~type_lookup ~fresh_type_var location row2.row_more label ty
    | None, Some (RowFieldPresent ty) ->
      (* Label in row2 but not row1 - unify row1.row_more to include it *)
      unify_row_with_field_full ~type_lookup ~fresh_type_var location row1.row_more label ty
    | None, None ->
      ()  (* Should not happen if we built all_labels correctly *)
  ) all_labels;

  (* Unify the remaining row tails using sets for O(n log n) instead of O(n²) *)
  let labels1_set = StringSet.of_list labels1 in
  let labels2_set = StringSet.of_list labels2 in
  let remaining1 = List.filter (fun (l, _) -> not (StringSet.mem l labels2_set)) row1.row_fields in
  let remaining2 = List.filter (fun (l, _) -> not (StringSet.mem l labels1_set)) row2.row_fields in

  match remaining1, remaining2 with
  | [], [] ->
    (* All fields matched, unify tails *)
    unify_full ~type_lookup ~fresh_type_var location row1.row_more row2.row_more
  | _, [] when remaining1 <> [] ->
    (* row1 has extra fields, unify row2.row_more with a record containing them *)
    unify_full ~type_lookup ~fresh_type_var location row2.row_more (TypeRecord { row_fields = remaining1; row_more = row1.row_more })
  | [], _ when remaining2 <> [] ->
    (* row2 has extra fields, unify row1.row_more with a record containing them *)
    unify_full ~type_lookup ~fresh_type_var location row1.row_more (TypeRecord { row_fields = remaining2; row_more = row2.row_more })
  | _, _ ->
    (* Both have extra fields - create fresh row variable and unify both tails *)
    let fresh_more = fresh_type_var () in
    unify_full ~type_lookup ~fresh_type_var location row1.row_more (TypeRecord { row_fields = remaining2; row_more = fresh_more });
    unify_full ~type_lookup ~fresh_type_var location row2.row_more (TypeRecord { row_fields = remaining1; row_more = fresh_more })

(* Unify a row tail with a field that should be present *)
and unify_row_with_field_full ~type_lookup ~fresh_type_var location row_more label field_ty =
  match representative row_more with
  | TypeRowEmpty ->
    unification_error location TypeRowEmpty
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      (Printf.sprintf "Missing field '%s' in closed record" label)
  | TypeVariable tv ->
    (* Instantiate the row variable to include this field *)
    let fresh_more = fresh_type_var () in
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
      unify_full ~type_lookup ~fresh_type_var location field_ty inner_ty
    | None ->
      unify_row_with_field_full ~type_lookup ~fresh_type_var location inner_row.row_more label field_ty
    end
  | _ ->
    unification_error location row_more
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      "Expected row type"

(* Polymorphic variant row unification.
   Similar to record row unification but handles presence/absence of tags. *)
and unify_poly_variant_rows_full ~type_lookup ~fresh_type_var location pv_row1 pv_row2 =
  (* Collect all tags from both rows *)
  let tags1 = List.map fst pv_row1.pv_fields in
  let tags2 = List.map fst pv_row2.pv_fields in
  let all_tags = List.sort_uniq compare (tags1 @ tags2) in

  (* For each tag, find field in both rows and unify *)
  List.iter (fun tag ->
    let field1 = List.assoc_opt tag pv_row1.pv_fields in
    let field2 = List.assoc_opt tag pv_row2.pv_fields in
    match field1, field2 with
    | Some (PVFieldPresent arg1), Some (PVFieldPresent arg2) ->
      (* Both present - unify argument types if any *)
      begin match arg1, arg2 with
      | Some ty1, Some ty2 ->
        unify_full ~type_lookup ~fresh_type_var location ty1 ty2
      | None, None -> ()
      | Some _, None | None, Some _ ->
        unification_error location
          (TypePolyVariant pv_row1) (TypePolyVariant pv_row2)
          (Printf.sprintf "Tag `%s arity mismatch" tag)
      end
    | Some PVFieldAbsent, Some PVFieldAbsent ->
      (* Both absent - OK *)
      ()
    | Some (PVFieldPresent _), Some PVFieldAbsent
    | Some PVFieldAbsent, Some (PVFieldPresent _) ->
      (* One present, one absent - error *)
      unification_error location
        (TypePolyVariant pv_row1) (TypePolyVariant pv_row2)
        (Printf.sprintf "Tag `%s presence conflict" tag)
    | Some (PVFieldPresent arg_ty), None ->
      (* Tag in row1 but not row2 - extend row2's more *)
      unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location
        pv_row2.pv_more tag (PVFieldPresent arg_ty)
    | None, Some (PVFieldPresent arg_ty) ->
      (* Tag in row2 but not row1 - extend row1's more *)
      unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location
        pv_row1.pv_more tag (PVFieldPresent arg_ty)
    | Some PVFieldAbsent, None | None, Some PVFieldAbsent ->
      (* Absent tags don't need to be propagated to the other row *)
      ()
    | None, None ->
      ()  (* Should not happen *)
  ) all_tags;

  (* Unify the row tails *)
  let tags1_set = StringSet.of_list tags1 in
  let tags2_set = StringSet.of_list tags2 in
  let remaining1 = List.filter (fun (t, _) -> not (StringSet.mem t tags2_set)) pv_row1.pv_fields in
  let remaining2 = List.filter (fun (t, _) -> not (StringSet.mem t tags1_set)) pv_row2.pv_fields in

  match remaining1, remaining2 with
  | [], [] ->
    (* All tags matched, unify tails *)
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more pv_row2.pv_more
  | _, [] when remaining1 <> [] ->
    (* pv_row1 has extra tags, unify pv_row2.pv_more with a poly variant containing them *)
    let new_pv = TypePolyVariant {
      pv_fields = remaining1;
      pv_more = pv_row1.pv_more;
      pv_closed = pv_row1.pv_closed;
    } in
    unify_full ~type_lookup ~fresh_type_var location pv_row2.pv_more new_pv
  | [], _ when remaining2 <> [] ->
    (* pv_row2 has extra tags, unify pv_row1.pv_more with a poly variant containing them *)
    let new_pv = TypePolyVariant {
      pv_fields = remaining2;
      pv_more = pv_row2.pv_more;
      pv_closed = pv_row2.pv_closed;
    } in
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more new_pv
  | _, _ ->
    (* Both have extra tags - create fresh row variable and unify both tails *)
    let fresh_more = fresh_type_var () in
    let new_pv1 = TypePolyVariant {
      pv_fields = remaining2;
      pv_more = fresh_more;
      pv_closed = false;
    } in
    let new_pv2 = TypePolyVariant {
      pv_fields = remaining1;
      pv_more = fresh_more;
      pv_closed = false;
    } in
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more new_pv1;
    unify_full ~type_lookup ~fresh_type_var location pv_row2.pv_more new_pv2

(* Unify a poly variant row tail with a tag that should be present *)
and unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location row_more tag field =
  match representative row_more with
  | TypeRowEmpty ->
    (* Closed row - tag must be absent *)
    unification_error location TypeRowEmpty
      (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
      (Printf.sprintf "Tag `%s not allowed in closed poly variant" tag)
  | TypeVariable tv ->
    (* Open row - instantiate the variable to include this tag *)
    let fresh_more = fresh_type_var () in
    let new_pv = TypePolyVariant {
      pv_fields = [(tag, field)];
      pv_more = fresh_more;
      pv_closed = false;
    } in
    occurs_check location tv new_pv;
    tv.link <- Some new_pv
  | TypePolyVariant inner_pv ->
    (* Check if tag is in inner poly variant row *)
    begin match List.assoc_opt tag inner_pv.pv_fields with
    | Some (PVFieldPresent inner_arg) ->
      begin match field with
      | PVFieldPresent arg ->
        begin match inner_arg, arg with
        | Some ty1, Some ty2 ->
          unify_full ~type_lookup ~fresh_type_var location ty1 ty2
        | None, None -> ()
        | _ ->
          unification_error location
            (TypePolyVariant inner_pv)
            (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
            (Printf.sprintf "Tag `%s arity mismatch" tag)
        end
      | PVFieldAbsent ->
        unification_error location
          (TypePolyVariant inner_pv)
          (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
          (Printf.sprintf "Tag `%s presence conflict" tag)
      end
    | Some PVFieldAbsent ->
      begin match field with
      | PVFieldPresent _ ->
        unification_error location
          (TypePolyVariant inner_pv)
          (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
          (Printf.sprintf "Tag `%s presence conflict" tag)
      | PVFieldAbsent -> ()
      end
    | None ->
      unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location inner_pv.pv_more tag field
    end
  | _ ->
    unification_error location row_more
      (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
      "Expected poly variant row type"

(** Unify two types.

    @param type_lookup Function to look up type declarations for alias expansion
    @param location Source location for error messages
    @param ty1 First type to unify
    @param ty2 Second type to unify *)
let unify ~type_lookup location ty1 ty2 =
  unify_full ~type_lookup ~fresh_type_var:default_fresh_type_var location ty1 ty2
