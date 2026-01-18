open Common
open Types

(** String set for efficient label membership testing *)
module StringSet = Set.Make(String)

(** Partition two lists of labeled fields into merged labels and remaining fields.
    Returns (all_labels, fields_only_in_first, fields_only_in_second). *)
let partition_row_fields fields1 fields2 =
  let labels1 = List.map fst fields1 in
  let labels2 = List.map fst fields2 in
  let all_labels = List.sort_uniq String.compare (labels1 @ labels2) in
  let set1 = StringSet.of_list labels1 in
  let set2 = StringSet.of_list labels2 in
  let remaining1 = List.filter (fun (label, _) -> not (StringSet.mem label set2)) fields1 in
  let remaining2 = List.filter (fun (label, _) -> not (StringSet.mem label set1)) fields2 in
  (all_labels, remaining1, remaining2)

(** Unification trace element - shows the path of comparisons that led to an error. *)
type trace_element =
  | TraceDiff of { expected : type_expression; actual : type_expression }
  | TraceRecordField of string
  | TraceTupleElement of int
  | TraceFunctionArg
  | TraceFunctionResult
  | TraceConstructorArg of int
  | TracePolyVariantTag of string

let trace_element_to_string = function
  | TraceDiff { expected; actual } ->
    Printf.sprintf "comparing %s with %s"
      (type_expression_to_string expected)
      (type_expression_to_string actual)
  | TraceRecordField name -> Printf.sprintf "in field `%s`" name
  | TraceTupleElement idx -> Printf.sprintf "in element %d of the tuple" (idx + 1)
  | TraceFunctionArg -> "in the function's argument type"
  | TraceFunctionResult -> "in the function's return type"
  | TraceConstructorArg idx -> Printf.sprintf "in type argument %d" (idx + 1)
  | TracePolyVariantTag tag -> Printf.sprintf "in variant tag `%s`" tag

exception Unification_error of {
  expected : type_expression;
  actual : type_expression;
  location : Location.t;
  message : string;
  trace : trace_element list;
}

(** Current unification trace - thread-local storage for trace elements. *)
let current_trace : trace_element list ref = ref []

let unification_error location expected actual message =
  let trace = !current_trace in
  current_trace := [];
  raise (Unification_error { expected; actual; location; message; trace })

let with_trace_element elem f =
  current_trace := elem :: !current_trace;
  try
    let result = f () in
    current_trace := List.tl !current_trace;
    result
  with e ->
    (* Don't clear trace on error - it's captured in unification_error *)
    raise e

(** Format unification trace for error messages.
    Provides a breadcrumb trail showing where in the nested types the error occurred. *)
let format_trace trace =
  if trace = [] then ""
  else
    let path_elements = List.filter (function TraceDiff _ -> false | _ -> true) trace in
    if path_elements = [] then ""
    else
      let reversed = List.rev path_elements in

      let ordinal n =
        match n with
        | 1 -> "1st"
        | 2 -> "2nd"
        | 3 -> "3rd"
        | n -> Printf.sprintf "%dth" n
      in

      let is_tuple_element = function TraceTupleElement _ -> true | _ -> false in
      let all_tuple_elements = List.for_all is_tuple_element reversed in
      let tuple_count = List.length (List.filter is_tuple_element reversed) in

      if all_tuple_elements && tuple_count > 1 then
        ""
      else
        let format_single_element = function
          | TraceTupleElement idx ->
            Printf.sprintf "the %s element" (ordinal (idx + 1))
          | TraceRecordField name ->
            Printf.sprintf "field `%s`" name
          | TraceFunctionArg ->
            "the function argument"
          | TraceFunctionResult ->
            "the return type"
          | TraceConstructorArg idx ->
            Printf.sprintf "type argument %d" (idx + 1)
          | TracePolyVariantTag tag ->
            Printf.sprintf "variant `%s`" tag
          | TraceDiff _ -> ""
        in

        let parts = List.filter_map (fun elem ->
          let s = format_single_element elem in
          if s = "" then None else Some s
        ) reversed in

        match parts with
        | [] -> ""
        | [single] -> Printf.sprintf "\n\nThe problem is in %s." single
        | _ ->
          let path_desc = String.concat ", then " parts in
          Printf.sprintf "\n\nThe problem is in %s." path_desc

(** Type expansion: expand type aliases to their definitions *)

(** Type lookup function type *)
type type_lookup = path -> type_declaration option

(** Set of paths, used for cycle detection during type alias expansion *)
module PathSet = Set.Make(struct
  type t = path
  let compare = compare
end)

exception Cyclic_type_alias of path

(** Map a function over items and collect results, tracking whether any changed.
    Returns (mapped_items, any_expanded). *)
let map_expand_collect expand_fn items =
  let results = List.map expand_fn items in
  let expanded_items = List.map fst results in
  let any_expanded = List.exists snd results in
  (expanded_items, any_expanded)

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
      let expanded_args, any_expanded =
        map_expand_collect (expand_type_aux ~type_lookup visiting) args in
      if any_expanded then (TypeConstructor (path, expanded_args), true)
      else (ty, false)
    end
  | TypeArrow (_, arg, result) ->
    let (arg', arg_exp) = expand_type_aux ~type_lookup visiting arg in
    let (result', result_exp) = expand_type_aux ~type_lookup visiting result in
    if arg_exp || result_exp then
      (TypeArrow (Nolabel, arg', result'), true)
    else
      (ty, false)
  | TypeTuple elements ->
    let expanded_elems, any_expanded =
      map_expand_collect (expand_type_aux ~type_lookup visiting) elements in
    if any_expanded then (TypeTuple expanded_elems, true)
    else (ty, false)
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
      let var_name = type_expression_to_string (TypeVariable tv) in
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

  | TypeArrow (_, arg, result) ->
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

  | TypePackage pkg ->
    let new_path = TypePackage pkg :: path in
    List.iter (fun (_, ty) ->
      occurs_check_with_path location tv ty new_path
    ) pkg.package_signature

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

(** Format a type mismatch error message using human-readable descriptions. *)
let type_mismatch_message ty1 ty2 =
  let expected_explain, actual_explain, hint = Type_explain.explain_mismatch ~expected:ty1 ~actual:ty2 in
  let base_message = Printf.sprintf "We expected %s but found %s" expected_explain actual_explain in

  match hint with
  | Some hint_text -> Printf.sprintf "%s\n\n%s" base_message hint_text
  | None -> base_message

(** {2 Type Variable Unification Helpers} *)

(** Unify two type variables, handling rigidity and levels.

    Rigid variables (from locally abstract types) have special rules:
    - Two rigid variables unify only if they are the same variable
    - Rigid variables can absorb non-rigid variables but not vice versa

    @param location Source location for errors
    @param ty1 First type (for error messages)
    @param ty2 Second type (for error messages)
    @param tv1 First type variable
    @param tv2 Second type variable *)
let unify_type_variables location ty1 ty2 tv1 tv2 =
  if tv1.rigid && tv2.rigid then begin
    (* Both rigid - must be the same variable *)
    if tv1.id <> tv2.id then
      unification_error location ty1 ty2
        "These are different locally abstract types that cannot be unified.\n\n\
         When you write `(type a)` and `(type b)` in a function signature, \
         they represent distinct abstract types."
  end else if tv1.rigid then begin
    (* tv1 is rigid - link tv2 to tv1 *)
    if not tv2.rigid then tv2.link <- Some ty1
  end else if tv2.rigid then begin
    (* tv2 is rigid - link tv1 to tv2 *)
    tv1.link <- Some ty2
  end else begin
    (* Neither is rigid - link higher level to lower *)
    if tv1.level < tv2.level then
      tv2.link <- Some ty1
    else
      tv1.link <- Some ty2
  end

(** Unify a type variable with a concrete (non-variable) type.

    Rigid variables cannot unify with concrete types.
    Non-rigid variables are linked after occurs check.

    @param location Source location for errors
    @param tv The type variable
    @param ty The concrete type *)
let unify_variable_with_concrete location tv ty =
  if tv.rigid then begin
    (* Rigid variable vs concrete type - error.
       This ensures (type a) in function parameters stays abstract. *)
    match ty with
    | TypeVariable tv2 when not tv2.rigid ->
      (* Non-rigid variable can be linked to rigid variable *)
      tv2.link <- Some (TypeVariable tv)
    | TypeVariable tv2 when tv2.rigid && tv.id = tv2.id ->
      (* Same rigid variable - OK *)
      ()
    | TypeVariable tv2 when tv2.rigid ->
      (* Different rigid variables - error *)
      unification_error location (TypeVariable tv) ty
        "These are different locally abstract types that cannot be unified.\n\n\
         When you write `(type a)` and `(type b)` in a function signature, \
         they represent distinct abstract types."
    | _ ->
      unification_error location (TypeVariable tv) ty
        (Printf.sprintf "The locally abstract type `%s` cannot be unified with the concrete type %s.\n\n\
                         Locally abstract types remain abstract within their scope."
          (type_expression_to_string (TypeVariable tv))
          (Type_explain.explain ty))
  end else begin
    occurs_check location tv ty;
    tv.link <- Some ty
  end

(** {2 Main Unification} *)

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

  (* Helper to expand a type constructor and retry unification. *)
  let try_expand_and_unify ty_ctor other_ty unify_order =
    try
      let (expanded, did_expand) = expand_type_aux ~type_lookup PathSet.empty ty_ctor in
      if did_expand then begin
        unify_order expanded other_ty;
        true
      end else
        false
    with Cyclic_type_alias path ->
      unification_error location ty1 ty2
        (Printf.sprintf "The type alias `%s` is cyclic.\n\n\
                         A type alias cannot refer to itself, either directly or indirectly. \
                         Consider using a recursive type with a data constructor instead."
           (path_to_string path))
  in

  if ty1 == ty2 then ()
  else match ty1, ty2 with
  | TypeVariable tv1, TypeVariable tv2 ->
    unify_type_variables location ty1 ty2 tv1 tv2

  | TypeVariable tv, ty | ty, TypeVariable tv ->
    unify_variable_with_concrete location tv ty

  | TypeConstructor (path1, args1), TypeConstructor (path2, args2) ->
    if path_equal path1 path2 then begin
      (* Same type constructor - unify arguments with trace *)
      if List.length args1 <> List.length args2 then
        unification_error location ty1 ty2
          (Printf.sprintf "The type constructor `%s` has %d type argument(s) in one place \
                           but %d in another."
             (path_to_string path1) (List.length args1) (List.length args2));
      List.iteri (fun idx (arg1, arg2) ->
        with_trace_element (TraceConstructorArg idx) (fun () ->
          unify_full ~type_lookup ~fresh_type_var location arg1 arg2
        )
      ) (List.combine args1 args2)
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
          (Printf.sprintf "The type alias `%s` is cyclic.\n\n\
                           A type alias cannot refer to itself, either directly or indirectly. \
                           Consider using a recursive type with a data constructor instead."
             (path_to_string path))
    end

  | TypeTuple elems1, TypeTuple elems2 ->
    if List.length elems1 <> List.length elems2 then
      unification_error location ty1 ty2
        (Printf.sprintf "We expected a tuple of %d elements but found one with %d elements"
          (List.length elems1) (List.length elems2));
    List.iteri (fun idx (e1, e2) ->
      with_trace_element (TraceTupleElement idx) (fun () ->
        unify_full ~type_lookup ~fresh_type_var location e1 e2
      )
    ) (List.combine elems1 elems2)

  | TypeArrow (_, arg1, res1), TypeArrow (_, arg2, res2) ->
    with_trace_element TraceFunctionArg (fun () ->
      unify_full ~type_lookup ~fresh_type_var location arg1 arg2
    );
    with_trace_element TraceFunctionResult (fun () ->
      unify_full ~type_lookup ~fresh_type_var location res1 res2
    )

  | TypeRecord row1, TypeRecord row2 ->
    unify_rows_full ~type_lookup ~fresh_type_var location row1 row2

  | TypePolyVariant pv_row1, TypePolyVariant pv_row2 ->
    unify_poly_variant_rows_full ~type_lookup ~fresh_type_var location pv_row1 pv_row2

  | TypeRowEmpty, TypeRowEmpty ->
    ()

  | TypePackage pkg1, TypePackage pkg2 ->
    (* Package types unify if they have the same path.
       For now we use simple path equality - both must refer to the same module type. *)
    if not (path_equal pkg1.package_path pkg2.package_path) then
      unification_error location ty1 ty2
        (Printf.sprintf "These first-class modules have different module types.\n\n\
                         One is `(module %s)` but the other is `(module %s)`."
          (path_to_string pkg1.package_path)
          (path_to_string pkg2.package_path))

  (* When one side is a TypeConstructor, try expanding it as a type alias *)
  | TypeConstructor _, _ ->
    let unify_expanded exp other = unify_full ~type_lookup ~fresh_type_var location exp other in
    if not (try_expand_and_unify ty1 ty2 unify_expanded) then
      unification_error location ty1 ty2 (type_mismatch_message ty1 ty2)

  | _, TypeConstructor _ ->
    let unify_with_expanded exp other = unify_full ~type_lookup ~fresh_type_var location other exp in
    if not (try_expand_and_unify ty2 ty1 unify_with_expanded) then
      unification_error location ty1 ty2 (type_mismatch_message ty1 ty2)

  | _ ->
    unification_error location ty1 ty2 (type_mismatch_message ty1 ty2)

(** Unify remaining row fields after matching common labels/tags.
    The make_row_type callback constructs the appropriate row type (record or poly variant). *)
and unify_remaining_row_fields ~type_lookup ~fresh_type_var location
    ~make_row_type ~more1 ~more2 remaining1 remaining2 =
  match remaining1, remaining2 with
  | [], [] ->
    unify_full ~type_lookup ~fresh_type_var location more1 more2
  | _, [] when remaining1 <> [] ->
    unify_full ~type_lookup ~fresh_type_var location more2 (make_row_type remaining1 more1)
  | [], _ when remaining2 <> [] ->
    unify_full ~type_lookup ~fresh_type_var location more1 (make_row_type remaining2 more2)
  | _, _ ->
    let fresh_more = fresh_type_var () in
    unify_full ~type_lookup ~fresh_type_var location more1 (make_row_type remaining2 fresh_more);
    unify_full ~type_lookup ~fresh_type_var location more2 (make_row_type remaining1 fresh_more)

and unify_rows_full ~type_lookup ~fresh_type_var location row1 row2 =
  let all_labels, remaining1, remaining2 =
    partition_row_fields row1.row_fields row2.row_fields in

  List.iter (fun label ->
    let field1 = List.assoc_opt label row1.row_fields in
    let field2 = List.assoc_opt label row2.row_fields in
    match field1, field2 with
    | Some (RowFieldPresent ty1), Some (RowFieldPresent ty2) ->
      begin try
        unify_full ~type_lookup ~fresh_type_var location ty1 ty2
      with Unification_error err ->
        (* Only enhance the error if this field's location is tracked.
           If not found, the error likely came from a nested record that
           already has the correct field context. *)
        match Field_locations.find label with
        | Some field_loc ->
          let message =
            Printf.sprintf "The record field `%s` has type `%s`, but it should have type `%s`."
              label
              (Types.type_expression_to_string err.actual)
              (Types.type_expression_to_string err.expected)
          in
          raise (Unification_error { err with message; location = field_loc })
        | None ->
          (* Re-raise the original error - it already has proper context *)
          raise (Unification_error err)
      end
    | Some (RowFieldPresent ty), None ->
      unify_row_with_field_full ~type_lookup ~fresh_type_var location row2.row_more label ty
    | None, Some (RowFieldPresent ty) ->
      unify_row_with_field_full ~type_lookup ~fresh_type_var location row1.row_more label ty
    | None, None -> ()
  ) all_labels;

  let make_record fields more = TypeRecord { row_fields = fields; row_more = more } in
  unify_remaining_row_fields ~type_lookup ~fresh_type_var location
    ~make_row_type:make_record ~more1:row1.row_more ~more2:row2.row_more
    remaining1 remaining2

(* Unify a row tail with a field that should be present *)
and unify_row_with_field_full ~type_lookup ~fresh_type_var location row_more label field_ty =
  match representative row_more with
  | TypeRowEmpty ->
    unification_error location TypeRowEmpty
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      (Printf.sprintf "This record doesn't have a field named `%s`. \
                       The record type is closed and only has the fields that are defined." label)
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
    begin match List.assoc_opt label inner_row.row_fields with
    | Some (RowFieldPresent inner_ty) ->
      begin try
        unify_full ~type_lookup ~fresh_type_var location field_ty inner_ty
      with Unification_error err ->
        (* Only enhance the error if this field's location is tracked.
           If not found, the error likely came from a nested record that
           already has the correct field context. *)
        match Field_locations.find label with
        | Some field_loc ->
          let message =
            Printf.sprintf "The record field `%s` has type `%s`, but it should have type `%s`."
              label
              (Types.type_expression_to_string err.actual)
              (Types.type_expression_to_string err.expected)
          in
          raise (Unification_error { err with message; location = field_loc })
        | None ->
          (* Re-raise the original error - it already has proper context *)
          raise (Unification_error err)
      end
    | None ->
      unify_row_with_field_full ~type_lookup ~fresh_type_var location inner_row.row_more label field_ty
    end
  | _ ->
    unification_error location row_more
      (TypeRecord { row_fields = [(label, RowFieldPresent field_ty)]; row_more = TypeRowEmpty })
      "Expected row type"

and unify_pv_tag_args ~type_lookup ~fresh_type_var location pv_row1 pv_row2 tag arg1 arg2 =
  match arg1, arg2 with
  | Some ty1, Some ty2 ->
    unify_full ~type_lookup ~fresh_type_var location ty1 ty2
  | None, None -> ()
  | Some _, None | None, Some _ ->
    unification_error location
      (TypePolyVariant pv_row1) (TypePolyVariant pv_row2)
      (Printf.sprintf "The polymorphic variant tag `%s` has different arities.\n\n\
                       In one place it takes an argument, in another it doesn't." tag)

and unify_poly_variant_rows_full ~type_lookup ~fresh_type_var location pv_row1 pv_row2 =
  let all_tags, remaining1, remaining2 =
    partition_row_fields pv_row1.pv_fields pv_row2.pv_fields in

  List.iter (fun tag ->
    let field1 = List.assoc_opt tag pv_row1.pv_fields in
    let field2 = List.assoc_opt tag pv_row2.pv_fields in
    match field1, field2 with
    | Some (PVFieldPresent arg1), Some (PVFieldPresent arg2) ->
      unify_pv_tag_args ~type_lookup ~fresh_type_var location pv_row1 pv_row2 tag arg1 arg2
    | Some PVFieldAbsent, Some PVFieldAbsent -> ()
    | Some (PVFieldPresent _), Some PVFieldAbsent
    | Some PVFieldAbsent, Some (PVFieldPresent _) ->
      unification_error location
        (TypePolyVariant pv_row1) (TypePolyVariant pv_row2)
        (Printf.sprintf "The polymorphic variant tag `%s` is required in one type but \
                         explicitly absent in another.\n\n\
                         These two polymorphic variant types are incompatible." tag)
    | Some (PVFieldPresent arg_ty), None ->
      unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location
        pv_row2.pv_more tag (PVFieldPresent arg_ty)
    | None, Some (PVFieldPresent arg_ty) ->
      unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location
        pv_row1.pv_more tag (PVFieldPresent arg_ty)
    | Some PVFieldAbsent, None | None, Some PVFieldAbsent -> ()
    | None, None -> ()
  ) all_tags;

  let make_pv_with_closed closed fields more =
    TypePolyVariant { pv_fields = fields; pv_more = more; pv_closed = closed }
  in
  let make_pv1 = make_pv_with_closed pv_row1.pv_closed in
  let make_pv2 = make_pv_with_closed pv_row2.pv_closed in

  match remaining1, remaining2 with
  | [], [] ->
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more pv_row2.pv_more
  | _, [] when remaining1 <> [] ->
    unify_full ~type_lookup ~fresh_type_var location pv_row2.pv_more (make_pv1 remaining1 pv_row1.pv_more)
  | [], _ when remaining2 <> [] ->
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more (make_pv2 remaining2 pv_row2.pv_more)
  | _, _ ->
    let fresh_more = fresh_type_var () in
    let make_open = make_pv_with_closed false in
    unify_full ~type_lookup ~fresh_type_var location pv_row1.pv_more (make_open remaining2 fresh_more);
    unify_full ~type_lookup ~fresh_type_var location pv_row2.pv_more (make_open remaining1 fresh_more)

(* Unify a poly variant row tail with a tag that should be present *)
and unify_pv_row_with_tag_full ~type_lookup ~fresh_type_var location row_more tag field =
  match representative row_more with
  | TypeRowEmpty ->
    (* Closed row - tag must be absent *)
    unification_error location TypeRowEmpty
      (TypePolyVariant { pv_fields = [(tag, field)]; pv_more = TypeRowEmpty; pv_closed = true })
      (Printf.sprintf "The tag `%s` is not allowed in this closed polymorphic variant type.\n\n\
                       The type only permits specific tags and `%s` is not one of them." tag tag)
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
            (Printf.sprintf "The polymorphic variant tag `%s` has inconsistent arities.\n\n\
                             One version takes an argument while the other doesn't." tag)
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
