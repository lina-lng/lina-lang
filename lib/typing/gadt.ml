(** GADT (Generalized Algebraic Data Types) support.

    This module implements type equation extraction for GADT pattern matching.
    When matching a GADT constructor, type equations are extracted rather than
    doing global unification on rigid type variables.

    For example, matching `Int n` against scrutinee of type `a expr` where `a`
    is a rigid locally abstract type variable, extracts the equation `a = int`.
    This equation is then applied locally within the match branch. *)

open Types

(** A type equation extracted from GADT pattern matching.
    Maps a rigid type variable to its refined type within a branch. *)
type equation = {
  eq_variable : type_variable;  (** The rigid type variable *)
  eq_type : type_expression;    (** The type it equals in this branch *)
}

(** Result of GADT equation extraction. *)
type extraction_result = {
  equations : equation list;  (** Extracted type equations *)
  success : bool;             (** Whether the types could be matched *)
}

(** Check if a type expression contains any rigid type variables. *)
let rec has_rigid_variables ty =
  match representative ty with
  | TypeVariable tv -> tv.rigid
  | TypeArrow (arg, result) ->
    has_rigid_variables arg || has_rigid_variables result
  | TypeTuple types ->
    List.exists has_rigid_variables types
  | TypeConstructor (_, args) ->
    List.exists has_rigid_variables args
  | TypeRecord row ->
    List.exists (fun (_, field) ->
      match field with
      | RowFieldPresent ty -> has_rigid_variables ty
    ) row.row_fields ||
    has_rigid_variables row.row_more
  | TypePolyVariant pv_row ->
    List.exists (fun (_, field) ->
      match field with
      | PVFieldPresent (Some ty) -> has_rigid_variables ty
      | PVFieldPresent None | PVFieldAbsent -> false
    ) pv_row.pv_fields ||
    has_rigid_variables pv_row.pv_more
  | TypeRowEmpty -> false

(** Extract GADT type equations by comparing scrutinee type with constructor result type.

    This function walks both types in parallel, extracting equations when a rigid
    type variable in the scrutinee is matched against a concrete type in the
    constructor result.

    @param scrutinee_type The type of the value being matched
    @param constructor_result_type The instantiated result type of the GADT constructor
    @return Extraction result with equations and success flag *)
let extract_equations scrutinee_type constructor_result_type =
  let equations = ref [] in
  let success = ref true in

  let rec extract scrutinee_ty constructor_ty =
    let scrutinee_ty = representative scrutinee_ty in
    let constructor_ty = representative constructor_ty in
    match scrutinee_ty, constructor_ty with
    (* Rigid variable in scrutinee - extract equation *)
    | TypeVariable tv, _ when tv.rigid ->
      (* Check if we already have an equation for this variable *)
      let existing = List.find_opt (fun eq -> eq.eq_variable.id = tv.id) !equations in
      begin match existing with
      | Some _eq ->
        (* Already have equation - for simplicity, keep the first one.
           A more robust implementation would verify consistency. *)
        ()
      | None ->
        (* New equation *)
        equations := { eq_variable = tv; eq_type = constructor_ty } :: !equations
      end

    (* Non-rigid variable - unification will handle this normally *)
    | TypeVariable _, _ -> ()
    | _, TypeVariable _ -> ()

    (* Type constructors - recurse on arguments *)
    | TypeConstructor (path1, args1), TypeConstructor (path2, args2) ->
      if path_equal path1 path2 && List.length args1 = List.length args2 then
        List.iter2 extract args1 args2
      else
        success := false

    (* Arrows - recurse on both parts *)
    | TypeArrow (arg1, res1), TypeArrow (arg2, res2) ->
      extract arg1 arg2;
      extract res1 res2

    (* Tuples - recurse on elements *)
    | TypeTuple tys1, TypeTuple tys2 ->
      if List.length tys1 = List.length tys2 then
        List.iter2 extract tys1 tys2
      else
        success := false

    (* Records - recurse on fields *)
    | TypeRecord row1, TypeRecord row2 ->
      extract_row_fields row1.row_fields row2.row_fields;
      extract row1.row_more row2.row_more

    (* Poly variants - recurse on fields *)
    | TypePolyVariant pv1, TypePolyVariant pv2 ->
      extract_pv_fields pv1.pv_fields pv2.pv_fields;
      extract pv1.pv_more pv2.pv_more

    (* Empty rows match *)
    | TypeRowEmpty, TypeRowEmpty -> ()

    (* Mismatch *)
    | _ -> success := false

  and extract_row_fields fields1 fields2 =
    (* Simple comparison - assumes same field order *)
    List.iter2 (fun (name1, field1) (name2, field2) ->
      if name1 = name2 then
        match field1, field2 with
        | RowFieldPresent ty1, RowFieldPresent ty2 -> extract ty1 ty2
      else
        success := false
    ) fields1 fields2

  and extract_pv_fields fields1 fields2 =
    List.iter2 (fun (name1, field1) (name2, field2) ->
      if name1 = name2 then
        match field1, field2 with
        | PVFieldPresent (Some ty1), PVFieldPresent (Some ty2) -> extract ty1 ty2
        | PVFieldPresent None, PVFieldPresent None -> ()
        | PVFieldAbsent, PVFieldAbsent -> ()
        | _ -> success := false
      else
        success := false
    ) fields1 fields2
  in

  extract scrutinee_type constructor_result_type;
  { equations = !equations; success = !success }

(** Apply type equations as substitutions to a type expression.

    Replaces occurrences of rigid type variables with their equation types.
    This is used when type-checking the body of a GADT match branch.

    @param equations List of type equations to apply
    @param ty The type to transform
    @return The type with substitutions applied *)
let apply_equations equations ty =
  Type_traversal.map (fun t ->
    match t with
    | TypeVariable tv when tv.rigid ->
      begin match List.find_opt (fun eq -> eq.eq_variable.id = tv.id) equations with
      | Some eq -> eq.eq_type
      | None -> t
      end
    | _ -> t
  ) ty

(** Apply type equations to a type scheme.

    Applies equations to the body of the scheme. Quantified variables
    are preserved.

    @param equations List of type equations to apply
    @param scheme The type scheme to transform
    @return The scheme with substitutions applied in the body *)
let apply_equations_to_scheme equations scheme =
  { scheme with body = apply_equations equations scheme.body }

(** Check if this is a GADT constructor pattern that needs equation extraction.

    A pattern needs GADT handling if:
    1. The constructor is marked as a GADT constructor
    2. The scrutinee type contains rigid type variables

    @param constructor_info The constructor being matched
    @param scrutinee_type The type of the value being matched
    @return true if GADT equation extraction should be performed *)
let needs_gadt_handling constructor_info scrutinee_type =
  constructor_info.constructor_is_gadt && has_rigid_variables scrutinee_type

(** Check if a type contains any of the given existential type variables.

    @param existential_ids Set of type variable IDs that are existential
    @param ty The type to check
    @return Some var_id if an existential escapes, None otherwise *)
let check_existential_escape existential_ids ty =
  let found = ref None in
  Type_traversal.iter (fun t ->
    match t with
    | TypeVariable tv when List.mem tv.id existential_ids ->
      found := Some tv.id
    | _ -> ()
  ) ty;
  !found

(** Extract existential type variable IDs from a typed pattern.
    These are the fresh type variables created when instantiating
    a GADT constructor with existentials.

    For example, when matching `Any x` where `Any : 'a -> any_expr`,
    the pattern introduces an existential type for x.

    @param pattern The typed pattern to examine
    @return List of existential type variable IDs introduced by this pattern *)
let rec collect_existentials_from_pattern (pattern : Typed_tree.typed_pattern) =
  match pattern.pattern_desc with
  | Typed_tree.TypedPatternConstructor (ctor_info, arg_opt) ->
    (* Only GADT constructors with existentials can introduce existential types.
       The constructor_existentials field contains the *original* existential
       type variable IDs. We need to find the *fresh* instantiated variables
       that correspond to them in the pattern's types.

       The existential variables appear in the argument type but not in the result type.
       After instantiation, they're still in the argument type but not the result type. *)
    let arg_existentials =
      if ctor_info.constructor_existentials = [] then
        []
      else
        match arg_opt with
        | Some arg_pattern ->
          (* Collect type variables from the argument pattern's type *)
          let arg_vars = Type_traversal.free_type_variables arg_pattern.pattern_type in
          (* Collect type variables from the pattern's result type
             (which is the instantiated constructor result type) *)
          let result_vars = Type_traversal.free_type_variables pattern.pattern_type in
          let result_var_ids = List.map (fun tv -> tv.id) result_vars in
          (* Existentials are in arg but not in result *)
          List.filter_map (fun tv ->
            if not (List.mem tv.id result_var_ids) then Some tv.id
            else None
          ) arg_vars
        | None -> []
    in
    (* Also collect from nested patterns *)
    let nested = match arg_opt with
      | Some arg_pattern -> collect_existentials_from_pattern arg_pattern
      | None -> []
    in
    arg_existentials @ nested
  | Typed_tree.TypedPatternTuple patterns ->
    List.concat_map collect_existentials_from_pattern patterns
  | Typed_tree.TypedPatternRecord (fields, _) ->
    List.concat_map (fun (f : Typed_tree.typed_record_pattern_field) ->
      collect_existentials_from_pattern f.typed_pattern_field_pattern
    ) fields
  | _ -> []
