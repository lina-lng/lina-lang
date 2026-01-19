(** Variance operations for type parameters.

    This module provides operations for manipulating type parameter variances.
    See {!module:Variance.mli} for detailed documentation. *)

type t = Types.variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

let flip = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant
  | Bivariant -> Bivariant

let combine variance1 variance2 =
  match variance1, variance2 with
  (* Bivariant is the identity for combination *)
  | Bivariant, other | other, Bivariant -> other
  (* Same variance stays the same *)
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  (* Invariant absorbs everything *)
  | Invariant, _ | _, Invariant -> Invariant
  (* Conflicting variances become invariant *)
  | Covariant, Contravariant | Contravariant, Covariant -> Invariant

let combine_opt variance1_opt variance2_opt =
  match variance1_opt, variance2_opt with
  | None, variance -> variance
  | variance, None -> variance
  | Some variance1, Some variance2 -> Some (combine variance1 variance2)

let compose context position =
  match context with
  | Covariant -> position
  | Contravariant -> flip position
  | Invariant -> Invariant
  | Bivariant -> Bivariant

let compatible ~impl ~decl =
  match decl, impl with
  (* Bivariant declaration accepts anything (phantom type parameter) *)
  | Bivariant, _ -> true
  (* Bivariant implementation means parameter is unused, compatible with anything *)
  | _, Bivariant -> true
  (* Same variance is compatible *)
  | Covariant, Covariant -> true
  | Contravariant, Contravariant -> true
  (* Invariant implementation is compatible with any declaration
     (impl is more restrictive) *)
  | _, Invariant -> true
  (* Covariant/Contravariant impl is NOT compatible with Invariant decl
     (impl is less restrictive) *)
  | Invariant, Covariant -> false
  | Invariant, Contravariant -> false
  (* Conflicting variances are incompatible *)
  | Covariant, Contravariant -> false
  | Contravariant, Covariant -> false

let to_string = function
  | Covariant -> "+"
  | Contravariant -> "-"
  | Invariant -> ""
  | Bivariant -> "_"

let pp fmt variance =
  Format.fprintf fmt "%s" (to_string variance)

let to_annotation = function
  | Covariant -> "+"
  | Contravariant -> "-"
  | Invariant -> "" (* No annotation means invariant *)
  | Bivariant -> "_" (* Underscore for unused/phantom *)

(** Type lookup function for finding type declarations *)
type type_lookup = Types.path -> Types.type_declaration option

(** Default type lookup that returns None (used when no environment available) *)
let no_lookup : type_lookup = fun _ -> None

(** Global reference for type lookup - set before variance checking *)
let current_type_lookup : type_lookup ref = ref no_lookup

let get_constructor_variances (path : Types.path) (param_count : int) : t list =
  match path with
  | Types.PathBuiltin Types.BuiltinRef ->
    (* ref is invariant - both reading and writing *)
    [Invariant]
  | Types.PathBuiltin Types.BuiltinArray ->
    (* array is invariant - mutable container *)
    [Invariant]
  | Types.PathBuiltin Types.BuiltinDict ->
    (* dict is covariant in both key and value - immutable container *)
    [Covariant; Covariant]
  | Types.PathBuiltin Types.BuiltinSet ->
    (* set is covariant - immutable container *)
    [Covariant]
  | Types.PathBuiltin _ ->
    (* Other builtins have no type parameters *)
    []
  | _ ->
    (* Look up declared variances from type declaration *)
    match !current_type_lookup path with
    | Some decl when decl.Types.declaration_variances <> [] ->
      decl.Types.declaration_variances
    | Some decl ->
      (* No declared variances - infer from type definition *)
      (* For now, assume covariant if no info available *)
      List.init (List.length decl.Types.declaration_parameters) (fun _ -> Covariant)
    | None ->
      (* Type not found - assume covariant *)
      List.init param_count (fun _ -> Covariant)

(** {1 Variance Checking in Types}

    These functions check how a type variable occurs within a type expression.
    Used for relaxed value restriction. *)

(** Check the variance of a type variable within a type expression.

    @param target_var The type variable we're checking
    @param context_variance The variance of the current context
    @param ty The type to search within
    @return The variance of target_var in ty *)
let rec check_in_context
    (target_var : Types.type_variable)
    (context_variance : t)
    (ty : Types.type_expression)
  : t =
  match Types.representative ty with
  | Types.TypeVariable tv ->
    if tv.Types.id = target_var.Types.id then
      context_variance
    else
      Bivariant  (* Different variable, not present *)

  | Types.TypeConstructor (path, args) ->
    let param_variances = get_constructor_variances path (List.length args) in
    List.fold_left2
      (fun accumulated_variance arg param_variance ->
        let effective_variance = compose context_variance param_variance in
        let arg_variance = check_in_context target_var effective_variance arg in
        combine accumulated_variance arg_variance)
      Bivariant
      args
      param_variances

  | Types.TypeTuple elements ->
    (* Tuple elements are covariant *)
    List.fold_left
      (fun accumulated_variance element ->
        let element_variance = check_in_context target_var context_variance element in
        combine accumulated_variance element_variance)
      Bivariant
      elements

  | Types.TypeArrow (_, arg_type, result_type) ->
    (* Argument is contravariant, result is covariant *)
    let arg_variance =
      check_in_context target_var (flip context_variance) arg_type
    in
    let result_variance =
      check_in_context target_var context_variance result_type
    in
    combine arg_variance result_variance

  | Types.TypeRecord row ->
    check_in_row target_var context_variance row

  | Types.TypePolyVariant pv_row ->
    check_in_poly_variant_row target_var context_variance pv_row

  | Types.TypeRowEmpty ->
    Bivariant

  | Types.TypePackage pkg ->
    (* Check variance in all type constraints of the package *)
    List.fold_left
      (fun accumulated_variance (_, ty) ->
        let ty_variance = check_in_context target_var context_variance ty in
        combine accumulated_variance ty_variance)
      Bivariant
      pkg.Types.package_signature

(** Check variance in a row type (for records). *)
and check_in_row
    (target_var : Types.type_variable)
    (context_variance : t)
    (row : Types.row)
  : t =
  let field_variance =
    List.fold_left
      (fun accumulated_variance (_, field) ->
        match field with
        | Types.RowFieldPresent field_type ->
          (* Record fields are covariant (read-only access) *)
          let fv = check_in_context target_var context_variance field_type in
          combine accumulated_variance fv)
      Bivariant
      row.Types.row_fields
  in
  (* Also check the row extension variable *)
  let row_more_variance =
    check_in_context target_var context_variance row.Types.row_more
  in
  combine field_variance row_more_variance

(** Check variance in a polymorphic variant row type. *)
and check_in_poly_variant_row
    (target_var : Types.type_variable)
    (context_variance : t)
    (pv_row : Types.poly_variant_row)
  : t =
  let field_variance =
    List.fold_left
      (fun accumulated_variance (_, field) ->
        match field with
        | Types.PVFieldPresent (Some field_type) ->
          (* Variant arguments are covariant *)
          let fv = check_in_context target_var context_variance field_type in
          combine accumulated_variance fv
        | Types.PVFieldPresent None | Types.PVFieldAbsent ->
          accumulated_variance)
      Bivariant
      pv_row.Types.pv_fields
  in
  (* Also check the row extension variable *)
  let row_more_variance =
    check_in_context target_var context_variance pv_row.Types.pv_more
  in
  combine field_variance row_more_variance

(** Check the variance of a type variable in a type expression.
    Entry point that starts in a covariant context. *)
let check_in_type (target_var : Types.type_variable) (ty : Types.type_expression) : t =
  check_in_context target_var Covariant ty

(** Set the type lookup function for variance checking.
    Should be called before checking variance with an environment-based lookup. *)
let set_type_lookup (lookup : type_lookup) : unit =
  current_type_lookup := lookup

(** Check variance with a specific type lookup function. *)
let check_in_type_with_lookup
    ~(type_lookup : type_lookup)
    (target_var : Types.type_variable)
    (ty : Types.type_expression)
  : t =
  let old_lookup = !current_type_lookup in
  current_type_lookup := type_lookup;
  let result = check_in_context target_var Covariant ty in
  current_type_lookup := old_lookup;
  result
