(** Variance inference for type declarations.

    This module infers the variance of each type parameter from how it is used
    in the type definition. Type parameters that appear only in "output" positions
    (covariant) can be safely generalized even for non-values under Garrigue's
    relaxed value restriction.

    {2 Variance Rules}

    - Direct occurrence: covariant
    - Arrow argument: contravariant (flips the context)
    - Arrow result: covariant
    - Tuple/record fields: covariant
    - Type constructor argument: depends on constructor's declared variance

    {2 Combination Rules}

    When a type variable appears in multiple positions, variances combine:
    - covariant + covariant = covariant
    - contravariant + contravariant = contravariant
    - covariant + contravariant = invariant
    - anything + invariant = invariant *)

open Types

(** Flip variance when entering a contravariant position (e.g., function argument). *)
let flip_variance = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant
  | Bivariant -> Bivariant

(** Combine two variances when a variable appears in multiple positions. *)
let combine_variance variance1 variance2 =
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

(** Apply context variance to a position variance.
    If we're in a contravariant context, flip the position variance. *)
let apply_variance declared_variance actual_variance =
  match declared_variance with
  | Covariant -> actual_variance
  | Contravariant -> flip_variance actual_variance
  | Invariant -> Invariant
  | Bivariant -> Bivariant

(** Get the declared variances for a type constructor's parameters.

    For built-in types:
    - ref: invariant (can both read and write)
    - list, option, etc.: covariant (read-only structure)

    For user-defined types, we would look up the declared variances,
    but for now we conservatively assume covariant. *)
let get_declared_variances (path : path) (param_count : int) : variance list =
  match path with
  | PathBuiltin BuiltinRef -> [Invariant]
  | PathBuiltin _ -> []
  | _ ->
    (* TODO: Look up declared variances from type declarations.
       For now, assume covariant which is safe for most types. *)
    List.init param_count (fun _ -> Covariant)

(** Infer the variance of a type parameter within a type expression.

    @param param_id The id of the type parameter we're checking
    @param context_variance The variance of the current context (starts as Covariant)
    @param ty The type to search within
    @return The combined variance of param_id in ty, or None if not present *)
let rec infer_variance_in_type
    (param_id : int)
    (context_variance : variance)
    (ty : type_expression)
  : variance option =
  match representative ty with
  | TypeVariable tv ->
    if tv.id = param_id then
      Some context_variance
    else
      None  (* Different variable, not the one we're looking for *)

  | TypeConstructor (path, args) ->
    let param_variances = get_declared_variances path (List.length args) in
    (* Combine variances from all arguments *)
    List.fold_left2
      (fun accumulated arg param_variance ->
        let effective_variance = apply_variance param_variance context_variance in
        let arg_variance = infer_variance_in_type param_id effective_variance arg in
        match accumulated, arg_variance with
        | None, v -> v
        | v, None -> v
        | Some v1, Some v2 -> Some (combine_variance v1 v2))
      None
      args
      param_variances

  | TypeTuple elements ->
    (* Tuple elements are covariant *)
    List.fold_left
      (fun accumulated element ->
        let element_variance = infer_variance_in_type param_id context_variance element in
        match accumulated, element_variance with
        | None, v -> v
        | v, None -> v
        | Some v1, Some v2 -> Some (combine_variance v1 v2))
      None
      elements

  | TypeArrow (arg_type, result_type) ->
    (* Argument is contravariant, result is covariant *)
    let arg_variance =
      infer_variance_in_type param_id (flip_variance context_variance) arg_type
    in
    let result_variance =
      infer_variance_in_type param_id context_variance result_type
    in
    begin match arg_variance, result_variance with
    | None, v -> v
    | v, None -> v
    | Some v1, Some v2 -> Some (combine_variance v1 v2)
    end

  | TypeRecord row ->
    infer_variance_in_row param_id context_variance row

  | TypeRowEmpty ->
    None

(** Check variance in a row type (for records). *)
and infer_variance_in_row
    (param_id : int)
    (context_variance : variance)
    (row : row)
  : variance option =
  let field_variance =
    List.fold_left
      (fun accumulated (_, field) ->
        match field with
        | RowFieldPresent field_type ->
          (* Record fields are covariant (read-only access) *)
          let field_var = infer_variance_in_type param_id context_variance field_type in
          match accumulated, field_var with
          | None, v -> v
          | v, None -> v
          | Some v1, Some v2 -> Some (combine_variance v1 v2))
      None
      row.row_fields
  in
  (* Also check the row extension variable *)
  let row_more_variance =
    infer_variance_in_type param_id context_variance row.row_more
  in
  match field_variance, row_more_variance with
  | None, v -> v
  | v, None -> v
  | Some v1, Some v2 -> Some (combine_variance v1 v2)

(** Infer variances for all parameters of a type declaration.

    @param params The type parameters (type variables)
    @param kind The type declaration kind (abstract, variant, record, alias)
    @return A list of variances, one per parameter *)
let infer_declaration_variances
    (params : type_variable list)
    (kind : type_declaration_kind)
  : variance list =
  match kind with
  | DeclarationAbstract ->
    (* Abstract types: default to invariant (most restrictive, safest) *)
    List.map (fun _ -> Invariant) params

  | DeclarationVariant constructors ->
    List.map (fun param ->
      let variance_from_ctors =
        List.fold_left (fun accumulated ctor ->
          match ctor.constructor_argument_type with
          | None -> accumulated
          | Some arg_ty ->
            let arg_variance = infer_variance_in_type param.id Covariant arg_ty in
            match accumulated, arg_variance with
            | None, v -> v
            | v, None -> v
            | Some v1, Some v2 -> Some (combine_variance v1 v2)
        ) None constructors
      in
      (* If the parameter doesn't appear, it's bivariant (phantom type) *)
      Option.value variance_from_ctors ~default:Bivariant
    ) params

  | DeclarationRecord fields ->
    List.map (fun param ->
      let variance_from_fields =
        List.fold_left (fun accumulated (_, field_ty) ->
          let field_variance = infer_variance_in_type param.id Covariant field_ty in
          match accumulated, field_variance with
          | None, v -> v
          | v, None -> v
          | Some v1, Some v2 -> Some (combine_variance v1 v2)
        ) None fields
      in
      (* If the parameter doesn't appear, it's bivariant (phantom type) *)
      Option.value variance_from_fields ~default:Bivariant
    ) params

(** Merge explicit variance annotations with inferred variances.

    Explicit annotations take precedence over inferred variances.

    @param explicit Optional explicit variances from the source
    @param inferred Inferred variances from the definition
    @return Final list of variances *)
let merge_variances
    (explicit : variance option list)
    (inferred : variance list)
  : variance list =
  List.map2 (fun exp inf ->
    match exp with
    | Some v -> v  (* Explicit annotation takes precedence *)
    | None -> inf  (* Fall back to inferred *)
  ) explicit inferred

(** Check if an explicit variance annotation is compatible with inferred variance.

    The explicit annotation must be at least as restrictive as the inferred variance:
    - Bivariant (inferred) is compatible with any annotation (parameter unused)
    - Invariant (inferred) requires Invariant annotation (or none)
    - Covariant (inferred) is compatible with Covariant or Invariant annotation
    - Contravariant (inferred) is compatible with Contravariant or Invariant annotation *)
let is_annotation_compatible ~explicit ~inferred =
  match explicit, inferred with
  (* Bivariant inferred: parameter unused, any annotation is fine *)
  | _, Bivariant -> true
  (* No explicit annotation is always compatible (we use inferred) *)
  | None, _ -> true
  (* Same variance is compatible *)
  | Some Covariant, Covariant -> true
  | Some Contravariant, Contravariant -> true
  | Some Invariant, Invariant -> true
  (* Invariant annotation is compatible with any inferred variance
     (more restrictive than needed is OK) *)
  | Some Invariant, _ -> true
  (* Bivariant annotation is only OK for bivariant inferred (handled above)
     or covariant/contravariant (declaring phantom is OK) *)
  | Some Bivariant, (Covariant | Contravariant) -> true
  (* Bivariant annotation is NOT compatible with Invariant inferred
     (can't declare phantom if parameter is used in both positions) *)
  | Some Bivariant, Invariant -> false
  (* Covariant annotation requires covariant or bivariant inferred *)
  | Some Covariant, (Contravariant | Invariant) -> false
  (* Contravariant annotation requires contravariant or bivariant inferred *)
  | Some Contravariant, (Covariant | Invariant) -> false

(** Variance annotation error with details for error messages. *)
type variance_error = {
  param_name : string;
  explicit_variance : variance;
  inferred_variance : variance;
}

(** Validate that explicit variance annotations are compatible with inferred variances.

    @param param_names Names of type parameters for error messages
    @param explicit Optional explicit variances from the source
    @param inferred Inferred variances from the definition
    @return [Ok ()] if all annotations are compatible, or [Error err] with details *)
let validate_annotations
    ~(param_names : string list)
    ~(explicit : variance option list)
    ~(inferred : variance list)
  : (unit, variance_error) result =
  let rec check names expls infs =
    match names, expls, infs with
    | [], [], [] -> Ok ()
    | name :: rest_names, exp :: rest_exp, inf :: rest_inf ->
      if is_annotation_compatible ~explicit:exp ~inferred:inf then
        check rest_names rest_exp rest_inf
      else begin
        match exp with
        | Some explicit_v ->
          Error { param_name = name; explicit_variance = explicit_v; inferred_variance = inf }
        | None ->
          (* Should not happen since no annotation is always compatible *)
          check rest_names rest_exp rest_inf
      end
    | _ ->
      (* Length mismatch - should not happen with well-formed inputs *)
      Ok ()
  in
  check param_names explicit inferred

(** Format a variance error for display. *)
let format_variance_error (err : variance_error) : string =
  let explicit_str = match err.explicit_variance with
    | Covariant -> "covariant (+)"
    | Contravariant -> "contravariant (-)"
    | Invariant -> "invariant"
    | Bivariant -> "bivariant (_)"
  in
  let inferred_str = match err.inferred_variance with
    | Covariant -> "covariant (output position only)"
    | Contravariant -> "contravariant (input position only)"
    | Invariant -> "invariant (both input and output positions)"
    | Bivariant -> "bivariant (unused)"
  in
  Printf.sprintf
    "Type parameter '%s is declared as %s but is used as %s in the definition"
    err.param_name explicit_str inferred_str
