(** Value restriction checking.

    Implements Wright's value restriction to ensure soundness of polymorphic
    references in ML-family type systems.

    See value_check.mli for detailed documentation. *)

open Typed_tree
open Parsing.Syntax_tree

let rec is_value (expression : typed_expression) : bool =
  match expression.expression_desc with
  (* Variables are always values - they reference existing bindings *)
  | TypedExpressionVariable _ -> true

  (* Constants are values - no allocation or effects *)
  | TypedExpressionConstant _ -> true

  (* Lambda abstractions are values - they don't evaluate their body *)
  | TypedExpressionFunction _ -> true

  (* Tuples are values if all components are values *)
  | TypedExpressionTuple components ->
    List.for_all is_value components

  (* Nullary constructors (like None) are values *)
  | TypedExpressionConstructor (_, None) -> true

  (* Constructors with arguments are values if the argument is a value *)
  | TypedExpressionConstructor (_, Some argument) ->
    is_value argument

  (* Records are values if all field values are values *)
  | TypedExpressionRecord fields ->
    List.for_all (fun field -> is_value field.typed_field_value) fields

  (* Let bindings are values if:
     - All bound expressions are values
     - The body is a value
     This allows: let x = 1 in let y = 2 in (x, y) *)
  | TypedExpressionLet (_, bindings, body) ->
    List.for_all (fun (binding : Typed_tree.typed_binding) ->
      is_value binding.binding_expression
    ) bindings
    && is_value body

  (* ----------------------------------------
     The following are NOT values (expansive)
     ---------------------------------------- *)

  (* Function applications can create new mutable state (e.g., ref []) *)
  | TypedExpressionApply _ -> false

  (* Match expressions evaluate branches which may have effects *)
  | TypedExpressionMatch _ -> false

  (* If expressions evaluate branches which may have effects *)
  | TypedExpressionIf _ -> false

  (* Sequences explicitly allow effects in the first expression *)
  | TypedExpressionSequence _ -> false

  (* Record access could trigger evaluation in lazy contexts *)
  | TypedExpressionRecordAccess _ -> false

  (* Record update creates a new record - could involve allocation *)
  | TypedExpressionRecordUpdate _ -> false

  (* Module access could trigger module initialization effects *)
  | TypedExpressionModuleAccess _ -> false


let rec is_syntax_value (expression : Parsing.Syntax_tree.expression) : bool =
  match expression.Common.Location.value with
  (* Variables are always values *)
  | ExpressionVariable _ -> true

  (* Constants are values *)
  | ExpressionConstant _ -> true

  (* Lambda abstractions are values *)
  | ExpressionFunction _ -> true

  (* Tuples are values if all components are values *)
  | ExpressionTuple components ->
    List.for_all is_syntax_value components

  (* Nullary constructors (like None) are values *)
  | ExpressionConstructor (_, None) -> true

  (* Constructors with arguments are values if the argument is a value *)
  | ExpressionConstructor (_, Some argument) ->
    is_syntax_value argument

  (* Records are values if all field values are values *)
  | ExpressionRecord fields ->
    List.for_all (fun field -> is_syntax_value field.field_value) fields

  (* Let bindings are values if all parts are values *)
  | ExpressionLet (_, bindings, body) ->
    List.for_all (fun (binding : Parsing.Syntax_tree.binding) ->
      is_syntax_value binding.binding_expression
    ) bindings
    && is_syntax_value body

  (* Type constraints don't affect value-ness *)
  | ExpressionConstraint (inner, _) ->
    is_syntax_value inner

  (* ----------------------------------------
     The following are NOT values (expansive)
     ---------------------------------------- *)

  | ExpressionApply _ -> false
  | ExpressionMatch _ -> false
  | ExpressionIf _ -> false
  | ExpressionSequence _ -> false
  | ExpressionRecordAccess _ -> false
  | ExpressionRecordUpdate _ -> false
  | ExpressionModuleAccess _ -> false


(** {1 Relaxed Value Restriction - Variance Checking} *)

(** Variance of a type variable in a type expression. *)
type variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

(** Flip variance when entering a contravariant position (left side of arrow). *)
let flip_variance = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant
  | Bivariant -> Bivariant

(** Combine two variances when a variable appears in multiple positions. *)
let combine_variance variance1 variance2 =
  match variance1, variance2 with
  | Bivariant, other | other, Bivariant -> other
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  | Invariant, _ | _, Invariant -> Invariant
  | Covariant, Contravariant | Contravariant, Covariant -> Invariant

(** Apply context variance to a position variance.
    If we're in a contravariant context, flip the position variance. *)
let apply_context_variance context_variance position_variance =
  match context_variance with
  | Covariant -> position_variance
  | Contravariant -> flip_variance position_variance
  | Invariant -> Invariant
  | Bivariant -> Bivariant

(** Get the variance of type parameters for a type constructor.

    For built-in and standard types:
    - ref: invariant (can read and write)
    - list, option, and most others: covariant

    For user-defined types, we conservatively assume covariant.
    A more complete implementation would track declared variances. *)
let get_constructor_param_variances (path : Types.path) (param_count : int) : variance list =
  match path with
  | Types.PathLocal "ref" ->
    (* ref is invariant - both reading and writing *)
    List.init param_count (fun _ -> Invariant)
  | _ ->
    (* Most type constructors are covariant in their parameters *)
    List.init param_count (fun _ -> Covariant)

(** Check the variance of a type variable within a type expression.

    @param target_var The type variable we're checking
    @param context_variance The variance of the current context
    @param ty The type to search within
    @return The variance of target_var in ty *)
let rec check_variance_in_context
    (target_var : Types.type_variable)
    (context_variance : variance)
    (ty : Types.type_expression)
  : variance =
  match Types.representative ty with
  | Types.TypeVariable tv ->
    if tv.Types.id = target_var.Types.id then
      context_variance
    else
      Bivariant  (* Different variable, not present *)

  | Types.TypeConstructor (path, args) ->
    let param_variances = get_constructor_param_variances path (List.length args) in
    List.fold_left2
      (fun accumulated_variance arg param_variance ->
        let effective_variance = apply_context_variance context_variance param_variance in
        let arg_variance = check_variance_in_context target_var effective_variance arg in
        combine_variance accumulated_variance arg_variance)
      Bivariant
      args
      param_variances

  | Types.TypeTuple elements ->
    (* Tuple elements are covariant *)
    List.fold_left
      (fun accumulated_variance element ->
        let element_variance = check_variance_in_context target_var context_variance element in
        combine_variance accumulated_variance element_variance)
      Bivariant
      elements

  | Types.TypeArrow (arg_type, result_type) ->
    (* Argument is contravariant, result is covariant *)
    let arg_variance =
      check_variance_in_context target_var (flip_variance context_variance) arg_type
    in
    let result_variance =
      check_variance_in_context target_var context_variance result_type
    in
    combine_variance arg_variance result_variance

  | Types.TypeRecord row ->
    check_variance_in_row target_var context_variance row

  | Types.TypeRowEmpty ->
    Bivariant

(** Check variance in a row type (for records). *)
and check_variance_in_row
    (target_var : Types.type_variable)
    (context_variance : variance)
    (row : Types.row)
  : variance =
  let field_variance =
    List.fold_left
      (fun accumulated_variance (_, field) ->
        match field with
        | Types.RowFieldPresent field_type ->
          (* Record fields are covariant (read-only access) *)
          let fv = check_variance_in_context target_var context_variance field_type in
          combine_variance accumulated_variance fv)
      Bivariant
      row.Types.row_fields
  in
  (* Also check the row extension variable *)
  let row_more_variance =
    check_variance_in_context target_var context_variance row.Types.row_more
  in
  combine_variance field_variance row_more_variance

(** Check the variance of a type variable in a type expression.
    Entry point that starts in a covariant context. *)
let check_variance (target_var : Types.type_variable) (ty : Types.type_expression) : variance =
  check_variance_in_context target_var Covariant ty

(** Check if a type variable can be generalized under relaxed value restriction.
    A variable can be generalized if it appears only in covariant positions
    or does not appear at all. *)
let can_generalize_relaxed (target_var : Types.type_variable) (ty : Types.type_expression) : bool =
  match check_variance target_var ty with
  | Covariant | Bivariant -> true
  | Contravariant | Invariant -> false
