(** Variance inference for type declarations.

    This module infers the variance of each type parameter from how it is used
    in the type definition. Type parameters that appear only in "output" positions
    (covariant) can be safely generalized even for non-values under Garrigue's
    relaxed value restriction.

    {2 Usage}

    When processing a type declaration like:
    {[
      type 'a producer = unit -> 'a
    ]}

    Call [infer_declaration_variances params kind] to get [[Covariant]] because
    ['a] appears only in the result position of the arrow (covariant).

    For a type like:
    {[
      type 'a ref = { mutable contents : 'a }
    ]}

    The variance would be [[Invariant]] because the field can be both read
    (covariant) and written (contravariant), making it invariant overall.

    {2 Explicit Annotations}

    Users can provide explicit variance annotations in the syntax:
    {[
      type +'a producer = unit -> 'a   (* explicitly covariant *)
      type -'a consumer = 'a -> unit   (* explicitly contravariant *)
    ]}

    Use [merge_variances] to combine explicit annotations with inferred variances. *)

(** {1 Variance Operations} *)

(** Flip variance when entering a contravariant context.

    - [Covariant] becomes [Contravariant]
    - [Contravariant] becomes [Covariant]
    - [Invariant] stays [Invariant] *)
val flip_variance : Types.variance -> Types.variance

(** Combine two variances when a parameter appears in multiple positions.

    - [Covariant] + [Covariant] = [Covariant]
    - [Contravariant] + [Contravariant] = [Contravariant]
    - [Covariant] + [Contravariant] = [Invariant]
    - Anything + [Invariant] = [Invariant] *)
val combine_variance : Types.variance -> Types.variance -> Types.variance

(** Apply a declared variance to an actual variance.

    When a type constructor declares its parameter as contravariant,
    the variance of occurrences within that parameter is flipped. *)
val apply_variance : Types.variance -> Types.variance -> Types.variance

(** {1 Inference} *)

(** Infer the variances of all type parameters in a type declaration.

    @param params The type parameters as type variables
    @param kind The kind of type declaration (variant, record, alias, abstract)
    @return A list of variances, one per parameter

    For abstract types, returns [Invariant] for all parameters (most restrictive).
    For variant and record types, analyzes the structure to determine variance. *)
val infer_declaration_variances :
  Types.type_variable list ->
  Types.type_declaration_kind ->
  Types.variance list

(** {1 Merging with Explicit Annotations} *)

(** Merge explicit variance annotations with inferred variances.

    @param explicit Optional explicit variances from the source (None = infer)
    @param inferred Inferred variances from the definition
    @return Final list of variances

    Explicit annotations take precedence over inferred variances.
    This allows users to be more restrictive than inference would suggest. *)
val merge_variances :
  Types.variance option list ->
  Types.variance list ->
  Types.variance list

(** {1 Variance Validation} *)

(** Variance annotation error with details for error messages. *)
type variance_error = {
  param_name : string;
  explicit_variance : Types.variance;
  inferred_variance : Types.variance;
}

(** Validate that explicit variance annotations are compatible with inferred variances.

    Returns [Ok ()] if all annotations are compatible, or [Error err] with details
    about which parameter has an incompatible annotation.

    An annotation is compatible if:
    - The explicit variance matches the inferred variance
    - The explicit variance is more restrictive (e.g., Invariant for any inferred)
    - The inferred variance is Bivariant (parameter unused, any annotation OK) *)
val validate_annotations :
  param_names:string list ->
  explicit:Types.variance option list ->
  inferred:Types.variance list ->
  (unit, variance_error) result

(** Format a variance error for display. *)
val format_variance_error : variance_error -> string
