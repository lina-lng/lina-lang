(** Pattern exhaustiveness and redundancy checking *)

(** Witness showing an unmatched pattern for error messages *)
type witness

(** Convert witness to string for error messages *)
val witness_to_string : witness -> string

(** Check exhaustiveness, returns Some witness if not exhaustive *)
val check_exhaustiveness :
  Environment.t ->
  Types.type_expression ->
  Typed_tree.typed_match_arm list ->
  witness option

(** Check for redundant patterns, returns locations of redundant arms *)
val check_redundancy :
  Environment.t ->
  Types.type_expression ->
  Typed_tree.typed_match_arm list ->
  Common.Location.t list

(** Main entry point - check match and emit warnings.
    Call this after typing match arms to verify exhaustiveness and redundancy. *)
val check_match :
  Environment.t ->
  Common.Location.t ->
  Types.type_expression ->
  Typed_tree.typed_match_arm list ->
  unit
