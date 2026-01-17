(** Dead code detection.

    This module detects unreachable code in typed expressions using
    control flow analysis. *)

(** A dead code finding. *)
type finding = {
  location : Common.Location.t;
      (** Location of the unreachable code. *)
  reason : string;
      (** Description of why the code is unreachable. *)
}

(** [detect_in_expression expr] finds all unreachable code in an expression. *)
val detect_in_expression : Typing.Typed_tree.typed_expression -> finding list

(** [detect_in_structure structure] finds all unreachable code in a structure. *)
val detect_in_structure : Typing.Typed_tree.typed_structure -> finding list

(** [to_diagnostic config finding] converts a finding to a diagnostic. *)
val to_diagnostic :
  Common.Warning_config.t ->
  finding ->
  Common.Compiler_error.diagnostic
