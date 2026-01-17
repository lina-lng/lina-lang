(** Unused code detection.

    This module analyzes a {!Scope.scope_tree} to detect unused bindings
    and generate appropriate diagnostics. *)

(** Result of unused detection analysis. *)
type result = {
  diagnostics : Common.Compiler_error.diagnostic list;
      (** All unused code diagnostics. *)
  has_errors : bool;
      (** True if any diagnostics would be errors based on config. *)
}

(** [detect config scope_tree] analyzes the scope tree and returns
    diagnostics for all unused bindings.

    The detection rules are:
    - Unused variable: binding has no Read references
    - Unused function: function binding has no references at all
    - Unused parameter: parameter binding has no Read references
    - Unused module: module binding has no references
    - Unused type: type has no references (and not exported)
    - Unused constructor: constructor has no Pattern or Construct references
    - Unused field: record field has no Project references
    - Unused rec: recursive binding only has self-references

    Bindings starting with underscore are skipped (intentionally unused).
    Exported bindings are skipped (used externally).

    The [config] is used to determine severity (warning vs error). *)
val detect :
  Common.Warning_config.t ->
  Scope.scope_tree ->
  result

(** [detect_in_binding binding] checks a single binding for unused status.
    Returns [Some code] if unused, [None] if used. *)
val detect_in_binding :
  Scope.binding ->
  Common.Error_code.t option
