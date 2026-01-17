(** Analysis pipeline orchestration.

    This module coordinates all static analyses on typed ASTs:
    - Scope analysis
    - Unused code detection
    - Dead code detection
    - Pattern exhaustiveness (delegated to typing)

    All diagnostics are collected and filtered according to the warning
    configuration. *)

(** Result of running the analysis pipeline. *)
type result = {
  scope_tree : Scope.scope_tree;
      (** The scope tree built during analysis. *)
  diagnostics : Common.Compiler_error.diagnostic list;
      (** All diagnostics from all analyses. *)
  has_errors : bool;
      (** True if any diagnostics are errors (based on config). *)
}

(** [run config typed_ast] runs all analyses on the typed AST.

    The analyses are run in order:
    1. Scope analysis (builds binding/reference information)
    2. Unused detection (finds unused bindings)
    3. Dead code detection (finds unreachable code)

    Returns all diagnostics filtered by the warning configuration. *)
val run :
  Common.Warning_config.t ->
  Typing.Typed_tree.typed_structure ->
  result

(** [run_with_signature config typed_ast signature] runs analyses with an
    explicit module signature. Bindings appearing in the signature are
    considered exported and won't trigger unused warnings. *)
val run_with_signature :
  Common.Warning_config.t ->
  Typing.Typed_tree.typed_structure ->
  Typing.Module_types.signature option ->
  result
