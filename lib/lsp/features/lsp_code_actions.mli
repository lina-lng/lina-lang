(** LSP code actions for quick fixes and refactorings.

    This module provides code actions that appear as "quick fixes" in the editor,
    typically shown as lightbulb icons or available via keyboard shortcuts. *)

module Lsp = Linol.Lsp

(** Get code actions for a code action request.

    Analyzes the diagnostics in the request and returns appropriate
    quick fix suggestions.

    @param params The code action request parameters from the client
    @return Code actions to present to the user, or None if no actions available *)
val get_code_actions : Lsp.Types.CodeActionParams.t -> Lsp.Types.CodeActionResult.t
