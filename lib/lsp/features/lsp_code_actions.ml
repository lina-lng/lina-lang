(** LSP code actions for quick fixes and refactorings.

    This module provides code actions that appear as "quick fixes" in the editor.
    It works directly with LSP protocol types for clean integration. *)

open Common

module Lsp = Linol.Lsp
open Lsp.Types

(** Code action kinds. *)
module Kind = struct
  let quick_fix = CodeActionKind.QuickFix
end

(** Check if a diagnostic has a specific error code. *)
let has_code code (diag : Lsp_types.diagnostic) =
  match diag.code with
  | Some c -> c = code
  | None -> false

(** Create a code action with no edit (informational only). *)
let make_info_action ~title ~(diag : Lsp_types.diagnostic) =
  `CodeAction
    (CodeAction.create
       ~title
       ~kind:Kind.quick_fix
       ~diagnostics:[ Lsp_conversions.lsp_diagnostic_of_internal diag ]
       ~isPreferred:false
       ())

(** Generate actions for unbound variable errors. *)
let actions_for_unbound_variable (diag : Lsp_types.diagnostic) =
  [ make_info_action ~title:"Check spelling of variable name" ~diag ]

(** Generate actions for non-exhaustive pattern match warnings. *)
let actions_for_non_exhaustive (diag : Lsp_types.diagnostic) =
  [ make_info_action ~title:"Add wildcard pattern '| _ -> ...'" ~diag ]

(** Generate actions for redundant pattern warnings. *)
let actions_for_redundant_pattern (diag : Lsp_types.diagnostic) =
  [
    `CodeAction
      (CodeAction.create
         ~title:"Remove redundant pattern"
         ~kind:Kind.quick_fix
         ~diagnostics:[ Lsp_conversions.lsp_diagnostic_of_internal diag ]
         ~isPreferred:true
         ());
  ]

(** Generate actions for type mismatch errors. *)
let actions_for_type_mismatch (diag : Lsp_types.diagnostic) =
  if String.length diag.message > 0 then
    [ make_info_action ~title:"View type mismatch details" ~diag ]
  else []

(** Get code actions for given diagnostics.

    @param params The code action request parameters from the LSP client
    @return List of code actions to present to the user *)
let get_code_actions (params : CodeActionParams.t) : CodeActionResult.t =
  let diagnostics = List.map Lsp_conversions.internal_diagnostic_of_lsp params.context.diagnostics in

  let unbound_value_code = Error_code.to_string Error_code.e_unbound_value in
  let non_exhaustive_code = Error_code.to_string Error_code.w_non_exhaustive in
  let redundant_code = Error_code.to_string Error_code.w_redundant_pattern in
  let type_mismatch_code = Error_code.to_string Error_code.e_type_mismatch in

  let actions =
    List.concat_map
      (fun diag ->
        if has_code unbound_value_code diag then
          actions_for_unbound_variable diag
        else if has_code non_exhaustive_code diag then
          actions_for_non_exhaustive diag
        else if has_code redundant_code diag then
          actions_for_redundant_pattern diag
        else if has_code type_mismatch_code diag then
          actions_for_type_mismatch diag
        else [])
      diagnostics
  in
  if actions = [] then None else Some actions
