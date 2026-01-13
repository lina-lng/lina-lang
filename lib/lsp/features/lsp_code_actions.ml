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

(** Convert internal range to LSP range. *)
let lsp_range_of_internal (range : Lsp_types.range) : Range.t =
  Range.create
    ~start:(Position.create ~line:range.start_pos.line
              ~character:range.start_pos.character)
    ~end_:(Position.create ~line:range.end_pos.line
             ~character:range.end_pos.character)

(** Convert internal diagnostic to LSP diagnostic. *)
let lsp_diagnostic_of_internal (diag : Lsp_types.diagnostic) : Diagnostic.t =
  let severity =
    match diag.severity with
    | Lsp_types.Error -> Some DiagnosticSeverity.Error
    | Lsp_types.Warning -> Some DiagnosticSeverity.Warning
    | Lsp_types.Information -> Some DiagnosticSeverity.Information
    | Lsp_types.Hint -> Some DiagnosticSeverity.Hint
  in
  Diagnostic.create
    ~range:(lsp_range_of_internal diag.range)
    ~message:(`String diag.message)
    ?code:(Option.map (fun c -> `String c) diag.code)
    ?severity
    ?source:diag.source
    ()

(** Convert LSP range to internal range. *)
let internal_range_of_lsp (range : Range.t) : Lsp_types.range =
  {
    start_pos = { line = range.start.line; character = range.start.character };
    end_pos = { line = range.end_.line; character = range.end_.character };
  }

(** Convert LSP diagnostic to internal diagnostic. *)
let internal_diagnostic_of_lsp (diag : Diagnostic.t) : Lsp_types.diagnostic =
  let severity =
    match diag.severity with
    | Some DiagnosticSeverity.Error -> Lsp_types.Error
    | Some DiagnosticSeverity.Warning -> Lsp_types.Warning
    | Some DiagnosticSeverity.Information -> Lsp_types.Information
    | Some DiagnosticSeverity.Hint -> Lsp_types.Hint
    | None -> Lsp_types.Error
  in
  let code =
    match diag.code with
    | Some (`String s) -> Some s
    | Some (`Int i) -> Some (string_of_int i)
    | None -> None
  in
  let message =
    match diag.message with
    | `String s -> s
    | `MarkupContent m -> m.value
  in
  {
    range = internal_range_of_lsp diag.range;
    severity;
    message;
    code;
    source = diag.source;
    related_information = [];
  }

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
       ~diagnostics:[ lsp_diagnostic_of_internal diag ]
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
         ~diagnostics:[ lsp_diagnostic_of_internal diag ]
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
  let diagnostics = List.map internal_diagnostic_of_lsp params.context.diagnostics in

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
