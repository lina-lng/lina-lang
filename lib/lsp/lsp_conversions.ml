(** LSP type conversions between internal types and linol/LSP types.

    This module centralizes all conversions between our internal Lsp_types
    and the external Lsp.Types from linol. *)

module Lsp = Linol.Lsp
open Lsp.Types

(** {1 Position Conversions} *)

(** Convert internal position to LSP position. *)
let lsp_position_of_internal (pos : Lsp_types.position) : Position.t =
  Position.create ~line:pos.line ~character:pos.character

(** Convert LSP position to internal position. *)
let internal_position_of_lsp (pos : Position.t) : Lsp_types.position =
  { line = pos.line; character = pos.character }

(** {1 Range Conversions} *)

(** Convert internal range to LSP range. *)
let lsp_range_of_internal (range : Lsp_types.range) : Range.t =
  Range.create
    ~start:(lsp_position_of_internal range.start_pos)
    ~end_:(lsp_position_of_internal range.end_pos)

(** Convert LSP range to internal range. *)
let internal_range_of_lsp (range : Range.t) : Lsp_types.range =
  {
    start_pos = internal_position_of_lsp range.start;
    end_pos = internal_position_of_lsp range.end_;
  }

(** {1 Severity Conversions} *)

(** Convert internal severity to LSP severity. *)
let lsp_severity_of_internal (severity : Lsp_types.diagnostic_severity)
    : DiagnosticSeverity.t option =
  match severity with
  | Lsp_types.Error -> Some DiagnosticSeverity.Error
  | Lsp_types.Warning -> Some DiagnosticSeverity.Warning
  | Lsp_types.Information -> Some DiagnosticSeverity.Information
  | Lsp_types.Hint -> Some DiagnosticSeverity.Hint

(** Convert LSP severity to internal severity. *)
let internal_severity_of_lsp (severity : DiagnosticSeverity.t option)
    : Lsp_types.diagnostic_severity =
  match severity with
  | Some DiagnosticSeverity.Error -> Lsp_types.Error
  | Some DiagnosticSeverity.Warning -> Lsp_types.Warning
  | Some DiagnosticSeverity.Information -> Lsp_types.Information
  | Some DiagnosticSeverity.Hint -> Lsp_types.Hint
  | None -> Lsp_types.Error

(** {1 Diagnostic Conversions} *)

(** Convert internal diagnostic to LSP diagnostic. *)
let lsp_diagnostic_of_internal (diag : Lsp_types.diagnostic) : Diagnostic.t =
  Diagnostic.create
    ~range:(lsp_range_of_internal diag.range)
    ~message:(`String diag.message)
    ?code:(Option.map (fun c -> `String c) diag.code)
    ?severity:(lsp_severity_of_internal diag.severity)
    ?source:diag.source
    ()

(** Convert LSP diagnostic to internal diagnostic. *)
let internal_diagnostic_of_lsp (diag : Diagnostic.t) : Lsp_types.diagnostic =
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
    severity = internal_severity_of_lsp diag.severity;
    message;
    code;
    source = diag.source;
    related_information = [];
  }
