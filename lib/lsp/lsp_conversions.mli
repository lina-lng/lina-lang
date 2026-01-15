(** LSP type conversions between internal types and linol/LSP types.

    This module centralizes all conversions between our internal Lsp_types
    and the external Lsp.Types from linol, eliminating duplication across
    LSP feature modules. *)

module Lsp = Linol.Lsp

(** {1 Position Conversions} *)

(** Convert internal position to LSP position. *)
val lsp_position_of_internal : Lsp_types.position -> Lsp.Types.Position.t

(** Convert LSP position to internal position. *)
val internal_position_of_lsp : Lsp.Types.Position.t -> Lsp_types.position

(** {1 Range Conversions} *)

(** Convert internal range to LSP range. *)
val lsp_range_of_internal : Lsp_types.range -> Lsp.Types.Range.t

(** Convert LSP range to internal range. *)
val internal_range_of_lsp : Lsp.Types.Range.t -> Lsp_types.range

(** {1 Severity Conversions} *)

(** Convert internal severity to LSP severity. *)
val lsp_severity_of_internal :
  Lsp_types.diagnostic_severity -> Lsp.Types.DiagnosticSeverity.t option

(** Convert LSP severity to internal severity. *)
val internal_severity_of_lsp :
  Lsp.Types.DiagnosticSeverity.t option -> Lsp_types.diagnostic_severity

(** {1 Diagnostic Conversions} *)

(** Convert internal diagnostic to LSP diagnostic. *)
val lsp_diagnostic_of_internal : Lsp_types.diagnostic -> Lsp.Types.Diagnostic.t

(** Convert LSP diagnostic to internal diagnostic. *)
val internal_diagnostic_of_lsp : Lsp.Types.Diagnostic.t -> Lsp_types.diagnostic
