(** Compiler error and warning infrastructure.

    This module provides a unified system for reporting errors and warnings
    throughout the compilation pipeline. Errors are fatal and raised as
    exceptions, while warnings are collected and reported after compilation.

    {2 Error Categories}

    - {!LexerError}: Invalid characters, unterminated strings/comments
    - {!ParserError}: Syntax errors, unexpected tokens
    - {!TypeError}: Type mismatches, unbound variables
    - {!InternalError}: Compiler bugs (should never occur in normal use)

    {2 Usage}

    {[
      (* Raise a type error *)
      Compiler_error.type_error loc "Expected int, got string"

      (* Emit a warning *)
      Compiler_error.emit_warning (NonExhaustiveMatch "option") loc

      (* Report all warnings *)
      List.iter (report_warning Format.std_formatter) (get_warnings ())
    ]} *)

(** {1 Error Types} *)

(** The category of compiler error. *)
type kind =
  | LexerError of string      (** Lexical analysis error *)
  | ParserError of string     (** Syntax error *)
  | TypeError of string       (** Type checking error *)
  | InternalError of string   (** Internal compiler error *)
[@@deriving show]

(** A compiler error with location and optional hints. *)
type t = {
  kind : kind;              (** Error category *)
  location : Location.t;    (** Source location *)
  hints : string list;      (** Suggestions for fixing the error *)
}
[@@deriving show]

(** Exception raised for all compiler errors. *)
exception Error of t

(** {1 Error Creation} *)

(** [make kind loc] creates an error without hints. *)
val make : kind -> Location.t -> t

(** [with_hints err hints] adds hints to an existing error. *)
val with_hints : t -> string list -> t

(** {1 Error Raising} *)

(** [lexer_error loc msg] raises a lexer error. Never returns.

    @param loc Source location
    @param msg Error message
    @raise Error Always *)
val lexer_error : Location.t -> string -> 'a

(** [parser_error loc msg] raises a parser error. Never returns.

    @param loc Source location
    @param msg Error message
    @raise Error Always *)
val parser_error : Location.t -> string -> 'a

(** [type_error loc msg] raises a type error. Never returns.

    @param loc Source location
    @param msg Error message
    @raise Error Always *)
val type_error : Location.t -> string -> 'a

(** [internal_error msg] raises an internal compiler error. Never returns.

    Use this for conditions that should never occur. Includes no location
    since the error is in the compiler, not the source.

    @param msg Error message
    @raise Error Always *)
val internal_error : string -> 'a

(** {1 Error Reporting} *)

(** [report fmt err] formats an error for display.

    Output includes location, error category, message, and hints. *)
val report : Format.formatter -> t -> unit

(** [report_to_string err] formats an error as a string. *)
val report_to_string : t -> string

(** {1 Warnings} *)

(** Warning categories. *)
type warning =
  | NonExhaustiveMatch of string  (** Missing pattern cases *)
  | RedundantPattern              (** Unreachable pattern *)

(** A warning with its source location. *)
type warning_info = {
  warning : warning;
  warning_location : Location.t;
}

(** [emit_warning warn loc] records a warning for later reporting.

    Warnings are accumulated in a global list and can be retrieved
    with {!get_warnings}. *)
val emit_warning : warning -> Location.t -> unit

(** [get_warnings ()] returns all accumulated warnings. *)
val get_warnings : unit -> warning_info list

(** [clear_warnings ()] removes all accumulated warnings. *)
val clear_warnings : unit -> unit

(** [warning_to_string warn] returns a human-readable warning message. *)
val warning_to_string : warning -> string

(** [report_warning fmt info] formats a warning for display. *)
val report_warning : Format.formatter -> warning_info -> unit

(** [report_warning_to_string info] formats a warning as a string. *)
val report_warning_to_string : warning_info -> string

(** [report_warning_as_error fmt info] formats a warning as an error. *)
val report_warning_as_error : Format.formatter -> warning_info -> unit

(** [report_warning_as_error_to_string info] formats a warning as an error string. *)
val report_warning_as_error_to_string : warning_info -> string

(** [warning_code info] returns the error code for a warning. *)
val warning_code : warning_info -> Error_code.t

(** {1 Enhanced Diagnostic System}

    The diagnostic system provides rich, structured error messages with:
    - Error codes for programmatic handling
    - Multiple labeled source spans
    - Suggested fixes with applicability levels
    - Additional notes and context

    This system is used by the new CLI renderer and JSON output format. *)

(** Severity level of a diagnostic. *)
type severity =
  | Error       (** Compilation failure *)
  | Warning     (** Potential issue, compilation continues *)
  | Info        (** Informational message *)
  | Hint        (** Style suggestion *)

(** How safe is it to automatically apply a suggestion? *)
type applicability =
  | MachineApplicable  (** Safe to auto-apply without review *)
  | MaybeIncorrect     (** Likely correct but should be reviewed *)
  | HasPlaceholders    (** Contains [...] requiring user input *)
  | Unspecified        (** Informational only, not applicable *)

(** A suggested fix for a diagnostic. *)
type suggestion = {
  suggestion_message : string;       (** Description of the fix *)
  replacement : string;              (** Text to replace the span with *)
  suggestion_span : Location.t;      (** Where to apply the replacement *)
  applicability : applicability;     (** How safe is auto-apply *)
}

(** A labeled source span within a diagnostic. *)
type label = {
  label_span : Location.t;           (** Source location *)
  label_message : string option;     (** Optional annotation text *)
  is_primary : bool;                 (** Primary vs secondary annotation *)
}

(** A rich diagnostic message. *)
type diagnostic = {
  severity : severity;               (** Error, Warning, Info, or Hint *)
  code : Error_code.t option;        (** Error/warning code (e.g., E0001) *)
  message : string;                  (** Main diagnostic message *)
  labels : label list;               (** Annotated source spans *)
  notes : string list;               (** Additional explanatory text *)
  suggestions : suggestion list;     (** Suggested fixes *)
}

(** {2 Diagnostic Creation} *)

(** [make_diagnostic ~severity ~message ?code ?labels ?notes ?suggestions ()]
    creates a new diagnostic. *)
val make_diagnostic :
  severity:severity ->
  message:string ->
  ?code:Error_code.t ->
  ?labels:label list ->
  ?notes:string list ->
  ?suggestions:suggestion list ->
  unit ->
  diagnostic

(** [diagnostic_of_error err] converts a legacy error to a diagnostic. *)
val diagnostic_of_error : t -> diagnostic

(** [diagnostic_of_warning info] converts a legacy warning to a diagnostic. *)
val diagnostic_of_warning : warning_info -> diagnostic

(** {2 Diagnostic Builders}

    Fluent API for building diagnostics incrementally. *)

(** [error ?code message] creates an error diagnostic. *)
val error : ?code:Error_code.t -> string -> diagnostic

(** [warning ?code message] creates a warning diagnostic. *)
val warning : ?code:Error_code.t -> string -> diagnostic

(** [with_label ~span ?message ?primary diag] adds a label to a diagnostic.
    If [primary] is not specified, the first label is primary. *)
val with_label :
  span:Location.t ->
  ?message:string ->
  ?primary:bool ->
  diagnostic ->
  diagnostic

(** [with_primary_label ~span ?message diag] adds a primary label. *)
val with_primary_label :
  span:Location.t ->
  ?message:string ->
  diagnostic ->
  diagnostic

(** [with_secondary_label ~span ?message diag] adds a secondary label. *)
val with_secondary_label :
  span:Location.t ->
  ?message:string ->
  diagnostic ->
  diagnostic

(** [with_note note diag] adds a note to a diagnostic. *)
val with_note : string -> diagnostic -> diagnostic

(** [with_suggestion ~span ~message ~replacement ?applicability diag]
    adds a fix suggestion to a diagnostic. *)
val with_suggestion :
  span:Location.t ->
  message:string ->
  replacement:string ->
  ?applicability:applicability ->
  diagnostic ->
  diagnostic
