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
