(** LSP type definitions and conversions.

    This module provides type conversions between Lina's internal types
    and LSP protocol types. LSP uses 0-indexed positions while Lina
    uses 1-indexed, so conversions handle this offset. *)

(** {1 Position Types} *)

(** LSP position (0-indexed line and character). *)
type position = {
  line : int;       (** Line number (0-indexed) *)
  character : int;  (** Character offset in line (0-indexed) *)
}
[@@deriving show, eq]

(** LSP range (start and end positions). *)
type range = {
  start_pos : position;  (** Inclusive start *)
  end_pos : position;    (** Exclusive end *)
}
[@@deriving show, eq]

(** {1 Diagnostic Types} *)

(** Diagnostic severity levels. *)
type diagnostic_severity =
  | Error
  | Warning
  | Information
  | Hint
[@@deriving show, eq]

(** A diagnostic message with location and severity. *)
type diagnostic = {
  range : range;
  severity : diagnostic_severity;
  message : string;
  code : string option;
  source : string option;
  related_information : related_diagnostic_info list;
}
[@@deriving show, eq]

(** Related diagnostic information with location in another file. *)
and related_diagnostic_info = {
  location_uri : string;
  location_range : range;
  related_message : string;
}
[@@deriving show, eq]

(** {1 Conversions} *)

(** [position_of_lina_position pos] converts a Lina position to LSP position.
    Lina uses 1-indexed, LSP uses 0-indexed. *)
val position_of_lina_position : Common.Location.position -> position

(** [range_of_location loc] converts a Lina location to LSP range. *)
val range_of_location : Common.Location.t -> range

(** [lina_position_of_position filename pos offset] creates a Lina position.
    Requires the byte offset to be provided separately. *)
val lina_position_of_position : string -> position -> int -> Common.Location.position

(** {1 Diagnostic Helpers} *)

(** [make_diagnostic ~severity ~message loc] creates a diagnostic at location. *)
val make_diagnostic :
  severity:diagnostic_severity ->
  message:string ->
  ?code:string ->
  ?hints:string list ->
  Common.Location.t ->
  diagnostic

(** [diagnostic_of_compiler_error err] converts a compiler error to diagnostic. *)
val diagnostic_of_compiler_error : Common.Compiler_error.t -> diagnostic

(** [severity_to_int sev] converts severity to LSP integer code. *)
val severity_to_int : diagnostic_severity -> int

(** [severity_of_int code] converts LSP integer code to severity. *)
val severity_of_int : int -> diagnostic_severity
