(** Error accumulation for LSP support.

    This module provides infrastructure for accumulating multiple errors
    during type inference, rather than stopping at the first error.
    This is essential for LSP diagnostics where we want to show all
    errors to the user simultaneously.

    {2 Usage}

    {[
      let acc = Error_accumulator.create () in
      (* During inference, instead of raising errors: *)
      Error_accumulator.add_type_error acc loc "Type mismatch";
      (* At the end, check for errors: *)
      if Error_accumulator.has_errors acc then
        let errors = Error_accumulator.get_errors acc in
        (* Report all errors *)
    ]} *)

(** {1 Error Types} *)

(** Kind of accumulated error. *)
type error_kind =
  | TypeError of string
  | UnboundVariable of string
  | UnboundType of string
  | UnboundConstructor of string
  | UnboundModule of string
  | UnificationError of {
      expected : Types.type_expression;
      actual : Types.type_expression;
      message : string;
    }
  | PatternError of string
  | ModuleError of string
[@@deriving show]

(** An accumulated error with location and hints. *)
type error = {
  location : Common.Location.t;
  kind : error_kind;
  hints : string list;
}
[@@deriving show]

(** {1 Accumulator} *)

(** Error accumulator type. *)
type t

(** Create a new empty error accumulator. *)
val create : unit -> t

(** {1 Adding Errors} *)

(** [add_error acc ~location ~kind ~hints] adds an error to the accumulator. *)
val add_error :
  t -> location:Common.Location.t -> kind:error_kind -> hints:string list -> unit

(** [add_type_error acc loc msg] adds a simple type error. *)
val add_type_error : t -> Common.Location.t -> string -> unit

(** [add_unbound_variable acc loc name] adds an unbound variable error. *)
val add_unbound_variable : t -> Common.Location.t -> string -> unit

(** [add_unbound_type acc loc name] adds an unbound type error. *)
val add_unbound_type : t -> Common.Location.t -> string -> unit

(** [add_unbound_constructor acc loc name] adds an unbound constructor error. *)
val add_unbound_constructor : t -> Common.Location.t -> string -> unit

(** [add_unbound_module acc loc name] adds an unbound module error. *)
val add_unbound_module : t -> Common.Location.t -> string -> unit

(** [add_unification_error acc loc ~expected ~actual msg] adds a unification error. *)
val add_unification_error :
  t ->
  Common.Location.t ->
  expected:Types.type_expression ->
  actual:Types.type_expression ->
  string ->
  unit

(** [add_pattern_error acc loc msg] adds a pattern-related error. *)
val add_pattern_error : t -> Common.Location.t -> string -> unit

(** [add_module_error acc loc msg] adds a module-related error. *)
val add_module_error : t -> Common.Location.t -> string -> unit

(** {1 Querying Errors} *)

(** [has_errors acc] returns true if any errors were accumulated. *)
val has_errors : t -> bool

(** [error_count acc] returns the number of accumulated errors. *)
val error_count : t -> int

(** [get_errors acc] returns all accumulated errors in order. *)
val get_errors : t -> error list

(** [clear acc] removes all accumulated errors. *)
val clear : t -> unit

(** {1 Error Formatting} *)

(** [error_message err] returns a human-readable error message. *)
val error_message : error -> string

(** [format_error fmt err] formats an error for display. *)
val format_error : Format.formatter -> error -> unit
