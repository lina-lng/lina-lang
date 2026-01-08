(** Error accumulation for LSP support. *)

open Common

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
  location : Location.t;
  kind : error_kind;
  hints : string list;
}
[@@deriving show]

(** Error accumulator type. *)
type t = { mutable errors : error list }

(** Create a new empty error accumulator. *)
let create () = { errors = [] }

(** Add an error to the accumulator. *)
let add_error acc ~location ~kind ~hints =
  acc.errors <- { location; kind; hints } :: acc.errors

(** Add a simple type error. *)
let add_type_error acc loc msg = add_error acc ~location:loc ~kind:(TypeError msg) ~hints:[]

(** Add an unbound variable error. *)
let add_unbound_variable acc loc name =
  add_error acc ~location:loc ~kind:(UnboundVariable name) ~hints:[]

(** Add an unbound type error. *)
let add_unbound_type acc loc name =
  add_error acc ~location:loc ~kind:(UnboundType name) ~hints:[]

(** Add an unbound constructor error. *)
let add_unbound_constructor acc loc name =
  add_error acc ~location:loc ~kind:(UnboundConstructor name) ~hints:[]

(** Add an unbound module error. *)
let add_unbound_module acc loc name =
  add_error acc ~location:loc ~kind:(UnboundModule name) ~hints:[]

(** Add a unification error with expected/actual types. *)
let add_unification_error acc loc ~expected ~actual msg =
  add_error acc ~location:loc
    ~kind:(UnificationError { expected; actual; message = msg })
    ~hints:[]

(** Add a pattern-related error. *)
let add_pattern_error acc loc msg =
  add_error acc ~location:loc ~kind:(PatternError msg) ~hints:[]

(** Add a module-related error. *)
let add_module_error acc loc msg =
  add_error acc ~location:loc ~kind:(ModuleError msg) ~hints:[]

(** Check if any errors were accumulated. *)
let has_errors acc = acc.errors <> []

(** Get the number of accumulated errors. *)
let error_count acc = List.length acc.errors

(** Get all accumulated errors in order. *)
let get_errors acc = List.rev acc.errors

(** Clear all accumulated errors. *)
let clear acc = acc.errors <- []

(** Get human-readable error message. *)
let error_message err =
  match err.kind with
  | TypeError msg -> msg
  | UnboundVariable name -> Printf.sprintf "Unbound variable: %s" name
  | UnboundType name -> Printf.sprintf "Unbound type: %s" name
  | UnboundConstructor name -> Printf.sprintf "Unbound constructor: %s" name
  | UnboundModule name -> Printf.sprintf "Unbound module: %s" name
  | UnificationError { message; _ } -> message
  | PatternError msg -> msg
  | ModuleError msg -> msg

(** Format an error for display. *)
let format_error fmt err =
  let msg = error_message err in
  Format.fprintf fmt "File \"%s\", line %d, characters %d-%d:@\n"
    err.location.start_pos.filename err.location.start_pos.line
    err.location.start_pos.column err.location.end_pos.column;
  Format.fprintf fmt "Error: %s@\n" msg;
  List.iter (fun hint -> Format.fprintf fmt "Hint: %s@\n" hint) err.hints
