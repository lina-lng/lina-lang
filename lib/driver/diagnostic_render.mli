(** Diagnostic rendering for CLI output.

    This module renders compiler diagnostics in various formats:
    - Human-readable with ANSI colors (default)
    - Short one-line format
    - JSON for machine consumption

    The human-readable format shows error location and code context:

    {v
    error[E0001] --> src/main.lina:23:15

    22 |   let id = get_user_id()
    23 |   process_user(id, "admin")
       |                    ^^^^^^^
       |                    expected `Role`, found `string`

    note: To convert a string to a Role, use `Role.of_string value`
    v} *)

(** {1 Output Format} *)

(** The output format for diagnostics. *)
type format =
  | Human   (** Colored, formatted for terminal *)
  | Short   (** One-line per diagnostic *)
  | Json    (** Machine-readable JSON *)

(** {1 Terminal Configuration} *)

(** When to use colors in output. *)
type color_choice =
  | Auto    (** Detect TTY and NO_COLOR/FORCE_COLOR *)
  | Always  (** Always use colors *)
  | Never   (** Never use colors *)

(** Terminal capabilities for rendering. *)
type terminal_config = {
  use_color : bool;  (** Whether to emit ANSI color codes *)
  width : int;       (** Terminal width for wrapping *)
}

(** [detect_terminal color_choice] detects terminal capabilities.
    Respects [NO_COLOR] and [FORCE_COLOR] environment variables. *)
val detect_terminal : color_choice -> terminal_config

(** {1 Source File Management} *)

(** A cache of source file contents for displaying code snippets. *)
type source_cache

(** [create_source_cache ()] creates an empty source cache. *)
val create_source_cache : unit -> source_cache

(** [add_source cache ~path ~content] adds a file's content to the cache. *)
val add_source : source_cache -> path:string -> content:string -> unit

(** [get_line cache ~path ~line] returns the content of a specific line.
    Returns [None] if the file or line is not available. *)
val get_line : source_cache -> path:string -> line:int -> string option

(** [load_source_file cache path] loads a file's content from disk into the cache.
    Returns [Ok ()] on success, [Error msg] on failure. *)
val load_source_file : source_cache -> string -> (unit, string) result

(** {1 Rendering} *)

(** [render_diagnostic ~format ~terminal ~sources diag] renders a single
    diagnostic to a string. *)
val render_diagnostic :
  format:format ->
  terminal:terminal_config ->
  sources:source_cache ->
  Common.Compiler_error.diagnostic ->
  string

(** [render_diagnostics ~format ~terminal ~sources diags] renders multiple
    diagnostics with a summary. *)
val render_diagnostics :
  format:format ->
  terminal:terminal_config ->
  sources:source_cache ->
  Common.Compiler_error.diagnostic list ->
  string

(** {1 Convenience Functions} *)

(** [render_human ~sources diag] renders a diagnostic in human format with
    auto-detected terminal settings. *)
val render_human :
  sources:source_cache ->
  Common.Compiler_error.diagnostic ->
  string

(** [render_json diag] renders a diagnostic as a JSON string. *)
val render_json : Common.Compiler_error.diagnostic -> string

(** [render_json_list diags] renders multiple diagnostics as a JSON array. *)
val render_json_list : Common.Compiler_error.diagnostic list -> string

(** {1 Legacy Error Rendering}

    These functions provide the new rendering for legacy error types. *)

(** [render_error ~sources err] renders a legacy compiler error using the
    new diagnostic renderer. *)
val render_error :
  sources:source_cache ->
  Common.Compiler_error.t ->
  string

(** [render_warning ~sources warn] renders a legacy warning using the
    new diagnostic renderer. *)
val render_warning :
  sources:source_cache ->
  Common.Compiler_error.warning_info ->
  string
