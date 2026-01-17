(** Warning configuration system.

    Allows users to configure warning behavior via command-line flags or
    configuration files. Warnings can be:
    - [Allow]: Silently ignored
    - [Warn]: Displayed as warnings (default)
    - [Deny]: Displayed as errors, compilation continues but exits with failure
    - [Forbid]: Treated as hard errors that stop compilation immediately

    {2 Command-Line Syntax}

    {[
      -W +all          # Enable all warnings
      -W -unused       # Disable unused variable warning
      -W unused=deny   # Treat unused as error
      --warn-error +a  # All warnings as errors
    ]} *)

(** Warning level configuration. *)
type level =
  | Allow   (** Silently ignore the warning *)
  | Warn    (** Display as warning (default) *)
  | Deny    (** Display as error, continue but exit with failure code *)
  | Forbid  (** Hard error, stop compilation immediately *)

(** Warning configuration state. *)
type t

(** [default] returns the default warning configuration.

    {b Strict mode is the default}: unused code detection warnings
    (unused variables, functions, parameters, modules, types, constructors,
    fields, dead code) are treated as errors. This ensures code quality
    for production and safety-critical systems.

    Use [relaxed] or configure [preset = "relaxed"] in lina.toml to
    treat these as warnings instead. *)
val default : t

(** [set_level config code level] returns a new configuration with the
    given warning code set to the specified level. *)
val set_level : t -> Error_code.t -> level -> t

(** [get_level config code] returns the current level for a warning code. *)
val get_level : t -> Error_code.t -> level

(** [enable_all config] enables all warnings (sets to [Warn]). *)
val enable_all : t -> t

(** [disable_all config] disables all warnings (sets to [Allow]). *)
val disable_all : t -> t

(** [warn_error_all config] treats all warnings as errors (sets to [Deny]). *)
val warn_error_all : t -> t

(** [parse_spec config spec] parses a warning specification and returns
    an updated configuration. Returns [Error msg] on invalid syntax.

    Supported formats:
    - [+name] or [name]: Enable warning
    - [-name]: Disable warning
    - [name=level]: Set to specific level (allow/warn/deny/forbid)
    - [+all]: Enable all
    - [-all]: Disable all *)
val parse_spec : t -> string -> (t, string) result

(** [parse_specs config specs] parses multiple warning specifications. *)
val parse_specs : t -> string list -> (t, string) result

(** [should_report config code] returns [true] if the warning should be
    displayed (level is [Warn], [Deny], or [Forbid]). *)
val should_report : t -> Error_code.t -> bool

(** [is_error config code] returns [true] if the warning should be treated
    as an error (level is [Deny] or [Forbid]). *)
val is_error : t -> Error_code.t -> bool

(** [is_fatal config code] returns [true] if the warning should stop
    compilation (level is [Forbid]). *)
val is_fatal : t -> Error_code.t -> bool

(** [severity_for config code] returns [Error] if the code is configured
    as an error (Deny/Forbid level), otherwise [Warning]. *)
val severity_for : t -> Error_code.t -> Compiler_error.severity

(** [level_to_string level] returns the string representation. *)
val level_to_string : level -> string

(** [level_of_string s] parses a level from string. *)
val level_of_string : string -> level option

(** {1 Relaxed Mode}

    The default configuration is strict (unused code = error).
    Use relaxed mode to opt-out and treat unused code as warnings only. *)

(** [relaxed] is a configuration where strict-mode codes (unused detection,
    dead code, etc.) are set to [Warn] instead of [Deny]. *)
val relaxed : t

(** [apply_relaxed config] returns a configuration where strict-mode codes
    are set to [Warn], preserving other settings. *)
val apply_relaxed : t -> t
