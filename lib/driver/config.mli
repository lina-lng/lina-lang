(** Project configuration from lina.toml.

    Parses and manages project configuration files. *)

(** {1 Configuration Types} *)

(** Package metadata. *)
type package = {
  name : string;
  version : string;
  edition : string;
}

(** Build configuration. *)
type build = {
  target : string;          (** Lua target: lua51, lua52, lua53, luajit *)
  source_dir : string;      (** Source directory, default "src" *)
  output_dir : string;      (** Output directory, default "_build" *)
}

(** Warning preset: strict (default) or relaxed. *)
type preset = Strict | Relaxed

(** Path-specific warning override. *)
type path_override = {
  path_pattern : string;
      (** Glob pattern like "test/**" or "generated/*" *)
  path_preset : preset option;
      (** Optional preset override for matching files *)
  path_overrides : (string * Common.Warning_config.level) list;
      (** Per-warning level overrides for matching files *)
}

(** Warning configuration from config file. *)
type warnings = {
  preset : preset;
      (** Base preset. Strict = unused code is error. Relaxed = unused code is warning. *)
  overrides : (string * Common.Warning_config.level) list;
      (** Per-warning level overrides. *)
  by_path : path_override list;
      (** Path-specific overrides, e.g. relax warnings for test files. *)
}

(** Complete project configuration. *)
type t = {
  package : package;
  build : build;
  warnings : warnings;
}

(** {1 Loading Configuration} *)

(** [load_file path] loads configuration from a TOML file.
    Returns [Error msg] if the file cannot be read or parsed. *)
val load_file : string -> (t, string) result

(** [load_project_config root] loads lina.toml from the project root.
    Returns [Error msg] if no configuration file is found or parsing fails. *)
val load_project_config : string -> (t, string) result

(** {1 Defaults} *)

(** [default_config name] returns a default configuration for a project. *)
val default_config : string -> t

(** [default_build] returns the default build configuration. *)
val default_build : build

(** [default_warnings] returns the default warning configuration. *)
val default_warnings : warnings

(** {1 Warning Integration} *)

(** [to_warning_config warnings] converts config warnings to Warning_config.t *)
val to_warning_config : warnings -> Common.Warning_config.t

(** [warning_config_for_file warnings file_path] returns warning config for a
    specific file, applying any path-specific overrides that match. *)
val warning_config_for_file : warnings -> string -> Common.Warning_config.t

(** [glob_match pattern path] tests if [path] matches the glob [pattern].
    Supports [*] for single component and [**] for any path suffix. *)
val glob_match : string -> string -> bool
