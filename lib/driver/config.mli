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

(** Warning configuration from config file. *)
type warnings = {
  default_level : Warning_config.level;
  overrides : (string * Warning_config.level) list;
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
val to_warning_config : warnings -> Warning_config.t
