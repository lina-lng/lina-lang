(** Package installer.

    Orchestrates the installation of LuaRocks packages into the project's
    .lina directory and generates the lockfile. *)

(** {1 Types} *)

(** Installation result. *)
type install_result = {
  lockfile : Lockfile.t;
  installed_count : int;
  skipped_count : int;
}
[@@deriving show]

(** Installation error. *)
type error =
  | LuarocksNotAvailable
  | InstallFailed of string * string  (** package name, error message *)
  | DirectoryError of string
[@@deriving show]

(** {1 Installation} *)

(** [install_all ~root dependencies] installs all dependencies to the project.

    Creates the .lina directory if needed, installs packages via LuaRocks,
    and generates/updates the lockfile.

    @param root Project root directory
    @param dependencies Dependencies from lina.toml
    @param include_dev Whether to include dev dependencies
    @return [Ok result] with lockfile and counts, or [Error e] *)
val install_all :
  root:string ->
  dependencies:Types.dependencies ->
  ?include_dev:bool ->
  unit ->
  (install_result, error) result

(** [ensure_installed ~root dependencies] checks if packages are installed
    and installs any missing ones.

    @param root Project root directory
    @param dependencies Dependencies from lina.toml
    @return [Ok lockfile] or [Error e] *)
val ensure_installed :
  root:string ->
  dependencies:Types.dependencies ->
  unit ->
  (Lockfile.t, error) result

(** {1 Directory Management} *)

(** [lina_dir root] returns the path to the .lina directory. *)
val lina_dir : string -> string

(** [bindings_dir root] returns the path to .lina/bindings/. *)
val bindings_dir : string -> string

(** [lockfile_path root] returns the path to lina.lock. *)
val lockfile_path : string -> string

(** [ensure_dirs root] creates the .lina directory structure. *)
val ensure_dirs : string -> (unit, error) result

(** {1 Lua Paths} *)

(** [get_lua_paths root] returns the Lua paths for the project. *)
val get_lua_paths : string -> Types.lua_paths
