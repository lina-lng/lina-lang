(** Shared types for the package management system.

    Defines the core data structures for dependencies, resolved packages,
    and installation state. *)

(** {1 Dependency Specification} *)

(** A LuaRocks dependency as specified in lina.toml. *)
type luarocks_dependency = {
  name : string;                      (** Package name on LuaRocks *)
  version_constraint : string;        (** Version constraint string (e.g., "^1.0") *)
  optional : bool;                    (** Whether the dependency is optional *)
}
[@@deriving show, eq]

(** A Lina project dependency as specified in lina.toml. *)
type lina_dependency = {
  lina_name : string;                 (** Module name to expose as *)
  lina_path : string;                 (** Relative or absolute path to project *)
  lina_optional : bool;               (** Whether the dependency is optional *)
}
[@@deriving show, eq]

(** All dependencies from the manifest. *)
type dependencies = {
  luarocks : luarocks_dependency list;
  dev_luarocks : luarocks_dependency list;
  lina : lina_dependency list;
  dev_lina : lina_dependency list;
}
[@@deriving show, eq]

(** Empty dependencies. *)
val empty_dependencies : dependencies

(** {1 Resolved Packages} *)

(** A resolved package with exact version and integrity information. *)
type resolved_package = {
  name : string;
  version : string;                   (** Exact version installed *)
  checksum : string;                  (** Integrity hash (MD5 for now) *)
}
[@@deriving show, eq]

(** {1 Installed Package Information} *)

(** Information about an installed LuaRocks package. *)
type installed_info = {
  package_name : string;
  package_version : string;
  install_path : string;              (** Path to installed files *)
  lua_modules : string list;          (** Lua module names provided *)
}
[@@deriving show, eq]

(** {1 Lua Paths} *)

(** Lua search paths for package loading. *)
type lua_paths = {
  lua_path : string list;             (** Paths for package.path *)
  lua_cpath : string list;            (** Paths for package.cpath *)
}
[@@deriving show, eq]

(** Empty Lua paths. *)
val empty_lua_paths : lua_paths

(** {1 Project Paths}

    Standard paths and naming conventions for Lina projects. These constants
    ensure consistency across the package management system. *)

(** Hidden directory name for package artifacts. *)
val lina_dir_name : string

(** Lockfile filename. *)
val lockfile_name : string

(** Configuration filename. *)
val config_name : string

(** Binding files subdirectory name. *)
val bindings_subdir : string

(** Lina source file extension. *)
val lina_extension : string

(** [lina_dir root] returns the path to the .lina directory.

    @param root Project root directory
    @return Absolute path to the .lina directory *)
val lina_dir : string -> string

(** [bindings_dir root] returns the path to .lina/bindings/.

    This directory contains FFI binding files for LuaRocks packages.

    @param root Project root directory
    @return Absolute path to the bindings directory *)
val bindings_dir : string -> string

(** [lockfile_path root] returns the path to lina.lock.

    @param root Project root directory
    @return Absolute path to the lockfile *)
val lockfile_path : string -> string

(** [config_path root] returns the path to lina.toml.

    @param root Project root directory
    @return Absolute path to the config file *)
val config_path : string -> string

(** [is_lina_file path] checks if [path] has the .lina extension.

    @param path File path to check
    @return [true] if the file has a .lina extension *)
val is_lina_file : string -> bool

(** [module_name_of_file filename] extracts the module name from a .lina filename.

    Strips the directory path and .lina extension.
    E.g., "/path/to/Socket.lina" -> "Socket"

    @param filename Path to a .lina file
    @return Module name *)
val module_name_of_file : string -> string
