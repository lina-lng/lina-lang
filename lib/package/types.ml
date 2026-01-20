(** Shared types for the package management system. *)

type luarocks_dependency = {
  name : string;
  version_constraint : string;
  optional : bool;
}
[@@deriving show, eq]

type lina_dependency = {
  lina_name : string;
  lina_path : string;
  lina_optional : bool;
}
[@@deriving show, eq]

type dependencies = {
  luarocks : luarocks_dependency list;
  dev_luarocks : luarocks_dependency list;
  lina : lina_dependency list;
  dev_lina : lina_dependency list;
}
[@@deriving show, eq]

let empty_dependencies = {
  luarocks = [];
  dev_luarocks = [];
  lina = [];
  dev_lina = [];
}

type resolved_package = {
  name : string;
  version : string;
  checksum : string;
}
[@@deriving show, eq]

type installed_info = {
  package_name : string;
  package_version : string;
  install_path : string;
  lua_modules : string list;
}
[@@deriving show, eq]

type lua_paths = {
  lua_path : string list;
  lua_cpath : string list;
}
[@@deriving show, eq]

let empty_lua_paths = {
  lua_path = [];
  lua_cpath = [];
}

let lina_dir_name = ".lina"
let lockfile_name = "lina.lock"
let config_name = "lina.toml"
let bindings_subdir = "bindings"
let lina_extension = ".lina"

let lina_dir root = Filename.concat root lina_dir_name

let bindings_dir root = Filename.concat (lina_dir root) bindings_subdir

let lockfile_path root = Filename.concat root lockfile_name

let config_path root = Filename.concat root config_name

let is_lina_file path = Filename.check_suffix path lina_extension

let module_name_of_file filename =
  Filename.chop_suffix (Filename.basename filename) lina_extension
