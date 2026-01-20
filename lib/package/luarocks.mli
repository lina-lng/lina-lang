(** LuaRocks CLI wrapper.

    Provides a safe interface to the luarocks command-line tool for
    installing, listing, and managing Lua packages. *)

(** {1 Error Types} *)

(** Errors that can occur during LuaRocks operations. *)
type error =
  | NotInstalled
      (** LuaRocks is not available on the system *)
  | CommandFailed of { command : string; exit_code : int; stderr : string }
      (** A luarocks command failed *)
  | ParseError of string
      (** Failed to parse luarocks output *)
[@@deriving show]

(** {1 Package Information} *)

(** Information about an installed package. *)
type package_info = {
  name : string;
  version : string;
  install_path : string;
}
[@@deriving show, eq]


(** {1 Availability} *)

(** [is_available ()] checks if luarocks is available on the system.
    Returns [true] if the luarocks command can be executed. *)
val is_available : unit -> bool

(** [version ()] returns the installed LuaRocks version if available. *)
val version : unit -> (string, error) result

(** {1 Package Operations} *)

(** [install ~name ?version ~tree ()] installs a package to the specified tree.

    @param name Package name on LuaRocks
    @param version Optional specific version to install
    @param tree Path to the installation tree (e.g., ".lina")
    @return [Ok ()] on success, [Error e] on failure *)
val install :
  name:string ->
  ?version:string ->
  tree:string ->
  unit ->
  (unit, error) result

(** [remove ~name ~tree ()] removes a package from the tree.

    @param name Package name to remove
    @param tree Path to the installation tree *)
val remove :
  name:string ->
  tree:string ->
  unit ->
  (unit, error) result

(** [list_installed ~tree ()] lists all packages installed in the tree.

    @param tree Path to the installation tree
    @return List of installed package info *)
val list_installed : tree:string -> (package_info list, error) result

(** [show ~name ~tree ()] gets information about an installed package.

    @param name Package name
    @param tree Path to the installation tree
    @return [Some info] if installed, [None] if not found *)
val show :
  name:string ->
  tree:string ->
  unit ->
  (package_info option, error) result

(** {1 Path Configuration} *)

(** [get_paths ~tree] returns the Lua search paths for packages
    installed in the given tree. *)
val get_paths : tree:string -> Types.lua_paths
