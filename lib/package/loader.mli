(** Package loader for the typing environment.

    Loads package bindings from .lina files into the typing environment,
    enabling type-safe usage of LuaRocks packages. *)

(** {1 Loading} *)

(** Result of loading packages. *)
type load_result = {
  env : Typing.Environment.t;
    (** Extended environment with package modules *)
  lua_prelude : string;
    (** Lua code to set up package paths and requires *)
}

(** [load_packages ~root base_env] loads all packages from .lina/bindings/
    into the typing environment.

    @param root Project root directory
    @param base_env The base environment (typically with stdlib)
    @return [Ok result] with extended environment and Lua prelude,
            or [Error msg] if loading fails *)
val load_packages :
  root:string ->
  Typing.Environment.t ->
  (load_result, string) result

(** [load_single_binding ~root base_env filename] loads a single binding file.

    @param root Project root directory
    @param base_env The current environment
    @param filename Name of the binding file (e.g., "Socket.lina")
    @return [Ok env] with the module added, or [Error msg] *)
val load_single_binding :
  root:string ->
  Typing.Environment.t ->
  string ->
  (Typing.Environment.t, string) result

(** {1 Utilities} *)

(** [binding_files root] returns the list of .lina files in .lina/bindings/. *)
val binding_files : string -> string list

(** [generate_lua_prelude root] generates Lua code to set up package paths. *)
val generate_lua_prelude : string -> string
