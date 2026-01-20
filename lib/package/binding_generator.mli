(** FFI binding generator for LuaRocks packages.

    Generates typed Lina binding stubs for LuaRocks packages to enable
    type-safe usage of Lua libraries. *)

(** {1 Types} *)

(** Result of batch binding generation. *)
type generation_result = {
  generated : string list;
  failed : (string * string) list;
}
[@@deriving show]

(** {1 Generation} *)

(** [generate ~package_name ~output_dir ()] generates a binding stub for a package.

    If a built-in binding template exists for the package, it will be used.
    Otherwise, a minimal stub is generated.

    @param package_name Name of the LuaRocks package
    @param output_dir Directory to write the binding file
    @return [Ok path] with the path to the generated file, or [Error msg] *)
val generate : package_name:string -> output_dir:string -> unit -> (string, string) result

(** [generate_all ~lockfile ~output_dir ()] generates bindings for all packages
    in the lockfile. Reports both successful generations and failures.

    @param lockfile The lockfile with resolved packages
    @param output_dir Directory to write binding files
    @return Record with generated paths and any failures *)
val generate_all : lockfile:Lockfile.t -> output_dir:string -> unit -> generation_result

(** {1 Templates} *)

(** [has_template package_name] checks if a built-in template exists. *)
val has_template : string -> bool

(** [template_for package_name] returns the template if available. *)
val template_for : string -> string option

(** {1 Module Naming} *)

(** [binding_module_name package_name] converts a package name to a Lina module name.
    E.g., "luasocket" -> "Socket", "lpeg" -> "Lpeg" *)
val binding_module_name : string -> string

(** [binding_filename package_name] returns the binding filename.
    E.g., "luasocket" -> "Socket.lina" *)
val binding_filename : string -> string
