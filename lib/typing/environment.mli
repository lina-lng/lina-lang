type t

val empty : t
val initial : t

val add_value : string -> Common.Identifier.t -> Types.type_scheme -> t -> t
val find_value : string -> t -> (Common.Identifier.t * Types.type_scheme) option

val add_type : string -> Types.type_declaration -> t -> t
val find_type : string -> t -> Types.type_declaration option

val add_constructor : string -> Types.constructor_info -> t -> t
val find_constructor : string -> t -> Types.constructor_info option

(** Look up all constructors for a variant type by type name.
    Returns None for abstract types or non-existent types. *)
val find_type_constructors : string -> t -> Types.constructor_info list option

(** Module operations *)

val add_module : string -> Module_types.module_binding -> t -> t
val find_module : string -> t -> Module_types.module_binding option

val add_module_type : string -> Module_types.module_type option -> t -> t
val find_module_type : string -> t -> Module_types.module_type option option

(** Open a module: bring its signature contents into scope.
    Returns (updated_env, list of (value_name, identifier) pairs for opened values) *)
val open_module : Module_types.signature -> t -> t * (string * Common.Identifier.t) list
