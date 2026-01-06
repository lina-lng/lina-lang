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
