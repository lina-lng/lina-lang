(** FFI attribute validation and semantic analysis.

    This module converts parsed FFI attributes into validated semantic
    representations, checking for conflicts and arity requirements. *)

open Common

(** Error type for FFI validation failures. *)
type ffi_error =
  | ConflictingAttributes of string * string
    (** Two attributes that cannot be used together *)
  | MissingRequiredAttribute of string
    (** An attribute that must be present *)
  | InvalidPayload of string * string
    (** Attribute name and description of expected payload *)
  | ArityMismatch of string * int * int
    (** Attribute name, expected arity, actual arity *)
  | DuplicateAttribute of string
    (** Same attribute appears multiple times *)
  | PolymorphicExternal
    (** Warning: external has polymorphic type *)
[@@deriving show]

(** [error_message err] converts an error to a human-readable message. *)
val error_message : ffi_error -> string

(** [parse_attribute attr] parses a single attribute from AST form to semantic form.
    Returns [Ok None] for unknown attributes (allows future extension).
    Returns [Error e] if the attribute has invalid payload. *)
val parse_attribute : Parsing_ffi.Attributes.attribute -> (Types.ffi_attribute option, ffi_error) result

(** [parse_attributes attrs] parses all attributes from AST form.
    Stops on first error. *)
val parse_attributes : Parsing_ffi.Attributes.attribute list -> (Types.ffi_attribute list, ffi_error) result

(** [check_duplicates attrs] checks for duplicate attributes.
    Each attribute type should appear at most once. *)
val check_duplicates : Types.ffi_attribute list -> (unit, ffi_error) result

(** [check_conflicts attrs] checks for conflicting attribute combinations.
    For example, [@module] and [@val] cannot be used together. *)
val check_conflicts : Types.ffi_attribute list -> (unit, ffi_error) result

(** [determine_kind attrs] determines the FFI kind from attributes.
    Defaults to global call if no specific kind is specified. *)
val determine_kind : Types.ffi_attribute list -> Types.ffi_kind

(** [check_arity kind arity] checks that the function arity satisfies
    the constraints for the given FFI kind. For example, [@get] requires
    exactly 1 argument (the receiver). *)
val check_arity : Types.ffi_kind -> int -> (unit, ffi_error) result

(** [get_lua_name attrs primitive] returns the Lua name to use.
    Uses [@as("name")] if present, otherwise falls back to [primitive]. *)
val get_lua_name : Types.ffi_attribute list -> string -> string

(** [is_return_nullable attrs] returns true if [@return(nullable)] is present. *)
val is_return_nullable : Types.ffi_attribute list -> bool

(** [is_variadic attrs] returns true if [@variadic] is present. *)
val is_variadic : Types.ffi_attribute list -> bool

(** [build_ffi_spec ~attrs ~primitive ~arity ~unit_params ~location] builds a complete
    FFI specification from parsed attributes.

    Performs all validation:
    - Parses attributes to semantic form
    - Checks for duplicates
    - Checks for conflicts
    - Validates arity constraints

    @param attrs The parsed attributes from the AST
    @param primitive The primitive string (Lua function name)
    @param arity The number of arguments (computed from the type)
    @param unit_params Which parameters are unit type (should not be passed to Lua)
    @param location Source location for error reporting
    @return The validated FFI spec or an error *)
val build_ffi_spec :
  attrs:Parsing_ffi.Attributes.attribute list ->
  primitive:string ->
  arity:int ->
  unit_params:bool list ->
  location:Location.t ->
  (Types.ffi_spec, ffi_error) result
