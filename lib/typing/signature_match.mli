(** Signature matching for modules. *)

(** Set the module type lookup function for expanding ModTypeIdent.
    This should be set before calling match functions. *)
val set_module_type_lookup : (Types.path -> Module_types.module_type option) -> unit

(** Module strengthening: Makes abstract types concrete by binding them
    to their path. Call this when binding a module to ensure type equality. *)
val strengthen_signature : Types.path -> Module_types.signature -> Module_types.signature
val strengthen_module_type : Types.path -> Module_types.module_type -> Module_types.module_type

(** Path substitution: Replace occurrences of old_path with new_path in module types.
    Used when applying functors to substitute parameter paths with argument paths. *)
val substitute_path_in_module_type : Types.path -> Types.path -> Module_types.module_type -> Module_types.module_type

type match_error =
  | MissingValue of string
  | MissingType of string
  | MissingModule of string
  | TypeMismatch of string * Types.type_expression * Types.type_expression
  | ModuleTypeMismatch of string * string

type match_result = (unit, match_error) result

(** Check if an implementation signature satisfies a specification signature *)
val match_signature : Common.Location.t -> Module_types.signature -> Module_types.signature -> match_result

(** Check if an implementation module type matches a specification module type *)
val match_module_type : Common.Location.t -> Module_types.module_type -> Module_types.module_type -> match_result

(** Format a match error for display *)
val format_match_error : match_error -> string
