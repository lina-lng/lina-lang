(** Signature matching for modules. *)

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
