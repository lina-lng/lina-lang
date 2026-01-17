(** Error and warning codes for the Lina compiler.

    This module defines unique codes for all compiler diagnostics, enabling:
    - Programmatic error handling and filtering
    - The [--explain] feature for detailed error documentation
    - Machine-readable output formats (JSON, SARIF)
    - Warning configuration (enable/disable/deny specific codes)

    {2 Code Format}

    - Error codes: [E0001] through [E9999]
    - Warning codes: [W0001] through [W9999]

    {2 Usage}

    {[
      (* Check if a code is an error *)
      if Error_code.is_error code then ...

      (* Get string representation *)
      let code_str = Error_code.to_string Error_code.e_type_mismatch
      (* Returns "E0001" *)
    ]} *)

(** The type of error/warning codes. *)
type t

(** {1 Error Codes (E0001-E9999)} *)

(** Type mismatch between expected and actual types. *)
val e_type_mismatch : t

(** Reference to an undefined value or variable. *)
val e_unbound_value : t

(** Reference to an undefined type. *)
val e_unbound_type : t

(** Reference to an undefined module. *)
val e_unbound_module : t

(** Reference to an undefined constructor. *)
val e_unbound_constructor : t

(** Pattern matching does not cover all cases. *)
val e_pattern_not_exhaustive : t

(** Pattern will never be matched (dead code). *)
val e_redundant_pattern : t

(** Infinite type detected (occurs check failure). *)
val e_occurs_check : t

(** Wrong number of arguments to function or constructor. *)
val e_arity_mismatch : t

(** Module does not match its signature. *)
val e_signature_mismatch : t

(** Syntax error in source code. *)
val e_syntax_error : t

(** Invalid token or character in source. *)
val e_lexer_error : t

(** Reference to an undefined module type. *)
val e_unbound_module_type : t

(** Reference to an undefined record field. *)
val e_unbound_field : t

(** Duplicate definition in the same scope. *)
val e_duplicate_definition : t

(** Recursive type without indirection. *)
val e_recursive_type : t

(** {1 Warning Codes (W0001-W9999)} *)

(** {2 Core Warnings} *)

(** Variable is defined but never used. *)
val w_unused_variable : t

(** Pattern matching may not cover all cases. *)
val w_non_exhaustive : t

(** Pattern will never be matched. *)
val w_redundant_pattern : t

(** Variable shadows an existing binding. *)
val w_shadowing : t

(** {2 Unused Code Detection}

    These warnings detect unused code. In strict mode, they become errors. *)

(** Function is defined but never called. *)
val w_unused_function : t

(** Function parameter is never used. *)
val w_unused_parameter : t

(** Module is imported but none of its bindings are used. *)
val w_unused_module : t

(** [open] statement imports no bindings that are used. *)
val w_unused_open : t

(** Type is defined but never referenced. *)
val w_unused_type : t

(** Variant constructor is defined but never used. *)
val w_unused_constructor : t

(** Record field is defined but never accessed. *)
val w_unused_field : t

(** [rec] flag is unnecessary (function does not call itself). *)
val w_unused_rec : t

(** Code is unreachable (after a diverging expression). *)
val w_dead_code : t

(** {2 Additional Analysis Warnings} *)

(** Weak type variable (cannot be generalized). *)
val w_weak_type_variable : t

(** Use of a deprecated binding. *)
val w_deprecated : t

(** Function has high cyclomatic complexity. *)
val w_complexity : t

(** {1 Strict Mode} *)

(** Codes that should become errors in strict mode.
    This includes all unused code detection warnings. *)
val strict_mode_codes : t list

(** [is_strict_mode_code code] returns [true] if the code should be an error
    in strict mode. *)
val is_strict_mode_code : t -> bool

(** {1 Code Operations} *)

(** [to_string code] returns the string representation (e.g., "E0001"). *)
val to_string : t -> string

(** [to_int code] returns the numeric part of the code. *)
val to_int : t -> int

(** [is_error code] returns [true] if the code represents an error. *)
val is_error : t -> bool

(** [is_warning code] returns [true] if the code represents a warning. *)
val is_warning : t -> bool

(** [of_string s] parses a code from its string representation.
    Returns [None] if the string is not a valid code. *)
val of_string : string -> t option

(** [description code] returns a short description of the error/warning. *)
val description : t -> string

(** [all_codes ()] returns a list of all defined codes. *)
val all_codes : unit -> t list

(** [pp fmt code] pretty-prints the code. *)
val pp : Format.formatter -> t -> unit

(** [compare a b] compares two codes. *)
val compare : t -> t -> int

(** [equal a b] tests equality of two codes. *)
val equal : t -> t -> bool
