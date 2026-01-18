(** Human-readable type explanations.

    This module converts type expressions into English-like descriptions
    for error messages. It provides friendly explanations that help users
    understand type errors without needing to parse raw type syntax.

    Examples:
    - [int] -> "an integer"
    - [string -> int] -> "a function from string to integer"
    - [\{ x: int; y: int \}] -> "a record with fields x and y"
    - ['a option] -> "an optional value" *)

(** {1 Type Explanations} *)

val explain : Types.type_expression -> string
(** [explain ty] returns a human-readable description of the type.

    Uses indefinite articles ("a", "an") appropriately:
    - "an integer"
    - "a string"
    - "a function"

    For complex types, provides structural description:
    - "a tuple of 3 elements"
    - "a record with fields x, y, and z"
    - "a list of integers" *)

val explain_short : Types.type_expression -> string
(** [explain_short ty] returns a brief description without articles.

    Used for contexts where the article doesn't fit:
    - "integer" instead of "an integer"
    - "function" instead of "a function" *)

(** {1 Pretty Printing} *)

(** Context for consistent type variable naming across multiple types. *)
type pretty_context

val create_context : unit -> pretty_context
(** [create_context ()] creates a fresh pretty-printing context.

    Use a single context when printing multiple related types to ensure
    consistent variable naming (e.g., 'a in one type maps to 'a in another). *)

val pretty_type : Types.type_expression -> string
(** [pretty_type ty] formats the type with human-friendly variable names.

    Remaps internal variable IDs to readable names:
    - 't42 -> 'a
    - 't43 -> 'b
    - etc.

    Creates a fresh context for each call. Use [pretty_type_in_context]
    for consistent naming across multiple types. *)

val pretty_type_in_context : pretty_context -> Types.type_expression -> string
(** [pretty_type_in_context ctx ty] formats the type using the given context.

    Type variables encountered in previous calls with the same context
    will use the same names. *)

(** {1 Utilities} *)

val article_for : string -> string
(** [article_for word] returns the appropriate indefinite article.

    @return "an" if [word] starts with a vowel, "a" otherwise *)

val is_constructor_named : string -> Types.type_expression -> bool
(** [is_constructor_named name ty] checks if [ty] is a type constructor
    with the given name.

    Handles both [PathLocal] and [PathIdent] path variants, making it
    robust for checking well-known constructors like "option" and "list". *)

val builtin_machine_name : Types.type_expression -> string option
(** [builtin_machine_name ty] extracts the short machine name for builtin types.

    @return [Some "int"], [Some "float"], etc. for builtin types, [None] otherwise.
    Used for conversion hint lookup tables. *)

(** {1 Type Comparison} *)

val explain_mismatch :
  expected:Types.type_expression ->
  actual:Types.type_expression ->
  string * string * string option
(** [explain_mismatch ~expected ~actual] explains the difference between
    two mismatched types.

    @return [(expected_explanation, actual_explanation, hint_option)]

    The hint provides additional guidance when the mismatch has a common
    cause, such as:
    - Function vs its result (forgot to apply?)
    - Option vs its contents (need to unwrap?)
    - Numeric type mismatch (need conversion?) *)
