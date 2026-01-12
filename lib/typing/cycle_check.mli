(** Type alias cycle detection with contractiveness checking.

    This module implements proactive cycle detection for type definitions.
    It ensures that recursive type definitions are well-founded by checking
    that all cycles pass through a "contractive" position (data constructor
    or record field).

    {2 Background}

    A type definition is {b well-founded} if every recursive path passes
    through a contractive position. A position is contractive if it's guarded
    by a data constructor:

    {v
    contractive(T)     = true    if T is a variant constructor
    contractive({...}) = true    if T is a record
    contractive(A->B)  = false   arrows are NOT contractive
    contractive(C(A))  = false   type aliases are NOT contractive
    v}

    {2 Valid vs Invalid Recursive Types}

    {[
      type t = t                      (* INVALID: direct cycle *)
      type t = int -> t               (* INVALID: arrow doesn't guard *)
      type t = Cons of t              (* VALID: constructor guards *)
      type 'a list = Nil | Cons of 'a * 'a list  (* VALID *)
      type t = { next : t option }    (* VALID: option is a constructor *)
    ]}

    {2 References}

    - Pierce, B.C. "Types and Programming Languages" Chapter 20 (Recursive Types)
    - OCaml uses iso-recursive types which require contractiveness *)

(** {1 Error Types} *)

(** Errors detected during cycle checking. *)
type cycle_error =
  | DirectCycle of string * Common.Location.t
      (** [type t = t] - immediate self-reference *)
  | UnguardedCycle of string list * Common.Location.t
      (** [type t = int -> t] - recursive through non-contractive position *)
  | MutualCycle of string list * Common.Location.t
      (** [type t = u and u = t] - mutual recursion without guards *)

(** Exception raised when a cycle is detected. *)
exception Cycle_detected of cycle_error

(** {1 Cycle Detection} *)

(** [check_type_definition ~env ~loc name params kind manifest] checks a single
    type definition for invalid cycles.

    A cycle is valid only if it passes through a contractive position
    (variant constructor or record field).

    @param env The typing environment (for looking up type declarations)
    @param loc Source location for error messages
    @param name The name of the type being defined
    @param params The type parameters
    @param kind The kind of type declaration (variant, record, abstract)
    @param manifest The manifest type for aliases ([Some ty] for [type t = ty])
    @raise Cycle_detected if an invalid cycle is found *)
val check_type_definition :
  env:Environment.t ->
  loc:Common.Location.t ->
  string ->
  Types.type_variable list ->
  Types.type_declaration_kind ->
  Types.type_expression option ->
  unit

(** [check_mutual_recursion ~env ~loc definitions] checks mutually recursive
    type definitions for invalid cycles.

    All types in the group are checked together to detect cycles that span
    multiple type definitions.

    @param env The typing environment
    @param loc Source location for error messages
    @param definitions List of type declarations in the mutually recursive group
    @raise Cycle_detected if an invalid cycle is found *)
val check_mutual_recursion :
  env:Environment.t ->
  loc:Common.Location.t ->
  Types.type_declaration list ->
  unit

(** {1 Error Formatting} *)

(** [format_error err] returns a user-friendly error message. *)
val format_error : cycle_error -> string
