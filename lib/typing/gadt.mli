(** GADT (Generalized Algebraic Data Types) support.

    This module implements type equation extraction for GADT pattern matching.
    When matching a GADT constructor, type equations are extracted rather than
    doing global unification on rigid type variables.

    {2 Example}

    Given:
    {[
      type _ expr =
        | Int : int -> int expr
        | Bool : bool -> bool expr

      let eval (type a) (e : a expr) : a =
        match e with
        | Int n -> n      (* Here, a = int is extracted *)
        | Bool b -> b     (* Here, a = bool is extracted *)
    ]}

    When matching [Int n] against scrutinee of type [a expr] where [a] is a
    rigid locally abstract type, the equation [a = int] is extracted. This
    equation is applied locally within the branch, allowing [n] to have type
    [int] and the branch result to satisfy the return type [a].

    {2 Rigid Type Variables}

    Type variables created by [(type a)] patterns are marked as "rigid".
    These variables don't unify globally during pattern matching. Instead,
    equations are extracted and applied locally per branch. *)

open Types

(** {1 Type Equations} *)

(** A type equation extracted from GADT pattern matching.
    Maps a rigid type variable to its refined type within a branch. *)
type equation = {
  eq_variable : type_variable;  (** The rigid type variable *)
  eq_type : type_expression;    (** The type it equals in this branch *)
}

(** Result of GADT equation extraction. *)
type extraction_result = {
  equations : equation list;  (** Extracted type equations *)
  success : bool;             (** Whether the types could be matched *)
}

(** {1 Equation Extraction} *)

(** [has_rigid_variables ty] returns true if [ty] contains any rigid
    type variables. Used to detect when GADT handling is needed. *)
val has_rigid_variables : type_expression -> bool

(** [extract_equations scrutinee_type constructor_result_type] extracts
    GADT type equations by comparing the scrutinee type with the
    constructor's result type.

    Walks both types in parallel, extracting equations when a rigid type
    variable in the scrutinee is matched against a concrete type in the
    constructor result.

    @param scrutinee_type The type of the value being matched
    @param constructor_result_type The instantiated result type of the GADT constructor
    @return Extraction result with equations and success flag *)
val extract_equations : type_expression -> type_expression -> extraction_result

(** {1 Equation Application} *)

(** [apply_equations equations ty] applies type equations as substitutions.

    Replaces occurrences of rigid type variables with their equation types.
    Used when type-checking the body of a GADT match branch.

    @param equations List of type equations to apply
    @param ty The type to transform
    @return The type with substitutions applied *)
val apply_equations : equation list -> type_expression -> type_expression

(** [apply_equations_to_scheme equations scheme] applies equations to a
    type scheme's body, preserving quantified variables. *)
val apply_equations_to_scheme : equation list -> type_scheme -> type_scheme

(** {1 GADT Detection} *)

(** [needs_gadt_handling constructor_info scrutinee_type] checks if a
    constructor pattern needs GADT equation extraction.

    Returns true if the constructor is a GADT and the scrutinee contains
    rigid type variables (from locally abstract types).

    @param constructor_info The constructor being matched
    @param scrutinee_type The type of the value being matched
    @return true if GADT equation extraction should be performed *)
val needs_gadt_handling : constructor_info -> type_expression -> bool
