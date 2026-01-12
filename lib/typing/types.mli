(** Type representation for Lina's type system.

    This module defines the core type algebra used throughout the compiler:
    - Type variables with levels for let-polymorphism
    - Type constructors with paths for modules
    - Function types, tuples, and records
    - Row types for structural record typing
    - Type schemes for polymorphic values

    {2 Level-Based Generalization}

    Type variables carry a [level] field used for efficient generalization.
    When entering a [let], the level increases. Variables at higher levels
    than the current scope can be generalized (made polymorphic).

    Variables with [level = generic_level] are fully polymorphic.

    {2 Paths}

    Type constructors reference types through paths, which can be:
    - Built-in types: [int], [bool], [string], [float], [unit]
    - Local types: user-defined types in the current scope
    - Module paths: [M.t], [M.N.t], [F(A).t] *)

(** {1 Type Levels} *)

(** Type variable levels for generalization.
    Higher levels indicate more deeply nested scopes. *)
type level = int

(** The level for fully generalized (polymorphic) type variables. *)
val generic_level : level

(** [current_level ()] returns the current nesting level. *)
val current_level : unit -> level

(** [enter_level ()] increments the level when entering a let-binding. *)
val enter_level : unit -> unit

(** [leave_level ()] decrements the level when leaving a let-binding. *)
val leave_level : unit -> unit

(** [reset_level ()] resets the level to the initial value. *)
val reset_level : unit -> unit

(** [set_level level] sets the current level directly.
    Used for context migration during module-boundary compilation. *)
val set_level : level -> unit

(** [set_next_type_variable_id id] sets the next ID for fresh type variables.
    Used for context migration during module-boundary compilation. *)
val set_next_type_variable_id : int -> unit

(** {1 Type Expressions} *)

(** Type variables with mutable link for unification. *)
type type_variable = {
  id : int;                         (** Unique identifier *)
  mutable level : level;            (** Generalization level *)
  mutable link : type_expression option;  (** Union-find link *)
  mutable weak : bool;              (** True if value restriction blocks generalization *)
}

(** Core type expressions.

    The type algebra includes:
    - [TypeVariable]: unification variables
    - [TypeConstructor]: named types like [int], [option]
    - [TypeTuple]: product types [(a * b * c)]
    - [TypeArrow]: function types [a -> b]
    - [TypeRecord]: structural record types with row polymorphism
    - [TypeRowEmpty]: closed row (no more fields) *)
and type_expression =
  | TypeVariable of type_variable
  | TypeConstructor of path * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of type_expression * type_expression
  | TypeRecord of row
  | TypeRowEmpty

(** Row type for record fields.
    A row contains named fields and a "tail" that is either:
    - [TypeRowEmpty]: closed record, no other fields allowed
    - [TypeVariable]: open record, more fields possible *)
and row = {
  row_fields : (string * row_field) list;  (** Sorted by field name *)
  row_more : type_expression;              (** Row tail *)
}

(** Row field presence indicator. *)
and row_field =
  | RowFieldPresent of type_expression

(** {2 Type Paths} *)

(** Paths to types, supporting module system.

    Examples:
    - [PathBuiltin BuiltinInt]: the [int] type
    - [PathLocal "t"]: a locally-defined type [t]
    - [PathIdent id]: a module-bound type via identifier
    - [PathDot (PathLocal "M", "t")]: [M.t]
    - [PathApply (f_path, arg_path)]: [F(A).t] *)
and path =
  | PathBuiltin of builtin_type
  | PathLocal of string
  | PathIdent of Common.Identifier.t
  | PathDot of path * string
  | PathApply of path * path

(** Built-in primitive types. *)
and builtin_type =
  | BuiltinInt
  | BuiltinFloat
  | BuiltinString
  | BuiltinBool
  | BuiltinUnit
  | BuiltinRef  (** Mutable reference type *)

(** {1 Variance} *)

(** Variance of a type parameter.

    Variance describes how a type constructor relates to subtyping
    (or in ML, how it affects generalization under the value restriction):
    - [Covariant]: parameter appears in output positions only
    - [Contravariant]: parameter appears in input positions only
    - [Invariant]: parameter appears in both positions
    - [Bivariant]: parameter doesn't appear (phantom type) *)
type variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

val pp_variance : Format.formatter -> variance -> unit
val equal_variance : variance -> variance -> bool

(** {1 Type Variable Creation} *)

(** [new_type_variable ()] creates a fresh type variable at the current level. *)
val new_type_variable : unit -> type_expression

(** [new_type_variable_at_level level] creates a type variable at a specific level. *)
val new_type_variable_at_level : level -> type_expression

(** {1 Built-in Types} *)

(** The [int] type. *)
val type_int : type_expression

(** The [float] type. *)
val type_float : type_expression

(** The [string] type. *)
val type_string : type_expression

(** The [bool] type. *)
val type_bool : type_expression

(** The [unit] type. *)
val type_unit : type_expression

(** [type_ref content_type] creates a reference type [content_type ref]. *)
val type_ref : type_expression -> type_expression

(** {1 Record Type Constructors} *)

(** [type_record_closed fields] creates a closed record type.
    The record has exactly the given fields and no others. *)
val type_record_closed : (string * row_field) list -> type_expression

(** [type_record_open fields] creates an open record type.
    The record has at least the given fields, plus potentially more. *)
val type_record_open : (string * row_field) list -> type_expression

(** {1 Type Manipulation} *)

(** [representative ty] follows union-find links to find the canonical type.

    Performs path compression for efficiency: intermediate links are updated
    to point directly to the root.

    Behavior:
    - If [ty] is already canonical (a non-variable or unlinked variable),
      returns [ty] unchanged
    - If [ty] is a linked variable, follows the chain and returns the root
    - Non-[TypeVariable] types are always canonical

    @return The canonical representative of [ty] *)
val representative : type_expression -> type_expression

(** {1 Type Schemes} *)

(** Polymorphic type scheme: [forall 'a 'b. 'a -> 'b -> 'a].

    A type scheme packages a type expression with the list of type variables
    that are universally quantified. *)
type type_scheme = {
  quantified_variables : type_variable list;
  body : type_expression;
}

(** [trivial_scheme ty] wraps a monomorphic type as a scheme with no quantifiers. *)
val trivial_scheme : type_expression -> type_scheme

(** [mark_as_weak ty] marks all type variables in [ty] that are at a higher
    level than the current level as "weak".

    This is called when value restriction blocks generalization. Weak type
    variables are displayed with an underscore prefix (e.g., ['_a]) to indicate
    they are not polymorphic.

    @param ty The type whose variables should be marked as weak *)
val mark_as_weak : type_expression -> unit

(** [generalize ty] generalizes a type at the current level.

    Variables with level > [current_level ()] are marked as [generic_level]
    and collected into the scheme's [quantified_variables] list.

    Behavior:
    - Variables at current level or below remain monomorphic
    - Variables at higher levels become polymorphic (quantified)
    - If [ty] contains no generalizable variables, returns a trivial scheme
    - Uses a hash table internally for O(n) duplicate detection

    This implements the "level-based" approach to generalization from
    Rémy's "Extension of ML Type System with a Sorted Equational Theory
    on Types" (1992).

    @param ty The type to generalize (should be fully unified)
    @return A type scheme with quantified variables *)
val generalize : type_expression -> type_scheme

(** [generalize_with_filter predicate ty] generalizes a type selectively.

    This is used for relaxed value restriction. The [predicate] function
    determines which type variables should be generalized. Variables that
    fail the predicate are marked as weak.

    @param predicate A function [type_variable -> type_expression -> bool]
           that returns [true] if the variable should be generalized.
           The [type_expression] argument is the full type being generalized.
    @param ty The type to generalize
    @return A type scheme with selectively quantified variables *)
val generalize_with_filter :
  (type_variable -> type_expression -> bool) -> type_expression -> type_scheme

(** [instantiate scheme] creates fresh type variables for all quantifiers.

    Each quantified variable in [scheme.quantified_variables] is replaced
    with a fresh type variable at the current level.

    Behavior:
    - If [scheme.quantified_variables] is empty, returns [scheme.body] unchanged
    - Fresh variables are created at [current_level ()]
    - The original scheme is not modified

    @param scheme The polymorphic type scheme to instantiate
    @return A monomorphic type with fresh variables *)
val instantiate : type_scheme -> type_expression

(** {1 Type Declarations} *)

(** Information about a variant constructor.

    For [type 'a option = None | Some of 'a]:
    - [Some] has [constructor_name = "Some"], [constructor_tag_index = 1],
      [constructor_argument_type = Some 'a] *)
type constructor_info = {
  constructor_name : string;
  constructor_tag_index : int;           (** 0-based index *)
  constructor_type_name : string;        (** Parent type name *)
  constructor_argument_type : type_expression option;
  constructor_result_type : type_expression;
  constructor_type_parameters : type_variable list;
}

(** Type declaration in the environment. *)
type type_declaration = {
  declaration_name : string;
  declaration_parameters : type_variable list;
  declaration_variances : variance list;  (** Variance of each type parameter *)
  declaration_manifest : type_expression option;  (** [Some t] for type aliases *)
  declaration_kind : type_declaration_kind;
}

(** The kind of type declaration. *)
and type_declaration_kind =
  | DeclarationAbstract                          (** Abstract type *)
  | DeclarationVariant of constructor_info list  (** Variant/sum type *)
  | DeclarationRecord of (string * type_expression) list  (** Record type *)

(** {1 Path Operations} *)

(** [path_equal p1 p2] tests structural equality of paths. *)
val path_equal : path -> path -> bool

(** [path_to_string path] converts a path to a human-readable string. *)
val path_to_string : path -> string

(** [pp_path fmt path] pretty-prints a path. *)
val pp_path : Format.formatter -> path -> unit

(** Alias for [pp_path] for backward compatibility. *)
val pp_type_path : Format.formatter -> path -> unit

(** {1 Pretty Printing} *)

(** [pp_type_expression fmt ty] pretty-prints a type expression. *)
val pp_type_expression : Format.formatter -> type_expression -> unit

(** [pp_type_scheme fmt scheme] pretty-prints a type scheme. *)
val pp_type_scheme : Format.formatter -> type_scheme -> unit

(** [type_expression_to_string ty] converts a type to a human-readable string. *)
val type_expression_to_string : type_expression -> string
