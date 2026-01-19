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

(** [fresh_type_variable_id ()] generates a fresh unique ID and increments
    the global counter. Returns the ID that was allocated.

    Note: This is the only global state in Types. All level management
    is done through Typing_context. *)
val fresh_type_variable_id : unit -> int

(** [reset_type_variable_id ()] resets the type variable ID counter to 0.

    This is intended for testing only, to ensure reproducible type variable
    names between test runs. Should not be used in production code. *)
val reset_type_variable_id : unit -> unit

(** {1 Argument Labels} *)

(** Argument labels for function types and applications.
    Mirrors OCaml's Asttypes.arg_label.
    - [Nolabel]: Regular unlabeled argument
    - [Labelled name]: Labeled argument [~name:]
    - [Optional name]: Optional argument [?name:] *)
type arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

(** {1 Type Expressions} *)

(** Type variables with mutable link for unification.

    {b Design note:} Field names intentionally omit a prefix (e.g., [id] instead
    of [tv_id]) for brevity, as type variables are accessed very frequently
    throughout the type checker. This differs from other record types in the
    module which use prefixes for disambiguation. *)
type type_variable = {
  id : int;                         (** Unique identifier *)
  mutable level : level;            (** Generalization level *)
  mutable link : type_expression option;  (** Union-find link *)
  mutable weak : bool;              (** True if value restriction blocks generalization *)
  mutable rigid : bool;             (** True for locally abstract types - don't unify, extract equations *)
}

(** Core type expressions.

    The type algebra includes:
    - [TypeVariable]: unification variables
    - [TypeConstructor]: named types like [int], [option]
    - [TypeTuple]: product types [(a * b * c)]
    - [TypeArrow]: function types [label:a -> b] with optional label
    - [TypeRecord]: structural record types with row polymorphism
    - [TypePackage]: first-class module types [(module S)]
    - [TypeRowEmpty]: closed row (no more fields) *)
and type_expression =
  | TypeVariable of type_variable
  | TypeConstructor of path * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of arg_label * type_expression * type_expression  (** [label:arg -> result] *)
  | TypeRecord of row
  | TypePolyVariant of poly_variant_row  (** Polymorphic variant type *)
  | TypePackage of package_type  (** First-class module type: (module S) *)
  | TypeRowEmpty

(** Package type for first-class modules.
    Contains the module type signature that the packed module must satisfy. *)
and package_type = {
  package_path : path;  (** The module type path (for printing) *)
  package_signature : (string * type_expression) list;  (** Flattened type constraints *)
}

(** Row type for record fields.
    A row contains named fields and a "tail" that is either:
    - [TypeRowEmpty]: closed record, no other fields allowed
    - [TypeVariable]: open record, more fields possible *)
and row = {
  row_fields : (string * row_field) list;  (** Sorted by field name *)
  row_more : type_expression;              (** Row tail *)
}

(** Row field presence indicator.

    {b Design note:} Currently only [RowFieldPresent] is implemented.
    The single-variant wrapper is retained for potential future extensions:
    - Absent fields for polymorphic variant rows
    - Optional fields for structural subtyping
    - Default-valued fields

    The wrapper adds minimal overhead but preserves extensibility. *)
and row_field =
  | RowFieldPresent of type_expression

(** Polymorphic variant row type.

    Like record rows but for sum types. Contains tag fields and a row variable. *)
and poly_variant_row = {
  pv_fields : (string * poly_variant_field) list;  (** Tag fields, sorted by name *)
  pv_more : type_expression;  (** Row variable or TypeRowEmpty *)
  pv_closed : bool;  (** False for open [\[> ...\]], True for closed *)
}

(** A single polymorphic variant field. *)
and poly_variant_field =
  | PVFieldPresent of type_expression option  (** Tag is present, with optional argument *)
  | PVFieldAbsent  (** Tag is explicitly absent (for closed variants) *)

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
  | BuiltinRef    (** Mutable reference type *)
  | BuiltinArray  (** Mutable array type *)
  | BuiltinDict   (** Immutable dictionary type *)
  | BuiltinSet    (** Immutable set type *)

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

(** [new_type_variable_at_level level] creates a type variable at a specific level.

    Note: For most use cases, prefer [Typing_context.new_type_variable] which
    manages levels automatically. This function is for low-level operations
    that need explicit level control. *)
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

(** [type_array element_type] creates an array type [element_type array]. *)
val type_array : type_expression -> type_expression

(** [type_dict key_type value_type] creates a dictionary type [(key_type, value_type) dict]. *)
val type_dict : type_expression -> type_expression -> type_expression

(** [type_set element_type] creates a set type [element_type set]. *)
val type_set : type_expression -> type_expression

(** {1 Record Type Constructors} *)

(** [type_record_closed fields] creates a closed record type.
    The record has exactly the given fields and no others. *)
val type_record_closed : (string * row_field) list -> type_expression

(** [type_record_open fields ~row_var] creates an open record type.
    The record has at least the given fields, with [row_var] as the tail
    to allow additional fields. *)
val type_record_open : (string * row_field) list -> row_var:type_expression -> type_expression

(** [type_poly_variant_at_least fields ~row_var] creates an open poly variant type [\[> `A | `B \]].
    The variant has at least the given tags, with [row_var] as the tail for additional tags. *)
val type_poly_variant_at_least : (string * poly_variant_field) list -> row_var:type_expression -> type_expression

(** [type_poly_variant_exact fields] creates a closed exact poly variant type [\[ `A | `B \]]. *)
val type_poly_variant_exact : (string * poly_variant_field) list -> type_expression

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

(* NOTE: Type scheme operations (generalize, instantiate, mark_as_weak) are in
   the Type_scheme module. Use Type_scheme.generalize, Type_scheme.instantiate,
   etc. instead. This avoids circular dependency with Type_traversal. *)

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
  constructor_is_gadt : bool;            (** True if constructor has explicit return type (GADT) *)
  constructor_existentials : type_variable list;  (** Type variables in argument but not in result *)
}

(** A type parameter constraint.

    Pairs a type variable with the type it must unify with.
    Example: constraint 'a = 'b * 'c *)
type type_constraint = {
  constraint_variable : type_variable;  (** The constrained type variable *)
  constraint_type : type_expression;  (** The type the variable must equal *)
}

(** Type declaration in the environment. *)
type type_declaration = {
  declaration_name : string;
  declaration_parameters : type_variable list;
  declaration_variances : variance list;  (** Variance of each type parameter *)
  declaration_injectivities : bool list;  (** Injectivity of each type parameter. True for datatypes, may be false for aliases *)
  declaration_manifest : type_expression option;  (** [Some t] for type aliases *)
  declaration_kind : type_declaration_kind;
  declaration_private : bool;  (** True if private (pattern match ok, construction blocked) *)
  declaration_constraints : type_constraint list;  (** Type parameter constraints *)
}

(** The kind of type declaration. *)
and type_declaration_kind =
  | DeclarationAbstract                          (** Abstract type *)
  | DeclarationVariant of constructor_info list  (** Variant/sum type *)
  | DeclarationRecord of (string * type_expression) list  (** Record type *)
  | DeclarationExtensible  (** Extensible variant: [type t = ..] *)

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
