open Common

(** Error information for error recovery during parsing.
    Stored in error nodes when the parser encounters invalid syntax. *)
type error_info = {
  error_message : string;
  error_span : Location.t;
}
[@@deriving show, eq]

type constant =
  | ConstantInteger of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBoolean of bool
  | ConstantUnit
[@@deriving show, eq]

(* Qualified identifiers for values, types, and constructors *)
type longident = longident_desc Location.located
[@@deriving show, eq]

and longident_desc =
  | Lident of string              (* Simple identifier: x, Some, int *)
  | Ldot of longident * string    (* Qualified: M.x, M.Some, M.t *)
[@@deriving show, eq]

(* Module paths - only uppercase module names: M.N.P *)
type module_path = string list Location.located
[@@deriving show, eq]

type recursion_flag =
  | Nonrecursive
  | Recursive
[@@deriving show, eq]

type type_expression = type_expression_desc Location.located
[@@deriving show, eq]

and type_expression_desc =
  | TypeVariable of string
  | TypeConstructor of string * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of type_expression * type_expression
  | TypeRecord of type_record_field list * bool
  | TypePolyVariant of poly_variant_row
      (** Polymorphic variant type: [[ `A | `B of int ]] *)
[@@deriving show, eq]

(** A polymorphic variant type row.

    Polymorphic variants come in three flavors:
    - Exact: [[ `A | `B ]] - exactly these tags, no more
    - AtLeast: [[> `A | `B ]] - at least these tags, possibly more
    - AtMost: [[< `A | `B > `A ]] - at most these tags, with required subset *)
and poly_variant_row =
  | PolyRowExact of poly_variant_field list
      (** Exact variant: [[ `A | `B ]] *)
  | PolyRowAtLeast of poly_variant_field list
      (** Open lower bound: [[> `A | `B ]] *)
  | PolyRowAtMost of poly_variant_field list * string list
      (** Closed upper bound with required tags: [[< `A | `B > `A ]] *)
[@@deriving show, eq]

(** A single polymorphic variant field/constructor. *)
and poly_variant_field = {
  poly_variant_tag : string;  (** The tag name (without backtick) *)
  poly_variant_argument : type_expression option;  (** Optional argument type *)
}
[@@deriving show, eq]

and type_record_field = {
  type_field_name : string;
  type_field_type : type_expression;
}
[@@deriving show, eq]

type pattern = pattern_desc Location.located
[@@deriving show, eq]

and pattern_desc =
  | PatternVariable of string
  | PatternWildcard
  | PatternConstant of constant
  | PatternTuple of pattern list
  | PatternConstructor of string * pattern option
  | PatternAlias of pattern * string
  | PatternConstraint of pattern * type_expression
  | PatternRecord of record_pattern_field list * bool
  | PatternLocallyAbstract of string
      (** Locally abstract type: [(type a)] introduces scoped abstract type *)
  | PatternPolyVariant of string * pattern option
      (** Polymorphic variant pattern: [` `A] or [` `A p] *)
  | PatternError of error_info
      (** Error recovery placeholder for invalid pattern syntax *)
[@@deriving show, eq]

and record_pattern_field = {
  pattern_field_name : string Location.located;
  pattern_field_pattern : pattern option;
}
[@@deriving show, eq]

type expression = expression_desc Location.located
[@@deriving show, eq]

and expression_desc =
  | ExpressionVariable of string
  | ExpressionConstant of constant
  | ExpressionTuple of expression list
  | ExpressionConstructor of string * expression option
  | ExpressionApply of expression * expression list
  | ExpressionFunction of pattern list * expression
  | ExpressionLet of recursion_flag * binding list * expression
  | ExpressionIf of expression * expression * expression option
  | ExpressionSequence of expression * expression
  | ExpressionConstraint of expression * type_expression
  | ExpressionRecord of record_field list
  | ExpressionRecordAccess of expression * string
  | ExpressionRecordUpdate of expression * record_field list
  | ExpressionMatch of expression * match_arm list
  | ExpressionModuleAccess of module_path * string  (** M.x, M.N.x *)
  | ExpressionRef of expression                     (** ref e *)
  | ExpressionDeref of expression                   (** !e *)
  | ExpressionAssign of expression * expression     (** e1 := e2 *)
  | ExpressionPolyVariant of string * expression option
      (** Polymorphic variant expression: [` `A] or [` `A e] *)
  | ExpressionError of error_info
      (** Error recovery placeholder for invalid expression syntax *)
[@@deriving show, eq]

and record_field = {
  field_name : string Location.located;
  field_value : expression;
}
[@@deriving show, eq]

and match_arm = {
  arm_pattern : pattern;
  arm_guard : expression option;
  arm_expression : expression;
  arm_location : Location.t;
}
[@@deriving show, eq]

and binding = {
  binding_pattern : pattern;
  binding_expression : expression;
  binding_location : Location.t;
}
[@@deriving show, eq]

type constructor_declaration = {
  constructor_name : string Location.located;
  constructor_argument : type_expression option;
  constructor_return_type : type_expression option;  (** GADT return type, e.g., [Int : int -> int expr] *)
}
[@@deriving show, eq]

(** Variance annotation for type parameters.

    Used in type declarations like [type +'a producer] or [type -'a consumer].
    When absent, variance is inferred from how the parameter is used. *)
type variance_annotation =
  | VarianceCovariant      (** [+'a]: covariant, output-only position *)
  | VarianceContravariant  (** [-'a]: contravariant, input-only position *)
[@@deriving show, eq]

(** Type parameter with optional variance annotation.

    Examples:
    - ['a] - variance inferred
    - [+'a] - explicitly covariant
    - [-'a] - explicitly contravariant *)
type type_parameter = {
  parameter_name : string;
  parameter_variance : variance_annotation option;
}
[@@deriving show, eq]

(** A type parameter constraint.

    Syntax: [constraint 'a = type_expression]

    Example: [type 'a t constraint 'a = 'b * 'c] *)
type type_constraint = {
  constraint_variable : string;  (** The constrained type variable, e.g., "'a" *)
  constraint_type : type_expression;  (** The type the variable is constrained to *)
  constraint_location : Location.t;
}
[@@deriving show, eq]

type type_declaration = {
  type_name : string Location.located;
  type_parameters : type_parameter list;  (** Type parameters with variance annotations *)
  type_kind : type_declaration_kind;
  type_private : bool;  (** True if this is a private type (prevents construction) *)
  type_constraints : type_constraint list;  (** Type parameter constraints *)
  type_location : Location.t;
}
[@@deriving show, eq]

and type_declaration_kind =
  | TypeAbstract
  | TypeVariant of constructor_declaration list
  | TypeAlias of type_expression
[@@deriving show, eq]

(** External declaration for FFI.

    Syntax: [attributes] external name : type = "lua_name"

    Example:
    {[
      @module("socket")
      external tcp : unit -> socket = "tcp"
    ]} *)
type external_declaration = {
  external_attributes : Parsing_ffi.Attributes.attribute list;
      (** List of [@attr] before the declaration *)
  external_name : string Location.located;
      (** The Lina binding name *)
  external_type : type_expression;
      (** The type signature *)
  external_primitive : string;
      (** The Lua name (string after =) *)
  external_location : Location.t;
      (** Source location of the entire declaration *)
}
[@@deriving show, eq]

(* Forward declaration for mutual recursion *)
type structure_item = structure_item_desc Location.located
[@@deriving show, eq]

and structure_item_desc =
  | StructureValue of recursion_flag * binding list
  | StructureType of type_declaration list
  | StructureModule of module_binding
  | StructureModuleType of string Location.located * module_type
  | StructureOpen of module_path
  | StructureInclude of module_expression
  | StructureExternal of external_declaration  (** FFI external declaration *)
  | StructureError of error_info
      (** Error recovery placeholder for invalid top-level syntax *)
[@@deriving show, eq]

and structure = structure_item list

(* Module expressions *)
and module_expression = module_expression_desc Location.located
[@@deriving show, eq]

and module_expression_desc =
  | ModuleStructure of structure              (* struct ... end *)
  | ModulePath of module_path                  (* M or M.N.P *)
  | ModuleFunctor of functor_parameter list * module_expression  (* functor (X : S) -> ME *)
  | ModuleApply of module_expression * module_expression  (* F(M) *)
  | ModuleConstraint of module_expression * module_type  (* (ME : MT) *)
[@@deriving show, eq]

and functor_parameter = {
  functor_param_name : string Location.located;
  functor_param_type : module_type;
}
[@@deriving show, eq]

(* Module types (signatures) *)
and module_type = module_type_desc Location.located
[@@deriving show, eq]

and module_type_desc =
  | ModuleTypePath of module_path              (* S or M.S *)
  | ModuleTypeSignature of signature           (* sig ... end *)
  | ModuleTypeFunctor of functor_parameter list * module_type  (* functor (X : S) -> MT *)
  | ModuleTypeWith of module_type * with_constraint list  (* MT with type t = ... *)
[@@deriving show, eq]

and signature = signature_item list
[@@deriving show, eq]

and signature_item = signature_item_desc Location.located
[@@deriving show, eq]

and signature_item_desc =
  | SignatureValue of string Location.located * type_expression  (* val x : t *)
  | SignatureType of type_declaration list     (* type t or type t = ... *)
  | SignatureModule of string Location.located * module_type  (* module M : S *)
  | SignatureModuleType of string Location.located * module_type option  (* module type S [= MT] *)
  | SignatureOpen of module_path               (* open M *)
  | SignatureInclude of module_type            (* include S *)
  | SignatureExternal of external_declaration  (** FFI external in signature *)
[@@deriving show, eq]

and with_constraint =
  | WithType of longident * string list * type_expression  (* with type M.t = ... *)
  | WithTypeDestructive of longident * string list * type_expression  (* with type M.t := ... *)
  | WithModule of longident * module_path     (* with module M.N = P *)
[@@deriving show, eq]

and module_binding = {
  module_name : string Location.located;
  module_params : functor_parameter list;      (* For functor shorthand: module F(X : S) = ... *)
  module_type : module_type option;            (* Optional sealing: module M : S = ... *)
  module_expr : module_expression;
  module_location : Location.t;
}
[@@deriving show, eq]
