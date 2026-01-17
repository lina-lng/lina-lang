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

(** Direction flag for for-loops. *)
type direction_flag =
  | Upto     (** for i = start to end *)
  | Downto   (** for i = start downto end *)
[@@deriving show, eq]

(** Argument label for function parameters and types.
    Mirrors the Types.arg_label type but for surface syntax. *)
type arg_label =
  | Nolabel                (** Unlabeled argument *)
  | Labelled of string     (** Labeled argument: [~label:x] *)
  | Optional of string     (** Optional argument: [?label:x] *)
[@@deriving show, eq]

type type_expression = type_expression_desc Location.located
[@@deriving show, eq]

and type_expression_desc =
  | TypeVariable of string
  | TypeConstructor of longident * type_expression list  (** Type constructor with optional module path: [int], [M.t] *)
  | TypeTuple of type_expression list
  | TypeArrow of arg_label * type_expression * type_expression
      (** Arrow type with optional label: [~label:int -> bool], [?opt:string -> int], [int -> bool] *)
  | TypeRecord of type_record_field list * bool
  | TypePolyVariant of poly_variant_row
      (** Polymorphic variant type: [[ `A | `B of int ]] *)
  | TypeForall of string list * type_expression
      (** Universally quantified type for polymorphic recursion: [type a b. body] *)
  | TypePackage of module_path
      (** First-class module type: [(module S)] - path to module type *)
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
  | PatternConstructor of longident * pattern option
      (** Constructor pattern: [Some x], [None], or qualified [M.Some x] *)
  | PatternAlias of pattern * string
  | PatternOr of pattern * pattern
      (** Or-pattern: [p1 | p2] matches if either pattern matches *)
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

(** Variance annotation for type parameters.

    Used in type declarations like [type +'a producer] or [type -'a consumer].
    When absent, variance is inferred from how the parameter is used. *)
type variance_annotation =
  | VarianceCovariant      (** [+'a]: covariant, output-only position *)
  | VarianceContravariant  (** [-'a]: contravariant, input-only position *)
[@@deriving show, eq]

(** Type parameter with optional variance and injectivity annotations.

    Examples:
    - ['a] - variance inferred, injectivity inferred
    - [+'a] - explicitly covariant
    - [-'a] - explicitly contravariant
    - [!'a] - explicitly injective (for GADT soundness with abstract types) *)
type type_parameter = {
  parameter_name : string;
  parameter_variance : variance_annotation option;
  parameter_injective : bool;  (** True if marked with [!] for injectivity *)
}
[@@deriving show, eq]

type constructor_declaration = {
  constructor_name : string Location.located;
  constructor_argument : type_expression option;
  constructor_return_type : type_expression option;  (** GADT return type, e.g., [Int : int -> int expr] *)
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
  | TypeExtensible  (** Extensible variant: [type t = ..] *)
[@@deriving show, eq]

(** Type extension for extensible variants.

    Syntax: type t += Constructor1 | Constructor2 of type

    This adds new constructors to an existing extensible type. *)
type type_extension = {
  extension_type_name : longident;
      (** The extensible type being extended (e.g., [t] or [M.t]) *)
  extension_type_params : type_parameter list;
      (** Type parameters (e.g., ['a] in [type 'a t += ...]) *)
  extension_constructors : constructor_declaration list;
      (** The new constructors being added *)
  extension_location : Location.t;
}
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

(* Mutually recursive block for expressions and module system.
   expression references module_expression and module_type,
   while module_expression references expression via ModuleUnpack. *)
type expression = expression_desc Location.located

and expression_desc =
  | ExpressionVariable of string
  | ExpressionConstant of constant
  | ExpressionTuple of expression list
  | ExpressionConstructor of longident * expression option
      (** Constructor expression: [Some], [None], or qualified [M.Some] *)
  | ExpressionApply of expression * (arg_label * expression) list
      (** Function application with labeled arguments: [f ~x:1 ?y:2 z] *)
  | ExpressionFunction of (arg_label * pattern) list * expression
      (** Function with labeled parameters: [fun ~x ?y z -> body] *)
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
  | ExpressionAssert of expression                  (** assert e *)
  | ExpressionWhile of expression * expression
      (** while loop: [while cond do body done] *)
  | ExpressionFor of string * expression * expression * direction_flag * expression
      (** for loop: [for i = start to/downto end do body done] *)
  | ExpressionPolyVariant of string * expression option
      (** Polymorphic variant expression: [` `A] or [` `A e] *)
  | ExpressionPack of module_expression * module_type
      (** First-class module packing: [(module ME : MT)] *)
  | ExpressionLetModule of string Location.located * module_expression * expression
      (** Local module binding: [let module M = ME in body] *)
  | ExpressionLetOp of string * let_op_binding list * expression
      (** Binding operator: [let* x = e1 in e2] or [let* x = e1 and* y = e2 in e3] *)
  | ExpressionError of error_info
      (** Error recovery placeholder for invalid expression syntax *)

(** Binding for let-operators.
    [letop_and] is [None] for the first binding, [Some "and*"] for subsequent bindings. *)
and let_op_binding = {
  letop_and : string option;
  letop_pattern : pattern;
  letop_expression : expression;
}

and record_field = {
  field_name : string Location.located;
  field_value : expression;
}

and match_arm = {
  arm_pattern : pattern;
  arm_guard : expression option;
  arm_expression : expression;
  arm_location : Location.t;
}

and binding = {
  binding_pattern : pattern;
  binding_expression : expression;
  binding_location : Location.t;
}

(** Function parameter with optional default value.
    Used during parsing to handle [?(x=default)] syntax. *)
and fun_param = {
  param_label : arg_label;
  param_pattern : pattern;
  param_default : expression option;
}

(** Recursive module binding - requires explicit signature.

    Unlike regular module bindings, recursive modules must have explicit
    signatures to enable forward references during type checking. *)
and rec_module_binding = {
  rec_module_name : string Location.located;
  rec_module_type : module_type;  (** Required signature *)
  rec_module_expr : module_expression;
  rec_module_location : Location.t;
}

and structure_item = structure_item_desc Location.located

and structure_item_desc =
  | StructureValue of recursion_flag * binding list
  | StructureType of type_declaration list
  | StructureTypeExtension of type_extension
      (** Extensible type extension: [type t += Constructor of type] *)
  | StructureModule of module_binding
  | StructureRecModule of rec_module_binding list
      (** Recursive module bindings: [module rec A : S = ... and B : T = ...] *)
  | StructureModuleType of string Location.located * module_type
  | StructureOpen of module_path
  | StructureInclude of module_expression
  | StructureExternal of external_declaration  (** FFI external declaration *)
  | StructureExpression of expression
      (** Top-level expression: [let x = 1 in x + 2] or [print "hello"] *)
  | StructureError of error_info
      (** Error recovery placeholder for invalid top-level syntax *)

and structure = structure_item list

(* Module expressions *)
and module_expression = module_expression_desc Location.located

and module_expression_desc =
  | ModuleStructure of structure              (* struct ... end *)
  | ModulePath of module_path                  (* M or M.N.P *)
  | ModuleFunctor of functor_parameter list * module_expression  (* functor (X : S) -> ME *)
  | ModuleApply of module_expression * module_expression  (* F(M) *)
  | ModuleConstraint of module_expression * module_type  (* (ME : MT) *)
  | ModuleUnpack of expression * module_type  (* (val e : MT) - unpack first-class module *)

(** Functor parameter: either named (applicative) or unit (generative).
    - Named: [(X : S)] - applicative functor, types are path-dependent
    - Unit: [()] - generative functor, types are fresh each application *)
and functor_parameter =
  | FunctorParamNamed of string Location.located * module_type  (** (X : S) *)
  | FunctorParamUnit of Location.t  (** () - generative functor *)

(* Module types (signatures) *)
and module_type = module_type_desc Location.located

and module_type_desc =
  | ModuleTypePath of module_path              (* S or M.S *)
  | ModuleTypeSignature of signature           (* sig ... end *)
  | ModuleTypeFunctor of functor_parameter list * module_type  (* functor (X : S) -> MT *)
  | ModuleTypeWith of module_type * with_constraint list  (* MT with type t = ... *)
  | ModuleTypeOf of module_expression          (* module type of M *)

and signature = signature_item list

and signature_item = signature_item_desc Location.located

and signature_item_desc =
  | SignatureValue of string Location.located * type_expression  (* val x : t *)
  | SignatureType of type_declaration list     (* type t or type t = ... *)
  | SignatureModule of string Location.located * module_type  (* module M : S *)
  | SignatureModuleType of string Location.located * module_type option  (* module type S [= MT] *)
  | SignatureOpen of module_path               (* open M *)
  | SignatureInclude of module_type            (* include S *)
  | SignatureExternal of external_declaration  (** FFI external in signature *)

and with_constraint =
  | WithType of longident * string list * type_expression  (* with type M.t = ... *)
  | WithTypeDestructive of longident * string list * type_expression  (* with type M.t := ... *)
  | WithModule of longident * module_path     (* with module M.N = P *)

and module_binding = {
  module_name : string Location.located;
  module_params : functor_parameter list;      (* For functor shorthand: module F(X : S) = ... *)
  module_type : module_type option;            (* Optional sealing: module M : S = ... *)
  module_expr : module_expression;
  module_location : Location.t;
}
[@@deriving show, eq]
