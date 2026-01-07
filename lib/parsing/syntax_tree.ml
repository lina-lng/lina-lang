open Common

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
}
[@@deriving show, eq]

type type_declaration = {
  type_name : string Location.located;
  type_parameters : string list;
  type_kind : type_declaration_kind;
  type_location : Location.t;
}
[@@deriving show, eq]

and type_declaration_kind =
  | TypeAbstract
  | TypeVariant of constructor_declaration list
  | TypeAlias of type_expression
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
[@@deriving show, eq]

and with_constraint =
  | WithType of longident * string list * type_expression  (* with type M.t = ... *)
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
