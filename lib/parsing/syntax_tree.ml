open Common

type constant =
  | ConstantInteger of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBoolean of bool
  | ConstantUnit
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
[@@deriving show, eq]

type structure_item = structure_item_desc Location.located
[@@deriving show, eq]

and structure_item_desc =
  | StructureValue of recursion_flag * binding list
  | StructureType of type_declaration list
[@@deriving show, eq]

type structure = structure_item list
[@@deriving show, eq]
