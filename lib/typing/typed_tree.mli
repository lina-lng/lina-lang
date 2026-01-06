open Common

type typed_pattern = {
  pattern_desc : typed_pattern_desc;
  pattern_type : Types.type_expression;
  pattern_location : Location.t;
}

and typed_pattern_desc =
  | TypedPatternVariable of Identifier.t
  | TypedPatternWildcard
  | TypedPatternConstant of Parsing.Syntax_tree.constant
  | TypedPatternTuple of typed_pattern list
  | TypedPatternConstructor of string * typed_pattern option

type typed_expression = {
  expression_desc : typed_expression_desc;
  expression_type : Types.type_expression;
  expression_location : Location.t;
}

and typed_expression_desc =
  | TypedExpressionVariable of Identifier.t
  | TypedExpressionConstant of Parsing.Syntax_tree.constant
  | TypedExpressionTuple of typed_expression list
  | TypedExpressionConstructor of string * typed_expression option
  | TypedExpressionApply of typed_expression * typed_expression list
  | TypedExpressionFunction of typed_pattern list * typed_expression
  | TypedExpressionLet of Parsing.Syntax_tree.recursion_flag * typed_binding list * typed_expression
  | TypedExpressionIf of typed_expression * typed_expression * typed_expression option
  | TypedExpressionSequence of typed_expression * typed_expression

and typed_binding = {
  binding_pattern : typed_pattern;
  binding_expression : typed_expression;
  binding_location : Location.t;
}

type typed_structure_item = {
  structure_item_desc : typed_structure_item_desc;
  structure_item_location : Location.t;
}

and typed_structure_item_desc =
  | TypedStructureValue of Parsing.Syntax_tree.recursion_flag * typed_binding list
  | TypedStructureType of Types.type_declaration list

type typed_structure = typed_structure_item list
