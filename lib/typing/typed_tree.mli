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
  | TypedPatternConstructor of Types.constructor_info * typed_pattern option
  | TypedPatternRecord of typed_record_pattern_field list * bool

and typed_record_pattern_field = {
  typed_pattern_field_name : string;
  typed_pattern_field_pattern : typed_pattern;
}

type typed_expression = {
  expression_desc : typed_expression_desc;
  expression_type : Types.type_expression;
  expression_location : Location.t;
}

and typed_expression_desc =
  | TypedExpressionVariable of Identifier.t
  | TypedExpressionConstant of Parsing.Syntax_tree.constant
  | TypedExpressionTuple of typed_expression list
  | TypedExpressionConstructor of Types.constructor_info * typed_expression option
  | TypedExpressionApply of typed_expression * typed_expression list
  | TypedExpressionFunction of typed_pattern list * typed_expression
  | TypedExpressionLet of Parsing.Syntax_tree.recursion_flag * typed_binding list * typed_expression
  | TypedExpressionIf of typed_expression * typed_expression * typed_expression option
  | TypedExpressionSequence of typed_expression * typed_expression
  | TypedExpressionRecord of typed_record_field list
  | TypedExpressionRecordAccess of typed_expression * string
  | TypedExpressionRecordUpdate of typed_expression * typed_record_field list
  | TypedExpressionMatch of typed_expression * typed_match_arm list

and typed_record_field = {
  typed_field_name : string;
  typed_field_value : typed_expression;
}

and typed_match_arm = {
  typed_arm_pattern : typed_pattern;
  typed_arm_guard : typed_expression option;
  typed_arm_expression : typed_expression;
  typed_arm_location : Location.t;
}

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
