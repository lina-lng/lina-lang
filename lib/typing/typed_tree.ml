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
  | TypedPatternLocallyAbstract of Identifier.t * Types.type_declaration
      (** Locally abstract type: [(type a)] introduces scoped abstract type *)
  | TypedPatternPolyVariant of string * typed_pattern option
      (** Polymorphic variant pattern: [`Tag] or [`Tag pat] *)
  | TypedPatternError of Parsing.Syntax_tree.error_info
      (** Error recovery placeholder for invalid pattern syntax *)

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
  | TypedExpressionVariable of Identifier.t * Location.t option
      (** Variable reference with optional definition location for go-to-definition *)
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
  | TypedExpressionModuleAccess of Module_types.path * string  (** M.x, M.N.x *)
  | TypedExpressionRef of typed_expression           (** ref e *)
  | TypedExpressionDeref of typed_expression         (** !e *)
  | TypedExpressionAssign of typed_expression * typed_expression  (** e1 := e2 *)
  | TypedExpressionPolyVariant of string * typed_expression option
      (** Polymorphic variant constructor: [`Tag] or [`Tag expr] *)
  | TypedExpressionError of Parsing.Syntax_tree.error_info
      (** Error recovery placeholder for invalid expression syntax *)

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

(** Typed external declaration for FFI *)
and typed_external = {
  external_id : Common.Identifier.t;
    (** Unique identifier for this external binding *)
  external_type : Types.type_expression;
    (** The type of the external value *)
  external_spec : Typing_ffi.Types.ffi_spec;
    (** The validated FFI specification *)
  external_location : Location.t;
    (** Source location for error reporting *)
}

and typed_structure_item_desc =
  | TypedStructureValue of Parsing.Syntax_tree.recursion_flag * typed_binding list
  | TypedStructureType of Types.type_declaration list
  | TypedStructureModule of Common.Identifier.t * typed_module_expression
  | TypedStructureModuleType of string * Module_types.module_type  (** module type S = MT *)
  | TypedStructureOpen of Module_types.path * (string * Common.Identifier.t) list  (** path, opened value bindings *)
  | TypedStructureInclude of typed_module_expression * (string * Common.Identifier.t) list  (** module expr, included value bindings *)
  | TypedStructureExternal of typed_external  (** FFI external declaration *)
  | TypedStructureError of Parsing.Syntax_tree.error_info
      (** Error recovery placeholder for invalid top-level syntax *)

(** Typed module expression *)
and typed_module_expression = {
  module_desc : typed_module_desc;
  module_type : Module_types.module_type;
  module_location : Location.t;
}

and typed_module_desc =
  | TypedModuleStructure of typed_structure
  | TypedModulePath of Module_types.path
  | TypedModuleFunctor of Module_types.functor_parameter * typed_module_expression
  | TypedModuleApply of typed_module_expression * typed_module_expression
  | TypedModuleConstraint of typed_module_expression * Module_types.module_type

and typed_structure = typed_structure_item list
