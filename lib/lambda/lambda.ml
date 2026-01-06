open Common

type primitive =
  | PrimitiveAddInt
  | PrimitiveSubInt
  | PrimitiveMulInt
  | PrimitiveDivInt
  | PrimitiveNegInt
  | PrimitiveIntEqual
  | PrimitiveIntNotEqual
  | PrimitiveIntLess
  | PrimitiveIntGreater
  | PrimitiveIntLessEqual
  | PrimitiveIntGreaterEqual
  | PrimitiveMakeBlock of int
  | PrimitiveGetField of int
  | PrimitivePrint

type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBool of bool
  | ConstantUnit

type lambda =
  | LambdaVariable of Identifier.t
  | LambdaConstant of constant
  | LambdaApply of lambda * lambda list
  | LambdaFunction of Identifier.t list * lambda
  | LambdaLet of Identifier.t * lambda * lambda
  | LambdaLetRecursive of (Identifier.t * lambda) list * lambda
  | LambdaPrimitive of primitive * lambda list
  | LambdaIfThenElse of lambda * lambda * lambda
  | LambdaSequence of lambda * lambda
  | LambdaMakeBlock of int * lambda list
  | LambdaGetField of int * lambda
  | LambdaSwitch of lambda * switch_case list * lambda option
  | LambdaConstructor of string * lambda option
  | LambdaMakeRecord of (string * lambda) list
  | LambdaGetRecordField of string * lambda
  | LambdaRecordUpdate of lambda * (string * lambda) list

and switch_case = {
  switch_tag : int;
  switch_body : lambda;
}

let primitive_of_operator = function
  | "+" -> Some PrimitiveAddInt
  | "-" -> Some PrimitiveSubInt
  | "*" -> Some PrimitiveMulInt
  | "/" -> Some PrimitiveDivInt
  | "==" -> Some PrimitiveIntEqual
  | "!=" -> Some PrimitiveIntNotEqual
  | "<" -> Some PrimitiveIntLess
  | ">" -> Some PrimitiveIntGreater
  | "<=" -> Some PrimitiveIntLessEqual
  | ">=" -> Some PrimitiveIntGreaterEqual
  | "print" -> Some PrimitivePrint
  | _ -> None

let translate_constant = function
  | Parsing.Syntax_tree.ConstantInteger n -> ConstantInt n
  | Parsing.Syntax_tree.ConstantFloat f -> ConstantFloat f
  | Parsing.Syntax_tree.ConstantString s -> ConstantString s
  | Parsing.Syntax_tree.ConstantBoolean b -> ConstantBool b
  | Parsing.Syntax_tree.ConstantUnit -> ConstantUnit

let rec translate_pattern_binding pattern value body =
  let open Typing.Typed_tree in
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
    LambdaLet (id, value, body)

  | TypedPatternWildcard ->
    LambdaLet (Identifier.create "_", value, body)

  | TypedPatternConstant _ ->
    body

  | TypedPatternTuple patterns ->
    let temp_id = Identifier.create "_tuple" in
    let bindings =
      List.mapi (fun i p ->
        let field_access = LambdaGetField (i, LambdaVariable temp_id) in
        translate_pattern_binding p field_access
      ) patterns
    in
    let inner = List.fold_right (fun f acc -> f acc) bindings body in
    LambdaLet (temp_id, value, inner)

  | TypedPatternConstructor (_, arg_pattern) ->
    begin match arg_pattern with
    | None -> body
    | Some inner_pattern ->
      let temp_id = Identifier.create "_ctor" in
      let arg_access = LambdaGetRecordField ("_0", LambdaVariable temp_id) in
      let inner = translate_pattern_binding inner_pattern arg_access body in
      LambdaLet (temp_id, value, inner)
    end

  | TypedPatternRecord (field_patterns, _is_open) ->
    let temp_id = Identifier.create "_record" in
    let bindings =
      List.map (fun (field_pattern : typed_record_pattern_field) ->
        let field_access = LambdaGetRecordField (field_pattern.typed_pattern_field_name, LambdaVariable temp_id) in
        translate_pattern_binding field_pattern.typed_pattern_field_pattern field_access
      ) field_patterns
    in
    let inner = List.fold_right (fun binding_function accumulator -> binding_function accumulator) bindings body in
    LambdaLet (temp_id, value, inner)

let rec translate_expression (expr : Typing.Typed_tree.typed_expression) : lambda =
  let open Typing.Typed_tree in
  match expr.expression_desc with
  | TypedExpressionVariable id ->
    LambdaVariable id

  | TypedExpressionConstant const ->
    LambdaConstant (translate_constant const)

  | TypedExpressionTuple exprs ->
    let translated = List.map translate_expression exprs in
    LambdaMakeBlock (0, translated)

  | TypedExpressionConstructor (name, arg_expr) ->
    let translated_arg = Option.map translate_expression arg_expr in
    LambdaConstructor (name, translated_arg)

  | TypedExpressionApply (func_expr, arg_exprs) ->
    let func = translate_expression func_expr in
    let args = List.map translate_expression arg_exprs in
    begin match func with
    | LambdaVariable id ->
      let name = Identifier.name id in
      begin match primitive_of_operator name with
      | Some prim -> LambdaPrimitive (prim, args)
      | None -> LambdaApply (func, args)
      end
    | _ -> LambdaApply (func, args)
    end

  | TypedExpressionFunction (param_patterns, body_expr) ->
    let param_ids =
      List.map (fun (p : typed_pattern) ->
        match p.pattern_desc with
        | TypedPatternVariable id -> id
        | _ -> Identifier.create "_param"
      ) param_patterns
    in
    let translated_body = translate_expression body_expr in
    let body_with_bindings =
      List.fold_right2 (fun pattern param_id body ->
        match pattern.pattern_desc with
        | TypedPatternVariable _ -> body
        | _ -> translate_pattern_binding pattern (LambdaVariable param_id) body
      ) param_patterns param_ids translated_body
    in
    LambdaFunction (param_ids, body_with_bindings)

  | TypedExpressionLet (rec_flag, bindings, body_expr) ->
    let translated_body = translate_expression body_expr in
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      List.fold_right (fun (binding : typed_binding) body ->
        let translated_expr = translate_expression binding.binding_expression in
        translate_pattern_binding binding.binding_pattern translated_expr body
      ) bindings translated_body

    | Parsing.Syntax_tree.Recursive ->
      let rec_bindings =
        List.map (fun (binding : typed_binding) ->
          let id = match binding.binding_pattern.pattern_desc with
            | TypedPatternVariable id -> id
            | _ -> Identifier.create "_rec"
          in
          let translated_expr = translate_expression binding.binding_expression in
          (id, translated_expr)
        ) bindings
      in
      LambdaLetRecursive (rec_bindings, translated_body)
    end

  | TypedExpressionIf (cond_expr, then_expr, else_expr_opt) ->
    let translated_cond = translate_expression cond_expr in
    let translated_then = translate_expression then_expr in
    let translated_else = match else_expr_opt with
      | Some else_expr -> translate_expression else_expr
      | None -> LambdaConstant ConstantUnit
    in
    LambdaIfThenElse (translated_cond, translated_then, translated_else)

  | TypedExpressionSequence (first_expr, second_expr) ->
    let translated_first = translate_expression first_expr in
    let translated_second = translate_expression second_expr in
    LambdaSequence (translated_first, translated_second)

  | TypedExpressionRecord record_fields ->
    let translated_fields = List.map (fun (field : typed_record_field) ->
      (field.typed_field_name, translate_expression field.typed_field_value)
    ) record_fields in
    LambdaMakeRecord translated_fields

  | TypedExpressionRecordAccess (record_expression, field_name) ->
    let translated_record = translate_expression record_expression in
    LambdaGetRecordField (field_name, translated_record)

  | TypedExpressionRecordUpdate (base_expression, update_fields) ->
    let translated_base = translate_expression base_expression in
    let translated_update_fields = List.map (fun (field : typed_record_field) ->
      (field.typed_field_name, translate_expression field.typed_field_value)
    ) update_fields in
    LambdaRecordUpdate (translated_base, translated_update_fields)

  | TypedExpressionMatch (scrutinee_expression, match_arms) ->
    let translated_scrutinee = translate_expression scrutinee_expression in
    let scrutinee_id = Identifier.create "_scrutinee" in
    let failure_case = LambdaApply (LambdaVariable (Identifier.create "error"),
                                    [LambdaConstant (ConstantString "Match failure")]) in
    let translated_arms = List.fold_right (fun (arm : typed_match_arm) rest ->
      let pattern_test = translate_pattern_test arm.typed_arm_pattern (LambdaVariable scrutinee_id) in
      let arm_body = translate_expression arm.typed_arm_expression in
      let body_with_pattern_bindings = translate_pattern_binding arm.typed_arm_pattern (LambdaVariable scrutinee_id) arm_body in
      let guarded_body = match arm.typed_arm_guard with
        | None -> body_with_pattern_bindings
        | Some guard_expression ->
          let arm_body_with_guard = translate_expression arm.typed_arm_expression in
          let full_body_with_bindings = translate_pattern_binding arm.typed_arm_pattern (LambdaVariable scrutinee_id) arm_body_with_guard in
          let guard_check = translate_expression guard_expression in
          let guard_with_bindings = translate_pattern_binding arm.typed_arm_pattern (LambdaVariable scrutinee_id) guard_check in
          LambdaIfThenElse (guard_with_bindings, full_body_with_bindings, rest)
      in
      LambdaIfThenElse (pattern_test, guarded_body, rest)
    ) match_arms failure_case in
    LambdaLet (scrutinee_id, translated_scrutinee, translated_arms)

and translate_pattern_test pattern value =
  let open Typing.Typed_tree in
  match pattern.pattern_desc with
  | TypedPatternVariable _ | TypedPatternWildcard -> LambdaConstant (ConstantBool true)
  | TypedPatternConstant const ->
    let const_value = LambdaConstant (translate_constant const) in
    LambdaPrimitive (PrimitiveIntEqual, [value; const_value])
  | TypedPatternTuple sub_patterns ->
    let tests = List.mapi (fun index sub_pattern ->
      translate_pattern_test sub_pattern (LambdaGetField (index, value))
    ) sub_patterns in
    List.fold_left (fun accumulator test ->
      LambdaIfThenElse (accumulator, test, LambdaConstant (ConstantBool false))
    ) (LambdaConstant (ConstantBool true)) tests
  | TypedPatternConstructor (constructor_name, argument_pattern) ->
    let tag_test = LambdaPrimitive (PrimitiveIntEqual,
      [LambdaGetRecordField ("_tag", value); LambdaConstant (ConstantString constructor_name)]) in
    begin match argument_pattern with
    | None -> tag_test
    | Some inner_pattern ->
      let arg_test = translate_pattern_test inner_pattern (LambdaGetRecordField ("_0", value)) in
      LambdaIfThenElse (tag_test, arg_test, LambdaConstant (ConstantBool false))
    end
  | TypedPatternRecord (field_patterns, _is_open) ->
    let tests = List.map (fun (field_pattern : typed_record_pattern_field) ->
      translate_pattern_test field_pattern.typed_pattern_field_pattern
        (LambdaGetRecordField (field_pattern.typed_pattern_field_name, value))
    ) field_patterns in
    List.fold_left (fun accumulator test ->
      LambdaIfThenElse (accumulator, test, LambdaConstant (ConstantBool false))
    ) (LambdaConstant (ConstantBool true)) tests

let translate_structure_item (item : Typing.Typed_tree.typed_structure_item) : lambda list =
  let open Typing.Typed_tree in
  match item.structure_item_desc with
  | TypedStructureValue (rec_flag, bindings) ->
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      List.map (fun (binding : typed_binding) ->
        let id = match binding.binding_pattern.pattern_desc with
          | TypedPatternVariable id -> id
          | _ -> Identifier.create "_top"
        in
        let translated_expr = translate_expression binding.binding_expression in
        LambdaLet (id, translated_expr, LambdaConstant ConstantUnit)
      ) bindings

    | Parsing.Syntax_tree.Recursive ->
      let rec_bindings =
        List.map (fun (binding : typed_binding) ->
          let id = match binding.binding_pattern.pattern_desc with
            | TypedPatternVariable id -> id
            | _ -> Identifier.create "_rec"
          in
          let translated_expr = translate_expression binding.binding_expression in
          (id, translated_expr)
        ) bindings
      in
      [LambdaLetRecursive (rec_bindings, LambdaConstant ConstantUnit)]
    end

  | TypedStructureType _ ->
    []

let translate_structure structure =
  List.concat_map translate_structure_item structure
