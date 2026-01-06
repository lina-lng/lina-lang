open Common
open Lua_ast

let mangle_identifier (id : Identifier.t) : identifier =
  let name = Identifier.name id in
  let stamp = Identifier.stamp id in
  if stamp = 0 then name
  else Printf.sprintf "%s_%d" name stamp

let translate_constant = function
  | Lambda.ConstantInt n -> ExpressionNumber (float_of_int n)
  | Lambda.ConstantFloat f -> ExpressionNumber f
  | Lambda.ConstantString s -> ExpressionString s
  | Lambda.ConstantBool b -> ExpressionBool b
  | Lambda.ConstantUnit -> ExpressionNil

let translate_primitive prim args =
  match prim, args with
  | Lambda.PrimitiveAddInt, [a; b] ->
    ExpressionBinaryOp (OpAdd, a, b)
  | Lambda.PrimitiveSubInt, [a; b] ->
    ExpressionBinaryOp (OpSub, a, b)
  | Lambda.PrimitiveMulInt, [a; b] ->
    ExpressionBinaryOp (OpMul, a, b)
  | Lambda.PrimitiveDivInt, [a; b] ->
    ExpressionBinaryOp (OpDiv, a, b)
  | Lambda.PrimitiveNegInt, [a] ->
    ExpressionUnaryOp (OpNegate, a)
  | Lambda.PrimitiveIntEqual, [a; b] ->
    ExpressionBinaryOp (OpEqual, a, b)
  | Lambda.PrimitiveIntNotEqual, [a; b] ->
    ExpressionBinaryOp (OpNotEqual, a, b)
  | Lambda.PrimitiveIntLess, [a; b] ->
    ExpressionBinaryOp (OpLess, a, b)
  | Lambda.PrimitiveIntGreater, [a; b] ->
    ExpressionBinaryOp (OpGreater, a, b)
  | Lambda.PrimitiveIntLessEqual, [a; b] ->
    ExpressionBinaryOp (OpLessEqual, a, b)
  | Lambda.PrimitiveIntGreaterEqual, [a; b] ->
    ExpressionBinaryOp (OpGreaterEqual, a, b)
  | Lambda.PrimitivePrint, [a] ->
    ExpressionCall (ExpressionVariable "print", [a])
  | Lambda.PrimitiveMakeBlock _, fields ->
    ExpressionTable (List.map (fun f -> FieldArray f) fields)
  | Lambda.PrimitiveGetField n, [obj] ->
    ExpressionIndex (obj, ExpressionNumber (float_of_int (n + 1)))
  | _ ->
    ExpressionNil

let rec translate_expression (lambda : Lambda.lambda) : expression =
  match lambda with
  | Lambda.LambdaVariable id ->
    ExpressionVariable (mangle_identifier id)

  | Lambda.LambdaConstant const ->
    translate_constant const

  | Lambda.LambdaApply (func, args) ->
    let func_expr = translate_expression func in
    let arg_exprs = List.map translate_expression args in
    ExpressionCall (func_expr, arg_exprs)

  | Lambda.LambdaFunction (params, body) ->
    let param_names = List.map mangle_identifier params in
    let body_stmts = translate_to_statements body in
    ExpressionFunction (param_names, body_stmts)

  | Lambda.LambdaLet (id, value, body) ->
    let func_body = translate_to_statements (Lambda.LambdaLet (id, value, body)) in
    ExpressionCall (ExpressionFunction ([], func_body), [])

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let func_body = translate_to_statements (Lambda.LambdaLetRecursive (bindings, body)) in
    ExpressionCall (ExpressionFunction ([], func_body), [])

  | Lambda.LambdaPrimitive (prim, args) ->
    let arg_exprs = List.map translate_expression args in
    translate_primitive prim arg_exprs

  | Lambda.LambdaIfThenElse (_cond, _then_branch, _else_branch) ->
    let func_body = translate_to_statements lambda in
    ExpressionCall (ExpressionFunction ([], func_body), [])

  | Lambda.LambdaSequence (_first, _second) ->
    let func_body = translate_to_statements lambda in
    ExpressionCall (ExpressionFunction ([], func_body), [])

  | Lambda.LambdaMakeBlock (_, fields) ->
    let field_exprs = List.map translate_expression fields in
    ExpressionTable (List.map (fun f -> FieldArray f) field_exprs)

  | Lambda.LambdaGetField (n, obj) ->
    let obj_expr = translate_expression obj in
    ExpressionIndex (obj_expr, ExpressionNumber (float_of_int (n + 1)))

  | Lambda.LambdaSwitch (_scrutinee, _cases, _default) ->
    let func_body = translate_to_statements lambda in
    ExpressionCall (ExpressionFunction ([], func_body), [])

  | Lambda.LambdaConstructor (name, arg) ->
    let fields = [FieldNamed ("_tag", ExpressionString name)] in
    let fields = match arg with
      | None -> fields
      | Some arg_expr ->
        fields @ [FieldNamed ("_0", translate_expression arg_expr)]
    in
    ExpressionTable fields

  | Lambda.LambdaMakeRecord field_bindings ->
    let translated_fields = List.map (fun (field_name, field_value) ->
      FieldNamed (field_name, translate_expression field_value)
    ) field_bindings in
    ExpressionTable translated_fields

  | Lambda.LambdaGetRecordField (field_name, record_expression) ->
    let translated_record = translate_expression record_expression in
    ExpressionField (translated_record, field_name)

  | Lambda.LambdaRecordUpdate (base_expression, update_fields) ->
    let func_body = translate_to_statements (Lambda.LambdaRecordUpdate (base_expression, update_fields)) in
    ExpressionCall (ExpressionFunction ([], func_body), [])

and translate_to_statements (lambda : Lambda.lambda) : block =
  match lambda with
  | Lambda.LambdaLet (id, value, body) ->
    let name = mangle_identifier id in
    let value_expr = translate_expression value in
    let local_stmt = StatementLocal ([name], [value_expr]) in
    local_stmt :: translate_to_statements body

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let assignments =
      List.map (fun (id, value) ->
        let name = mangle_identifier id in
        let value_expr = translate_expression value in
        StatementAssign ([LvalueVariable name], [value_expr])
      ) bindings
    in
    forward_decl :: assignments @ translate_to_statements body

  | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
    let cond_expr = translate_expression cond in
    let then_stmts = translate_to_statements then_branch in
    let else_stmts = translate_to_statements else_branch in
    [StatementIf ([(cond_expr, then_stmts)], Some else_stmts)]

  | Lambda.LambdaSequence (first, second) ->
    let first_stmts = translate_to_effect first in
    let second_stmts = translate_to_statements second in
    first_stmts @ second_stmts

  | Lambda.LambdaSwitch (scrutinee, cases, default) ->
    let scrutinee_expr = translate_expression scrutinee in
    let temp_name = "_switch" in
    let local_stmt = StatementLocal ([temp_name], [scrutinee_expr]) in
    let tag_access = ExpressionField (ExpressionVariable temp_name, "_tag") in
    let branches =
      List.map (fun (case : Lambda.switch_case) ->
        let cond = ExpressionBinaryOp (OpEqual, tag_access,
          ExpressionNumber (float_of_int case.switch_tag)) in
        let body = translate_to_statements case.switch_body in
        (cond, body)
      ) cases
    in
    let default_stmts = match default with
      | Some d -> Some (translate_to_statements d)
      | None -> None
    in
    [local_stmt; StatementIf (branches, default_stmts)]

  | Lambda.LambdaRecordUpdate (base_expression, update_fields) ->
    let base_expr = translate_expression base_expression in
    let result_name = "_result" in
    let copy_stmt = StatementLocal ([result_name], [ExpressionTable []]) in
    let copy_loop = StatementForIn (["_k"; "_v"],
      [ExpressionCall (ExpressionVariable "pairs", [base_expr])],
      [StatementAssign ([LvalueIndex (ExpressionVariable result_name, ExpressionVariable "_k")],
                        [ExpressionVariable "_v"])]) in
    let update_stmts = List.map (fun (field_name, field_value) ->
      StatementAssign ([LvalueField (ExpressionVariable result_name, field_name)],
                       [translate_expression field_value])
    ) update_fields in
    copy_stmt :: copy_loop :: update_stmts @ [StatementReturn [ExpressionVariable result_name]]

  | _ ->
    let expr = translate_expression lambda in
    [StatementReturn [expr]]

and translate_to_effect (lambda : Lambda.lambda) : block =
  match lambda with
  | Lambda.LambdaApply (func, args) ->
    let func_expr = translate_expression func in
    let arg_exprs = List.map translate_expression args in
    [StatementCall (func_expr, arg_exprs)]

  | Lambda.LambdaPrimitive (Lambda.PrimitivePrint, args) ->
    let arg_exprs = List.map translate_expression args in
    [StatementCall (ExpressionVariable "print", arg_exprs)]

  | Lambda.LambdaSequence (first, second) ->
    translate_to_effect first @ translate_to_effect second

  | Lambda.LambdaLet (id, value, body) ->
    let name = mangle_identifier id in
    let value_expr = translate_expression value in
    StatementLocal ([name], [value_expr]) :: translate_to_effect body

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let assignments =
      List.map (fun (id, value) ->
        let name = mangle_identifier id in
        let value_expr = translate_expression value in
        StatementAssign ([LvalueVariable name], [value_expr])
      ) bindings
    in
    forward_decl :: assignments @ translate_to_effect body

  | _ ->
    let expr = translate_expression lambda in
    match expr with
    | ExpressionCall (func, args) -> [StatementCall (func, args)]
    | _ -> []

let generate_top_level (lambda : Lambda.lambda) : block =
  match lambda with
  | Lambda.LambdaLet (id, value, Lambda.LambdaConstant Lambda.ConstantUnit) ->
    let name = mangle_identifier id in
    begin match value with
    | Lambda.LambdaFunction (params, body) ->
      let param_names = List.map mangle_identifier params in
      let body_stmts = translate_to_statements body in
      [StatementLocalFunction (name, param_names, body_stmts)]
    | _ ->
      let value_expr = translate_expression value in
      [StatementLocal ([name], [value_expr])]
    end

  | Lambda.LambdaLetRecursive (bindings, Lambda.LambdaConstant Lambda.ConstantUnit) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let function_defs =
      List.filter_map (fun (id, value) ->
        let name = mangle_identifier id in
        match value with
        | Lambda.LambdaFunction (params, body) ->
          let param_names = List.map mangle_identifier params in
          let body_stmts = translate_to_statements body in
          Some (StatementAssign ([LvalueVariable name],
            [ExpressionFunction (param_names, body_stmts)]))
        | _ ->
          let value_expr = translate_expression value in
          Some (StatementAssign ([LvalueVariable name], [value_expr]))
      ) bindings
    in
    forward_decl :: function_defs

  | _ ->
    translate_to_effect lambda

let generate lambdas =
  List.concat_map generate_top_level lambdas
