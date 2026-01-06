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
    | Some p ->
      let temp_id = Identifier.create "_ctor" in
      let arg_access = LambdaGetField (0, LambdaVariable temp_id) in
      let inner = translate_pattern_binding p arg_access body in
      LambdaLet (temp_id, value, inner)
    end

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
