open Common
open Lua_ast

(* Lua 5.1+ reserved keywords *)
let lua_reserved_keywords = [
  "and"; "break"; "do"; "else"; "elseif"; "end"; "false"; "for";
  "function"; "goto"; "if"; "in"; "local"; "nil"; "not"; "or";
  "repeat"; "return"; "then"; "true"; "until"; "while"
]

let is_lua_keyword name = List.mem name lua_reserved_keywords

let mangle_identifier (id : Identifier.t) : identifier =
  let name = Identifier.name id in
  let stamp = Identifier.stamp id in
  let base_name = if is_lua_keyword name then "_" ^ name else name in
  if stamp = 0 then base_name
  else Printf.sprintf "%s_%d" base_name stamp

(* Singleton registry for nullary constructors *)
module SingletonKey = struct
  type t = string * int  (* type_name, tag_index *)
  let compare = compare
end
module SingletonSet = Set.Make(SingletonKey)

let singleton_registry = ref SingletonSet.empty

let reset_singletons () = singleton_registry := SingletonSet.empty

let singleton_var_name type_name tag_index =
  Printf.sprintf "_Ctor_%s_%d" type_name tag_index

let register_singleton type_name tag_index =
  singleton_registry := SingletonSet.add (type_name, tag_index) !singleton_registry

let generate_singleton_preamble () =
  SingletonSet.fold (fun (type_name, tag_index) acc ->
    let var_name = singleton_var_name type_name tag_index in
    let table = ExpressionTable [FieldNamed ("_tag", ExpressionNumber (float_of_int tag_index))] in
    StatementLocal ([var_name], [table]) :: acc
  ) !singleton_registry []

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

  | Lambda.LambdaConstructor (tag, None) when tag.Lambda.tag_is_nullary ->
    (* Nullary constructor: use singleton *)
    register_singleton tag.Lambda.tag_type_name tag.Lambda.tag_index;
    ExpressionVariable (singleton_var_name tag.Lambda.tag_type_name tag.Lambda.tag_index)

  | Lambda.LambdaConstructor (tag, arg) ->
    let fields = [FieldNamed ("_tag", ExpressionNumber (float_of_int tag.Lambda.tag_index))] in
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

  | Lambda.LambdaModule bindings ->
    (* Module as a Lua table with named fields *)
    let fields = List.map (fun (binding : Lambda.module_binding) ->
      FieldNamed (binding.mb_name, translate_expression binding.mb_value)
    ) bindings in
    ExpressionTable fields

  | Lambda.LambdaModuleAccess (module_expr, field_name) ->
    (* Module access as field access: M.x *)
    let translated_module = translate_expression module_expr in
    ExpressionField (translated_module, field_name)

  | Lambda.LambdaFunctor (param_id, body) ->
    (* Functor as a function: functor (X : S) -> ME becomes function(X) return ME end *)
    let param_name = mangle_identifier param_id in
    let body_stmts = translate_to_statements body in
    ExpressionFunction ([param_name], body_stmts)

  | Lambda.LambdaFunctorApply (func_expr, arg_expr) ->
    (* Functor application: F(X) becomes F(X) function call *)
    let translated_func = translate_expression func_expr in
    let translated_arg = translate_expression arg_expr in
    ExpressionCall (translated_func, [translated_arg])

(* Helper to translate a value and assign it to a variable *)
and translate_value_to_assignment name lambda : block =
  match lambda with
  | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
    (* Nested let: float it out and then assign *)
    let inner_name = mangle_identifier inner_id in
    let inner_expr = translate_expression inner_value in
    let inner_decl = StatementLocal ([inner_name], [inner_expr]) in
    inner_decl :: translate_value_to_assignment name inner_body

  | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
    (* Nested if: generate if statement with assignments *)
    let cond_expr = translate_expression cond in
    let then_stmts = translate_value_to_assignment name then_branch in
    let else_stmts = translate_value_to_assignment name else_branch in
    [StatementIf ([(cond_expr, then_stmts)], Some else_stmts)]

  | Lambda.LambdaSequence (first, second) ->
    let first_stmts = translate_to_effect first in
    first_stmts @ translate_value_to_assignment name second

  | _ ->
    let expr = translate_expression lambda in
    [StatementAssign ([LvalueVariable name], [expr])]

(* Helper for dispatch table switch (used for 4+ cases) *)
and translate_switch_as_dispatch _scrutinee_name tag_access cases default =
  (* Generate:
     local _dispatch = {
       [0] = function() ... end,
       [1] = function() ... end,
     }
     local _handler = _dispatch[scrutinee._tag]
     if _handler then return _handler() else <default> end
  *)
  let dispatch_name = "_dispatch" in
  let handler_name = "_handler" in
  let dispatch_entries = List.map (fun (case : Lambda.switch_case) ->
    let handler_body = translate_to_statements case.switch_body in
    let handler = ExpressionFunction ([], handler_body) in
    FieldIndexed (ExpressionNumber (float_of_int case.switch_tag), handler)
  ) cases in
  let dispatch_table = ExpressionTable dispatch_entries in
  let dispatch_decl = StatementLocal ([dispatch_name], [dispatch_table]) in
  let handler_lookup = ExpressionIndex (ExpressionVariable dispatch_name, tag_access) in
  let handler_decl = StatementLocal ([handler_name], [handler_lookup]) in
  let handler_call = ExpressionCall (ExpressionVariable handler_name, []) in
  let default_stmts = match default with
    | Some d -> translate_to_statements d
    | None -> [StatementReturn [ExpressionCall (ExpressionVariable "error",
                [ExpressionString "Match failure"])]]
  in
  [
    dispatch_decl;
    handler_decl;
    StatementIf (
      [(ExpressionVariable handler_name, [StatementReturn [handler_call]])],
      Some default_stmts
    )
  ]

and translate_to_statements (lambda : Lambda.lambda) : block =
  match lambda with
  | Lambda.LambdaLet (id, value, body) ->
    begin match value with
    | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
      (* Float nested let out to avoid IIFE:
         let id = (let inner = v in e) in body
         becomes: let inner = v in let id = e in body *)
      translate_to_statements (Lambda.LambdaLet (inner_id, inner_value,
        Lambda.LambdaLet (id, inner_body, body)))

    | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
      (* Transform if-in-value to avoid IIFE:
         let id = if c then e1 else e2 in body
         becomes: local id; if c then id = e1 else id = e2 end; body *)
      let name = mangle_identifier id in
      let decl = StatementLocal ([name], []) in
      let cond_expr = translate_expression cond in
      let then_stmts = translate_value_to_assignment name then_branch in
      let else_stmts = translate_value_to_assignment name else_branch in
      let if_stmt = StatementIf ([(cond_expr, then_stmts)], Some else_stmts) in
      decl :: if_stmt :: translate_to_statements body

    | Lambda.LambdaSequence (first, second) ->
      (* Float sequence to avoid IIFE:
         let id = (e1; e2) in body
         becomes: e1; let id = e2 in body *)
      let first_stmts = translate_to_effect first in
      first_stmts @ translate_to_statements (Lambda.LambdaLet (id, second, body))

    | _ ->
      let name = mangle_identifier id in
      let value_expr = translate_expression value in
      let local_stmt = StatementLocal ([name], [value_expr]) in
      local_stmt :: translate_to_statements body
    end

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
    let num_cases = List.length cases in
    (* Use dispatch table for 4+ cases, otherwise use if-chain *)
    if num_cases >= 4 then
      translate_switch_as_dispatch temp_name tag_access cases default
      |> fun stmts -> local_stmt :: stmts
    else
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

(* Helper to generate top-level binding with let floating *)
let rec generate_top_level_binding (target_name : identifier) (value : Lambda.lambda) : block =
  match value with
  | Lambda.LambdaFunction (params, body) ->
    let param_names = List.map mangle_identifier params in
    let body_stmts = translate_to_statements body in
    [StatementLocalFunction (target_name, param_names, body_stmts)]

  | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
    (* Float nested let out:
       let target = (let inner = v in e)
       becomes: let inner = v; let target = e *)
    let inner_name = mangle_identifier inner_id in
    let inner_stmts = generate_top_level_binding inner_name inner_value in
    inner_stmts @ generate_top_level_binding target_name inner_body

  | Lambda.LambdaIfThenElse (condition, then_branch, else_branch) ->
    (* Transform if-in-value:
       let target = if c then e1 else e2
       becomes: local target; if c then target = e1 else target = e2 end *)
    let declaration = StatementLocal ([target_name], []) in
    let condition_expr = translate_expression condition in
    let then_stmts = translate_value_to_assignment target_name then_branch in
    let else_stmts = translate_value_to_assignment target_name else_branch in
    let if_statement = StatementIf ([(condition_expr, then_stmts)], Some else_stmts) in
    [declaration; if_statement]

  | Lambda.LambdaSequence (first_expr, second_expr) ->
    (* Float sequence:
       let target = (e1; e2)
       becomes: e1; let target = e2 *)
    let first_stmts = translate_to_effect first_expr in
    first_stmts @ generate_top_level_binding target_name second_expr

  | _ ->
    let value_expr = translate_expression value in
    [StatementLocal ([target_name], [value_expr])]

let generate_top_level (lambda : Lambda.lambda) : block =
  match lambda with
  | Lambda.LambdaLet (identifier, value, Lambda.LambdaConstant Lambda.ConstantUnit) ->
    let target_name = mangle_identifier identifier in
    generate_top_level_binding target_name value

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
  reset_singletons ();
  let body = List.concat_map generate_top_level lambdas in
  let preamble = generate_singleton_preamble () in
  preamble @ body
