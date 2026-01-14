open Common
open Lua_ast

(** Re-export identifier mangling for external use *)
let mangle_identifier = Identifier_mangle.mangle_identifier

(** {1 Code Generation Context}

    The context tracks state accumulated during code generation,
    using the Singleton_registry module for nullary constructor tracking. *)

(** Code generation context, threaded through translation functions. *)
type context = {
  singletons : Singleton_registry.t;  (** Registered nullary constructor singletons *)
}

(** Create an empty context *)
let empty_context = { singletons = Singleton_registry.empty }

(** Register a singleton in the context *)
let register_singleton ctx type_name tag_index =
  { singletons = Singleton_registry.register ctx.singletons type_name tag_index }

(** Generate singleton preamble from context *)
let generate_singleton_preamble ctx =
  Singleton_registry.generate_preamble ctx.singletons

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
  | Lambda.PrimitiveStringEqual, [a; b] ->
    ExpressionBinaryOp (OpEqual, a, b)
  | Lambda.PrimitivePrint, [a] ->
    ExpressionCall (ExpressionVariable "print", [a])
  | Lambda.PrimitiveMakeBlock _, fields ->
    ExpressionTable (List.map (fun f -> FieldArray f) fields)
  | Lambda.PrimitiveGetField n, [obj] ->
    ExpressionIndex (obj, ExpressionNumber (float_of_int (n + 1)))
  | _ ->
    ExpressionNil

(** Wrap the last argument with table.unpack() for variadic FFI calls.

    When a function is marked @variadic, the last argument is expected to be
    an array/table that should be spread as multiple arguments to the Lua
    function using table.unpack().

    @param args The translated Lua expressions for arguments
    @param is_variadic Whether this is a variadic call
    @return Arguments with last argument wrapped in table.unpack() if variadic *)
let wrap_variadic_args (args : expression list) (is_variadic : bool) : expression list =
  if not is_variadic then args
  else
    match List.rev args with
    | [] -> args  (* No arguments to unpack *)
    | last :: preceding ->
      let unpack_call = ExpressionCall (
        ExpressionField (ExpressionVariable "table", "unpack"),
        [last]
      ) in
      List.rev (unpack_call :: preceding)

(** Generate FFI call expression based on the spec and translated arguments.

    @param spec The FFI specification with kind, lua name, etc.
    @param args The translated Lua expressions for arguments *)
let generate_ffi_call (spec : Typing_ffi.Types.ffi_spec) (args : expression list) : expression =
  let open Typing_ffi.Types in
  let lua_name = spec.ffi_lua_name in
  let is_variadic = spec.ffi_is_variadic in
  match spec.ffi_kind with
  | FFIKindModule module_path ->
    (* require("module").name(args) - with variadic support *)
    let wrapped_args = wrap_variadic_args args is_variadic in
    let require_call = ExpressionCall (ExpressionVariable "require", [ExpressionString module_path]) in
    let func = ExpressionField (require_call, lua_name) in
    ExpressionCall (func, wrapped_args)

  | FFIKindGlobal scope_path ->
    (* Global with optional scope: name(args) or scope.name(args) - with variadic support *)
    let wrapped_args = wrap_variadic_args args is_variadic in
    let func = match scope_path with
      | [] -> ExpressionVariable lua_name
      | path ->
        let base = ExpressionVariable (List.hd path) in
        let scoped = List.fold_left (fun acc name -> ExpressionField (acc, name)) base (List.tl path) in
        ExpressionField (scoped, lua_name)
    in
    ExpressionCall (func, wrapped_args)

  | FFIKindMethod ->
    (* Method call: receiver:name(rest_args) - with variadic support for rest_args *)
    begin match args with
    | receiver :: rest_args ->
      let wrapped_rest = wrap_variadic_args rest_args is_variadic in
      ExpressionMethodCall (receiver, lua_name, wrapped_rest)
    | [] ->
      (* Should not happen - validation ensures arity >= 1 *)
      ExpressionCall (ExpressionVariable "error", [ExpressionString "FFI method call with no receiver"])
    end

  | FFIKindGetter ->
    (* Property getter: obj.field - fixed arity, no variadic *)
    begin match args with
    | [obj] -> ExpressionField (obj, lua_name)
    | _ -> ExpressionCall (ExpressionVariable "error", [ExpressionString "FFI getter with wrong arity"])
    end

  | FFIKindSetter ->
    (* Property setter: obj.field = value (returns nil) - fixed arity, no variadic *)
    (* We need to generate an IIFE: (function() obj.field = value; return nil end)() *)
    begin match args with
    | [obj; value] ->
      let assign = StatementAssign ([LvalueField (obj, lua_name)], [value]) in
      let return_nil = StatementReturn [ExpressionNil] in
      ExpressionCall (ExpressionFunction ([], [assign; return_nil]), [])
    | _ -> ExpressionCall (ExpressionVariable "error", [ExpressionString "FFI setter with wrong arity"])
    end

  | FFIKindIndexGetter ->
    (* Index getter: obj[key] - fixed arity, no variadic *)
    begin match args with
    | [obj; key] -> ExpressionIndex (obj, key)
    | _ -> ExpressionCall (ExpressionVariable "error", [ExpressionString "FFI index getter with wrong arity"])
    end

  | FFIKindIndexSetter ->
    (* Index setter: obj[key] = value (returns nil) - fixed arity, no variadic *)
    begin match args with
    | [obj; key; value] ->
      let assign = StatementAssign ([LvalueIndex (obj, key)], [value]) in
      let return_nil = StatementReturn [ExpressionNil] in
      ExpressionCall (ExpressionFunction ([], [assign; return_nil]), [])
    | _ -> ExpressionCall (ExpressionVariable "error", [ExpressionString "FFI index setter with wrong arity"])
    end

  | FFIKindConstructor ->
    (* Constructor: Class.new(args) - with variadic support *)
    let wrapped_args = wrap_variadic_args args is_variadic in
    let constructor = ExpressionField (ExpressionVariable lua_name, "new") in
    ExpressionCall (constructor, wrapped_args)

(** Translate a lambda expression to a Lua expression, threading context.
    Returns (expression, updated_context). *)
let rec translate_expression ctx (lambda : Lambda.lambda) : expression * context =
  match lambda with
  | Lambda.LambdaVariable id ->
    (ExpressionVariable (mangle_identifier id), ctx)

  | Lambda.LambdaConstant const ->
    (translate_constant const, ctx)

  | Lambda.LambdaApply (func, args) ->
    let func_expr, ctx = translate_expression ctx func in
    let arg_exprs, ctx = translate_expression_list ctx args in
    (ExpressionCall (func_expr, arg_exprs), ctx)

  | Lambda.LambdaFunction (params, body) ->
    let param_names = List.map mangle_identifier params in
    let body_stmts, ctx = translate_to_statements ctx body in
    (ExpressionFunction (param_names, body_stmts), ctx)

  | Lambda.LambdaLet (id, value, body) ->
    let func_body, ctx = translate_to_statements ctx (Lambda.LambdaLet (id, value, body)) in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let func_body, ctx = translate_to_statements ctx (Lambda.LambdaLetRecursive (bindings, body)) in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaPrimitive (prim, args) ->
    let arg_exprs, ctx = translate_expression_list ctx args in
    (translate_primitive prim arg_exprs, ctx)

  | Lambda.LambdaIfThenElse (_cond, _then_branch, _else_branch) ->
    let func_body, ctx = translate_to_statements ctx lambda in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaSequence (_first, _second) ->
    let func_body, ctx = translate_to_statements ctx lambda in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaMakeBlock (_, fields) ->
    let field_exprs, ctx = translate_expression_list ctx fields in
    (ExpressionTable (List.map (fun f -> FieldArray f) field_exprs), ctx)

  | Lambda.LambdaGetField (n, obj) ->
    let obj_expr, ctx = translate_expression ctx obj in
    (ExpressionIndex (obj_expr, ExpressionNumber (float_of_int (n + 1))), ctx)

  | Lambda.LambdaSwitch (_scrutinee, _cases, _default) ->
    let func_body, ctx = translate_to_statements ctx lambda in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaConstructor (tag, None) when tag.Lambda.tag_is_nullary ->
    (* Nullary constructor: use singleton *)
    let ctx = register_singleton ctx tag.Lambda.tag_type_name tag.Lambda.tag_index in
    (ExpressionVariable (Singleton_registry.var_name tag.Lambda.tag_type_name tag.Lambda.tag_index), ctx)

  | Lambda.LambdaConstructor (tag, arg) ->
    let fields = [FieldNamed ("_tag", ExpressionNumber (float_of_int tag.Lambda.tag_index))] in
    let fields, ctx = match arg with
      | None -> (fields, ctx)
      | Some arg_expr ->
        let translated, ctx = translate_expression ctx arg_expr in
        (fields @ [FieldNamed ("_0", translated)], ctx)
    in
    (ExpressionTable fields, ctx)

  | Lambda.LambdaMakeRecord field_bindings ->
    let translated_fields, ctx = List.fold_left (fun (acc, ctx) (field_name, field_value) ->
      let translated, ctx = translate_expression ctx field_value in
      (FieldNamed (field_name, translated) :: acc, ctx)
    ) ([], ctx) field_bindings in
    (ExpressionTable (List.rev translated_fields), ctx)

  | Lambda.LambdaGetRecordField (field_name, record_expression) ->
    let translated_record, ctx = translate_expression ctx record_expression in
    (ExpressionField (translated_record, field_name), ctx)

  | Lambda.LambdaRecordUpdate (base_expression, update_fields) ->
    let func_body, ctx = translate_to_statements ctx (Lambda.LambdaRecordUpdate (base_expression, update_fields)) in
    (ExpressionCall (ExpressionFunction ([], func_body), []), ctx)

  | Lambda.LambdaModule bindings ->
    (* Module as a Lua table with named fields *)
    let fields, ctx = List.fold_left (fun (acc, ctx) (binding : Lambda.module_binding) ->
      let translated, ctx = translate_expression ctx binding.mb_value in
      (FieldNamed (Lambda.module_binding_name binding, translated) :: acc, ctx)
    ) ([], ctx) bindings in
    (ExpressionTable (List.rev fields), ctx)

  | Lambda.LambdaModuleAccess (module_expr, field_name) ->
    (* Module access as field access: M.x *)
    let translated_module, ctx = translate_expression ctx module_expr in
    (ExpressionField (translated_module, field_name), ctx)

  | Lambda.LambdaFunctor (param_id, body) ->
    (* Functor as a function: functor (X : S) -> ME becomes function(X) return ME end *)
    let param_name = mangle_identifier param_id in
    let body_stmts, ctx = translate_to_statements ctx body in
    (ExpressionFunction ([param_name], body_stmts), ctx)

  | Lambda.LambdaFunctorApply (func_expr, arg_expr) ->
    (* Functor application: F(X) becomes F(X) function call *)
    let translated_func, ctx = translate_expression ctx func_expr in
    let translated_arg, ctx = translate_expression ctx arg_expr in
    (ExpressionCall (translated_func, [translated_arg]), ctx)

  | Lambda.LambdaExternalCall (spec, args) ->
    (* Translate arguments first *)
    let translated_args, ctx = translate_expression_list ctx args in
    (* Generate the FFI call based on the spec *)
    let call_expr = generate_ffi_call spec translated_args in
    (* Wrap result if @return(nullable) is specified *)
    if spec.Typing_ffi.Types.ffi_return_nullable then
      (* Generate:
         (function()
           local _result = ffi_call()
           if _result == nil then
             return {_tag = 0}  -- None
           else
             return {_tag = 1, _0 = _result}  -- Some
           end
         end)() *)
      let result_name = "_ffi_result" in
      let result_var = ExpressionVariable result_name in
      let none_expr = ExpressionTable [FieldNamed ("_tag", ExpressionNumber 0.0)] in
      let some_expr = ExpressionTable [
        FieldNamed ("_tag", ExpressionNumber 1.0);
        FieldNamed ("_0", result_var)
      ] in
      let result_decl = StatementLocal ([result_name], [call_expr]) in
      let nil_check = ExpressionBinaryOp (OpEqual, result_var, ExpressionNil) in
      let if_stmt = StatementIf (
        [(nil_check, [StatementReturn [none_expr]])],
        Some [StatementReturn [some_expr]]
      ) in
      let iife = ExpressionCall (ExpressionFunction ([], [result_decl; if_stmt]), []) in
      (iife, ctx)
    else
      (call_expr, ctx)

  (* Reference operations *)
  | Lambda.LambdaRef inner ->
    (* ref e -> {value = e} *)
    let inner_expr, ctx = translate_expression ctx inner in
    (ExpressionTable [FieldNamed ("value", inner_expr)], ctx)

  | Lambda.LambdaDeref ref_expr ->
    (* !e -> e.value *)
    let ref_lua, ctx = translate_expression ctx ref_expr in
    (ExpressionField (ref_lua, "value"), ctx)

  | Lambda.LambdaAssign (ref_expr, value_expr) ->
    (* e1 := e2 -> (function() e1.value = e2; return nil end)() *)
    (* We need to use an IIFE since assignment is a statement in Lua *)
    let ref_lua, ctx = translate_expression ctx ref_expr in
    let value_lua, ctx = translate_expression ctx value_expr in
    let lvalue = LvalueField (ref_lua, "value") in
    let assign_stmt = StatementAssign ([lvalue], [value_lua]) in
    let return_nil = StatementReturn [ExpressionNil] in
    let iife = ExpressionCall (ExpressionFunction ([], [assign_stmt; return_nil]), []) in
    (iife, ctx)

  | Lambda.LambdaPolyVariant (tag, arg) ->
    (* `Tag -> {_tag = "Tag"}
       `Tag x -> {_tag = "Tag", _0 = x} *)
    begin match arg with
    | None ->
      (* Nullary poly variant - just the tag *)
      let table = ExpressionTable [
        FieldNamed ("_tag", ExpressionString tag);
      ] in
      (table, ctx)
    | Some arg_expr ->
      (* Poly variant with argument *)
      let arg_lua, ctx = translate_expression ctx arg_expr in
      let table = ExpressionTable [
        FieldNamed ("_tag", ExpressionString tag);
        FieldNamed ("_0", arg_lua);
      ] in
      (table, ctx)
    end

(** Helper to translate a list of expressions.
    Uses cons + reverse for O(n) instead of O(n²) list append. *)
and translate_expression_list ctx exprs =
  let reversed, ctx = List.fold_left (fun (acc, ctx) expr ->
    let translated, ctx = translate_expression ctx expr in
    (translated :: acc, ctx)
  ) ([], ctx) exprs in
  (List.rev reversed, ctx)

(** Translate a value and assign it to a variable, threading context *)
and translate_value_to_assignment ctx name lambda : block * context =
  match lambda with
  | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
    (* Nested let: float it out and then assign *)
    let inner_name = mangle_identifier inner_id in
    let inner_expr, ctx = translate_expression ctx inner_value in
    let inner_decl = StatementLocal ([inner_name], [inner_expr]) in
    let rest_stmts, ctx = translate_value_to_assignment ctx name inner_body in
    (inner_decl :: rest_stmts, ctx)

  | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
    (* Nested if: generate if statement with assignments *)
    let cond_expr, ctx = translate_expression ctx cond in
    let then_stmts, ctx = translate_value_to_assignment ctx name then_branch in
    let else_stmts, ctx = translate_value_to_assignment ctx name else_branch in
    ([StatementIf ([(cond_expr, then_stmts)], Some else_stmts)], ctx)

  | Lambda.LambdaSequence (first, second) ->
    let first_stmts, ctx = translate_to_effect ctx first in
    let rest_stmts, ctx = translate_value_to_assignment ctx name second in
    (first_stmts @ rest_stmts, ctx)

  | _ ->
    let expr, ctx = translate_expression ctx lambda in
    ([StatementAssign ([LvalueVariable name], [expr])], ctx)

(** Dispatch table switch translation for many cases, threading context *)
and translate_switch_as_dispatch ctx _scrutinee_name tag_access cases default =
  (* Generate:
     local _dispatch = {
       [0] = function() ... end,
       [1] = function() ... end,
     }
     local _handler = _dispatch[scrutinee._tag]
     if _handler then return _handler() else <default> end
  *)
  let dispatch_name = Codegen_constants.dispatch_table_name in
  let handler_name = Codegen_constants.dispatch_handler_name in
  (* Use cons + reverse for O(n) instead of O(n²) list append *)
  let dispatch_entries_rev, ctx = List.fold_left (fun (acc, ctx) (case : Lambda.switch_case) ->
    let handler_body, ctx = translate_to_statements ctx case.switch_body in
    let handler = ExpressionFunction ([], handler_body) in
    let entry = FieldIndexed (ExpressionNumber (float_of_int case.switch_tag), handler) in
    (entry :: acc, ctx)
  ) ([], ctx) cases in
  let dispatch_entries = List.rev dispatch_entries_rev in
  let dispatch_table = ExpressionTable dispatch_entries in
  let dispatch_decl = StatementLocal ([dispatch_name], [dispatch_table]) in
  let handler_lookup = ExpressionIndex (ExpressionVariable dispatch_name, tag_access) in
  let handler_decl = StatementLocal ([handler_name], [handler_lookup]) in
  let handler_call = ExpressionCall (ExpressionVariable handler_name, []) in
  let default_stmts, ctx = match default with
    | Some d -> translate_to_statements ctx d
    | None -> ([StatementReturn [ExpressionCall (ExpressionVariable "error",
                [ExpressionString "Match failure"])]], ctx)
  in
  ([
    dispatch_decl;
    handler_decl;
    StatementIf (
      [(ExpressionVariable handler_name, [StatementReturn [handler_call]])],
      Some default_stmts
    )
  ], ctx)

(** Translate a lambda to a block of statements, threading context *)
and translate_to_statements ctx (lambda : Lambda.lambda) : block * context =
  match lambda with
  | Lambda.LambdaLet (id, value, body) ->
    begin match value with
    | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
      (* Float nested let out to avoid IIFE:
         let id = (let inner = v in e) in body
         becomes: let inner = v in let id = e in body *)
      translate_to_statements ctx (Lambda.LambdaLet (inner_id, inner_value,
        Lambda.LambdaLet (id, inner_body, body)))

    | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
      (* Transform if-in-value to avoid IIFE:
         let id = if c then e1 else e2 in body
         becomes: local id; if c then id = e1 else id = e2 end; body *)
      let name = mangle_identifier id in
      let decl = StatementLocal ([name], []) in
      let cond_expr, ctx = translate_expression ctx cond in
      let then_stmts, ctx = translate_value_to_assignment ctx name then_branch in
      let else_stmts, ctx = translate_value_to_assignment ctx name else_branch in
      let if_stmt = StatementIf ([(cond_expr, then_stmts)], Some else_stmts) in
      let rest, ctx = translate_to_statements ctx body in
      (decl :: if_stmt :: rest, ctx)

    | Lambda.LambdaSequence (first, second) ->
      (* Float sequence to avoid IIFE:
         let id = (e1; e2) in body
         becomes: e1; let id = e2 in body *)
      let first_stmts, ctx = translate_to_effect ctx first in
      let rest, ctx = translate_to_statements ctx (Lambda.LambdaLet (id, second, body)) in
      (first_stmts @ rest, ctx)

    | _ ->
      let name = mangle_identifier id in
      let value_expr, ctx = translate_expression ctx value in
      let local_stmt = StatementLocal ([name], [value_expr]) in
      let rest, ctx = translate_to_statements ctx body in
      (local_stmt :: rest, ctx)
    end

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let rev_assignments, ctx =
      List.fold_left (fun (acc, ctx) (id, value) ->
        let name = mangle_identifier id in
        let value_expr, ctx = translate_expression ctx value in
        (StatementAssign ([LvalueVariable name], [value_expr]) :: acc, ctx)
      ) ([], ctx) bindings
    in
    let rest, ctx = translate_to_statements ctx body in
    (forward_decl :: List.rev_append rev_assignments rest, ctx)

  | Lambda.LambdaIfThenElse (cond, then_branch, else_branch) ->
    let cond_expr, ctx = translate_expression ctx cond in
    let then_stmts, ctx = translate_to_statements ctx then_branch in
    let else_stmts, ctx = translate_to_statements ctx else_branch in
    ([StatementIf ([(cond_expr, then_stmts)], Some else_stmts)], ctx)

  | Lambda.LambdaSequence (first, second) ->
    let first_stmts, ctx = translate_to_effect ctx first in
    let second_stmts, ctx = translate_to_statements ctx second in
    (first_stmts @ second_stmts, ctx)

  | Lambda.LambdaSwitch (scrutinee, cases, default) ->
    let scrutinee_expr, ctx = translate_expression ctx scrutinee in
    let temp_name = Codegen_constants.switch_scrutinee_name in
    let local_stmt = StatementLocal ([temp_name], [scrutinee_expr]) in
    let tag_access = ExpressionField (ExpressionVariable temp_name, "_tag") in
    let num_cases = List.length cases in
    (* Use dispatch table for many cases, otherwise use if-chain *)
    if num_cases >= Codegen_constants.dispatch_table_threshold then
      let stmts, ctx = translate_switch_as_dispatch ctx temp_name tag_access cases default in
      (local_stmt :: stmts, ctx)
    else
      let rev_branches, ctx =
        List.fold_left (fun (acc, ctx) (case : Lambda.switch_case) ->
          let body, ctx = translate_to_statements ctx case.switch_body in
          let cond = ExpressionBinaryOp (OpEqual, tag_access,
            ExpressionNumber (float_of_int case.switch_tag)) in
          ((cond, body) :: acc, ctx)
        ) ([], ctx) cases
      in
      let branches = List.rev rev_branches in
      let default_stmts, ctx = match default with
        | Some d ->
          let stmts, ctx = translate_to_statements ctx d in
          (Some stmts, ctx)
        | None -> (None, ctx)
      in
      ([local_stmt; StatementIf (branches, default_stmts)], ctx)

  | Lambda.LambdaRecordUpdate (base_expression, update_fields) ->
    let base_expr, ctx = translate_expression ctx base_expression in
    let result_name = Codegen_constants.record_update_result_name in
    let key_name = Codegen_constants.pair_key_name in
    let value_name = Codegen_constants.pair_value_name in
    let copy_stmt = StatementLocal ([result_name], [ExpressionTable []]) in
    let copy_loop = StatementForIn ([key_name; value_name],
      [ExpressionCall (ExpressionVariable "pairs", [base_expr])],
      [StatementAssign ([LvalueIndex (ExpressionVariable result_name, ExpressionVariable key_name)],
                        [ExpressionVariable value_name])]) in
    let rev_update_stmts, ctx = List.fold_left (fun (acc, ctx) (field_name, field_value) ->
      let translated, ctx = translate_expression ctx field_value in
      (StatementAssign ([LvalueField (ExpressionVariable result_name, field_name)],
                        [translated]) :: acc, ctx)
    ) ([], ctx) update_fields in
    let update_stmts = List.rev rev_update_stmts in
    (copy_stmt :: copy_loop :: update_stmts @ [StatementReturn [ExpressionVariable result_name]], ctx)

  | _ ->
    let expr, ctx = translate_expression ctx lambda in
    ([StatementReturn [expr]], ctx)

(** Translate a lambda for side effects only (no return value), threading context *)
and translate_to_effect ctx (lambda : Lambda.lambda) : block * context =
  match lambda with
  | Lambda.LambdaApply (func, args) ->
    let func_expr, ctx = translate_expression ctx func in
    let arg_exprs, ctx = translate_expression_list ctx args in
    ([StatementCall (func_expr, arg_exprs)], ctx)

  | Lambda.LambdaPrimitive (Lambda.PrimitivePrint, args) ->
    let arg_exprs, ctx = translate_expression_list ctx args in
    ([StatementCall (ExpressionVariable "print", arg_exprs)], ctx)

  | Lambda.LambdaSequence (first, second) ->
    let first_stmts, ctx = translate_to_effect ctx first in
    let second_stmts, ctx = translate_to_effect ctx second in
    (first_stmts @ second_stmts, ctx)

  | Lambda.LambdaLet (id, value, body) ->
    let name = mangle_identifier id in
    let value_expr, ctx = translate_expression ctx value in
    let rest, ctx = translate_to_effect ctx body in
    (StatementLocal ([name], [value_expr]) :: rest, ctx)

  | Lambda.LambdaLetRecursive (bindings, body) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let rev_assignments, ctx =
      List.fold_left (fun (acc, ctx) (id, value) ->
        let name = mangle_identifier id in
        let value_expr, ctx = translate_expression ctx value in
        (StatementAssign ([LvalueVariable name], [value_expr]) :: acc, ctx)
      ) ([], ctx) bindings
    in
    let rest, ctx = translate_to_effect ctx body in
    (forward_decl :: List.rev_append rev_assignments rest, ctx)

  | _ ->
    let expr, ctx = translate_expression ctx lambda in
    match expr with
    | ExpressionCall (func, args) -> ([StatementCall (func, args)], ctx)
    | _ -> ([], ctx)

(** Generate top-level binding with let floating, threading context *)
let rec generate_top_level_binding ctx (target_name : identifier) (value : Lambda.lambda)
    : block * context =
  match value with
  | Lambda.LambdaFunction (params, body) ->
    let param_names = List.map mangle_identifier params in
    let body_stmts, ctx = translate_to_statements ctx body in
    ([StatementLocalFunction (target_name, param_names, body_stmts)], ctx)

  | Lambda.LambdaLet (inner_id, inner_value, inner_body) ->
    (* Float nested let out:
       let target = (let inner = v in e)
       becomes: let inner = v; let target = e *)
    let inner_name = mangle_identifier inner_id in
    let inner_stmts, ctx = generate_top_level_binding ctx inner_name inner_value in
    let rest_stmts, ctx = generate_top_level_binding ctx target_name inner_body in
    (inner_stmts @ rest_stmts, ctx)

  | Lambda.LambdaIfThenElse (condition, then_branch, else_branch) ->
    (* Transform if-in-value:
       let target = if c then e1 else e2
       becomes: local target; if c then target = e1 else target = e2 end *)
    let declaration = StatementLocal ([target_name], []) in
    let condition_expr, ctx = translate_expression ctx condition in
    let then_stmts, ctx = translate_value_to_assignment ctx target_name then_branch in
    let else_stmts, ctx = translate_value_to_assignment ctx target_name else_branch in
    let if_statement = StatementIf ([(condition_expr, then_stmts)], Some else_stmts) in
    ([declaration; if_statement], ctx)

  | Lambda.LambdaSequence (first_expr, second_expr) ->
    (* Float sequence:
       let target = (e1; e2)
       becomes: e1; let target = e2 *)
    let first_stmts, ctx = translate_to_effect ctx first_expr in
    let rest_stmts, ctx = generate_top_level_binding ctx target_name second_expr in
    (first_stmts @ rest_stmts, ctx)

  | Lambda.LambdaModule bindings ->
    (* Generate module bindings as local variables first, then create the table.
       This ensures bindings that reference earlier bindings work correctly.

       Instead of: local M = {id = ..., applied = id_17(id_17)}
       Generate:   local id_17 = ...
                   local applied_18 = id_17(id_17)
                   local M = {id = id_17, applied = applied_18}

       We use mb_id (the original identifier) so references between bindings resolve correctly.

       Note: We accumulate statements in reverse order using List.rev_append for O(n)
       complexity instead of O(n²) from repeated @ operations.
    *)
    let rev_binding_stmts, binding_names, ctx = List.fold_left (fun (rev_stmts, names, ctx) (binding : Lambda.module_binding) ->
      (* Use the original identifier so references between bindings work *)
      let binding_var = mangle_identifier binding.mb_id in
      let inner_stmts, ctx = generate_top_level_binding ctx binding_var binding.mb_value in
      (List.rev_append inner_stmts rev_stmts, (Lambda.module_binding_name binding, binding_var) :: names, ctx)
    ) ([], [], ctx) bindings in
    let binding_stmts = List.rev rev_binding_stmts in
    let binding_names = List.rev binding_names in
    (* Create table with references to the local variables *)
    let fields = List.map (fun (name, var) ->
      FieldNamed (name, ExpressionVariable var)
    ) binding_names in
    let table_stmt = StatementLocal ([target_name], [ExpressionTable fields]) in
    (binding_stmts @ [table_stmt], ctx)

  | _ ->
    let value_expr, ctx = translate_expression ctx value in
    ([StatementLocal ([target_name], [value_expr])], ctx)

(** Generate statements for a top-level lambda, threading context *)
let generate_top_level ctx (lambda : Lambda.lambda) : block * context =
  match lambda with
  | Lambda.LambdaLet (identifier, value, Lambda.LambdaConstant Lambda.ConstantUnit) ->
    let target_name = mangle_identifier identifier in
    generate_top_level_binding ctx target_name value

  | Lambda.LambdaLetRecursive (bindings, Lambda.LambdaConstant Lambda.ConstantUnit) ->
    let names = List.map (fun (id, _) -> mangle_identifier id) bindings in
    let forward_decl = StatementLocal (names, []) in
    let rev_function_defs, ctx =
      List.fold_left (fun (acc, ctx) (id, value) ->
        let name = mangle_identifier id in
        match value with
        | Lambda.LambdaFunction (params, body) ->
          let param_names = List.map mangle_identifier params in
          let body_stmts, ctx = translate_to_statements ctx body in
          let stmt = StatementAssign ([LvalueVariable name],
            [ExpressionFunction (param_names, body_stmts)]) in
          (stmt :: acc, ctx)
        | _ ->
          let value_expr, ctx = translate_expression ctx value in
          let stmt = StatementAssign ([LvalueVariable name], [value_expr]) in
          (stmt :: acc, ctx)
      ) ([], ctx) bindings
    in
    (forward_decl :: List.rev rev_function_defs, ctx)

  | _ ->
    translate_to_effect ctx lambda

(** Generate Lua code from a list of Lambda IR nodes.

    This is the main entry point for code generation. It uses an empty
    context and threads it through all translation functions, collecting
    singleton registrations along the way.

    Note: We accumulate statements in reverse order using List.rev_append for O(n)
    complexity instead of O(n²) from repeated @ operations.

    @param lambdas The Lambda IR nodes to translate
    @return A Lua chunk (list of statements) *)
let generate lambdas =
  let rev_body, final_ctx =
    List.fold_left (fun (rev_acc, ctx) lambda ->
      let stmts, ctx = generate_top_level ctx lambda in
      (List.rev_append stmts rev_acc, ctx)
    ) ([], empty_context) lambdas
  in
  let body = List.rev rev_body in
  let preamble = generate_singleton_preamble final_ctx in
  preamble @ body
