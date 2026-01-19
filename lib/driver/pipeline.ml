open Common

type options = {
  dump_ast : bool;
  dump_typed : bool;
  dump_lambda : bool;
  warning_config : Common.Warning_config.t;
}

let default_options = {
  dump_ast = false;
  dump_typed = false;
  dump_lambda = false;
  warning_config = Common.Warning_config.default;
}

let dump_typed_structure typed_ast =
  let open Typing.Typed_tree in
  let indent n = String.make (n * 2) ' ' in
  let rec dump_expr depth expr =
    let ty = Typing.Types.type_expression_to_string expr.expression_type in
    match expr.expression_desc with
    | TypedExpressionVariable (id, _) ->
      Printf.sprintf "%s(var %s : %s)" (indent depth) (Identifier.name id) ty
    | TypedExpressionConstant c ->
      let c_str = match c with
        | Parsing.Syntax_tree.ConstantInteger n -> string_of_int n
        | Parsing.Syntax_tree.ConstantFloat f -> string_of_float f
        | Parsing.Syntax_tree.ConstantString s -> Printf.sprintf "%S" s
        | Parsing.Syntax_tree.ConstantBoolean b -> string_of_bool b
        | Parsing.Syntax_tree.ConstantUnit -> "()"
      in
      Printf.sprintf "%s(const %s : %s)" (indent depth) c_str ty
    | TypedExpressionTuple exprs ->
      Printf.sprintf "%s(tuple : %s)\n%s" (indent depth) ty
        (String.concat "\n" (List.map (dump_expr (depth + 1)) exprs))
    | TypedExpressionApply (fn, labeled_args) ->
      Printf.sprintf "%s(apply : %s)\n%s\n%s" (indent depth) ty
        (dump_expr (depth + 1) fn)
        (String.concat "\n" (List.map (fun (_, e) -> dump_expr (depth + 1) e) labeled_args))
    | TypedExpressionFunction (labeled_params, body) ->
      let param_name (_, p) = match p.pattern_desc with
        | TypedPatternVariable id -> Identifier.name id
        | _ -> "_"
      in
      let param_names = List.map param_name labeled_params in
      Printf.sprintf "%s(fun [%s] : %s)\n%s" (indent depth)
        (String.concat ", " param_names) ty (dump_expr (depth + 1) body)
    | TypedExpressionLet (_, bindings, body) ->
      Printf.sprintf "%s(let : %s)\n%s\n%s" (indent depth) ty
        (String.concat "\n" (List.map (dump_binding (depth + 1)) bindings))
        (dump_expr (depth + 1) body)
    | TypedExpressionIf (cond, then_e, else_e) ->
      let else_str = match else_e with
        | Some e -> "\n" ^ dump_expr (depth + 1) e
        | None -> ""
      in
      Printf.sprintf "%s(if : %s)\n%s\n%s%s" (indent depth) ty
        (dump_expr (depth + 1) cond) (dump_expr (depth + 1) then_e) else_str
    | TypedExpressionMatch (scrutinee, arms) ->
      Printf.sprintf "%s(match : %s)\n%s\n%s" (indent depth) ty
        (dump_expr (depth + 1) scrutinee)
        (String.concat "\n" (List.map (dump_arm (depth + 1)) arms))
    | _ -> Printf.sprintf "%s(expr : %s)" (indent depth) ty
  and dump_binding depth binding =
    let name = match binding.binding_pattern.pattern_desc with
      | TypedPatternVariable id -> Identifier.name id
      | _ -> "_"
    in
    Printf.sprintf "%s(binding %s)\n%s" (indent depth) name
      (dump_expr (depth + 1) binding.binding_expression)
  and dump_arm depth arm =
    Printf.sprintf "%s(arm)\n%s" (indent depth)
      (dump_expr (depth + 1) arm.typed_arm_expression)
  in
  let dump_item item =
    match item.structure_item_desc with
    | TypedStructureValue (_, bindings) ->
      Printf.sprintf "(value)\n%s"
        (String.concat "\n" (List.map (dump_binding 1) bindings))
    | TypedStructureType decls ->
      Printf.sprintf "(type %s)"
        (String.concat ", " (List.map (fun d -> d.Typing.Types.declaration_name) decls))
    | TypedStructureModule (id, _) ->
      Printf.sprintf "(module %s)" (Identifier.name id)
    | _ -> "(structure-item)"
  in
  String.concat "\n\n" (List.map dump_item typed_ast)

let dump_lambda_ir lambdas =
  let indent n = String.make (n * 2) ' ' in
  let rec dump depth = function
    | Lambda.LambdaVariable id ->
      Printf.sprintf "%s(var %s)" (indent depth) (Identifier.name id)
    | Lambda.LambdaConstant c ->
      let c_str = match c with
        | Lambda.ConstantInt n -> string_of_int n
        | Lambda.ConstantFloat f -> string_of_float f
        | Lambda.ConstantString s -> Printf.sprintf "%S" s
        | Lambda.ConstantBool b -> string_of_bool b
        | Lambda.ConstantUnit -> "()"
      in
      Printf.sprintf "%s(const %s)" (indent depth) c_str
    | Lambda.LambdaApply (fn, args) ->
      Printf.sprintf "%s(apply)\n%s\n%s" (indent depth)
        (dump (depth + 1) fn)
        (String.concat "\n" (List.map (dump (depth + 1)) args))
    | Lambda.LambdaFunction (params, body) ->
      Printf.sprintf "%s(fun [%s])\n%s" (indent depth)
        (String.concat ", " (List.map Identifier.name params))
        (dump (depth + 1) body)
    | Lambda.LambdaLet (id, value, body) ->
      Printf.sprintf "%s(let %s)\n%s\n%s" (indent depth)
        (Identifier.name id) (dump (depth + 1) value) (dump (depth + 1) body)
    | Lambda.LambdaLetRecursive (bindings, body) ->
      Printf.sprintf "%s(letrec)\n%s\n%s" (indent depth)
        (String.concat "\n" (List.map (fun (id, v) ->
          Printf.sprintf "%s(%s)\n%s" (indent (depth + 1))
            (Identifier.name id) (dump (depth + 2) v)) bindings))
        (dump (depth + 1) body)
    | Lambda.LambdaPrimitive (_, args) ->
      Printf.sprintf "%s(prim)\n%s" (indent depth)
        (String.concat "\n" (List.map (dump (depth + 1)) args))
    | Lambda.LambdaIfThenElse (cond, then_e, else_e) ->
      Printf.sprintf "%s(if)\n%s\n%s\n%s" (indent depth)
        (dump (depth + 1) cond) (dump (depth + 1) then_e) (dump (depth + 1) else_e)
    | Lambda.LambdaConstructor (tag, arg) ->
      let arg_str = match arg with
        | Some a -> "\n" ^ dump (depth + 1) a
        | None -> ""
      in
      Printf.sprintf "%s(ctor %s)%s" (indent depth) tag.Lambda.tag_name arg_str
    | Lambda.LambdaMakeRecord fields ->
      Printf.sprintf "%s(record)\n%s" (indent depth)
        (String.concat "\n" (List.map (fun (name, v) ->
          Printf.sprintf "%s(%s)\n%s" (indent (depth + 1)) name
            (dump (depth + 2) v)) fields))
    | Lambda.LambdaSwitch (scrutinee, cases, default) ->
      let default_str = match default with
        | Some d -> Printf.sprintf "\n%s(default)\n%s" (indent (depth + 1)) (dump (depth + 2) d)
        | None -> ""
      in
      Printf.sprintf "%s(switch)\n%s\n%s%s" (indent depth)
        (dump (depth + 1) scrutinee)
        (String.concat "\n" (List.map (fun c ->
          Printf.sprintf "%s(case %d)\n%s" (indent (depth + 1))
            c.Lambda.switch_tag (dump (depth + 2) c.Lambda.switch_body)) cases))
        default_str
    | _ -> Printf.sprintf "%s(lambda-expr)" (indent depth)
  in
  String.concat "\n\n" (List.map (dump 0) lambdas)

let conversion_hints = [
  ("int", "string"), "int_of_string value";
  ("string", "int"), "string_of_int value";
  ("float", "string"), "float_of_string value";
  ("string", "float"), "string_of_float value";
  ("int", "float"), "int_of_float value` (truncates toward zero)";
  ("float", "int"), "float_of_int value";
  ("bool", "string"), "string_of_bool value";
  ("string", "bool"), "bool_of_string value";
]

let conversion_hint ~expected ~actual =
  let open Typing.Type_explain in
  match builtin_machine_name expected, builtin_machine_name actual with
  | Some expected_name, Some actual_name ->
    List.assoc_opt (expected_name, actual_name) conversion_hints
    |> Option.map (fun hint ->
         Printf.sprintf "To convert %s %s to %s %s, use `%s`"
           (article_for actual_name) actual_name
           (article_for expected_name) expected_name hint)
  | _ -> None

(** Create a diagnostic for a unification error with rich type information. *)
let diagnostic_of_unification_error ~expected ~actual ~location ~message ~trace =
  let expected_str = Typing.Types.type_expression_to_string expected in
  let actual_str = Typing.Types.type_expression_to_string actual in
  let trace_str = Typing.Unification.format_trace trace in

  let labels =
    if Location.is_none location then []
    else [{
      Compiler_error.label_span = location;
      label_message = Some (Printf.sprintf "expected `%s`, found `%s`" expected_str actual_str);
      is_primary = true;
    }]
  in

  let conversion = conversion_hint ~expected ~actual in

  (* Build notes: trace (if any), conversion hint (if any) *)
  let notes =
    (if trace_str <> "" then [String.trim trace_str] else []) @
    (match conversion with Some hint -> [hint] | None -> [])
  in

  Compiler_error.{
    severity = Error;
    code = Some Error_code.e_type_mismatch;
    message;
    labels;
    notes;
    suggestions = [];
  }

let compile_string options filename source =
  let sources = Diagnostic_render.create_source_cache () in
  Diagnostic_render.add_source sources ~path:filename ~content:source;
  let terminal = Diagnostic_render.detect_terminal Diagnostic_render.Auto in

  let render_diagnostic diag =
    Diagnostic_render.render_diagnostic
      ~format:Diagnostic_render.Human ~terminal ~sources diag
  in

  try
    let ast = Parsing.Parse.structure_from_string ~filename source in
    if options.dump_ast then begin
      Printf.eprintf "=== AST ===\n%s\n\n"
        (Parsing.Syntax_tree.show_structure ast)
    end;

    Typing.Types.reset_type_variable_id ();
    let env = Stdlib_loader.initial_with_stdlib () in
    let ctx = Typing.Typing_context.create env in
    let typed_ast, _ctx = Typing.Inference.infer_structure ctx ast in
    if options.dump_typed then begin
      Printf.eprintf "=== Typed AST ===\n%s\n\n" (dump_typed_structure typed_ast)
    end;

    (* Run static analysis: scope analysis, unused detection, dead code detection *)
    let analysis_result = Analysis.Analysis_pipeline.run options.warning_config typed_ast in
    let analysis_diagnostics = analysis_result.Analysis.Analysis_pipeline.diagnostics in

    let lambda = Lambda.translate_structure typed_ast in
    if options.dump_lambda then begin
      Printf.eprintf "=== Lambda IR ===\n%s\n\n" (dump_lambda_ir lambda)
    end;

    let lua_ast = Lua.Codegen.generate lambda in
    let user_code = Lua.Printer.print_chunk lua_ast in
    let lua_code = Stdlib_loader.stdlib_prelude () ^ user_code in

    (* Process type inference warnings with rich rendering *)
    let type_warnings = Compiler_error.get_warnings () in
    let has_error = ref false in

    List.iter (fun warning_info ->
      let code = Compiler_error.warning_code warning_info in
      if Common.Warning_config.should_report options.warning_config code then begin
        let is_error = Common.Warning_config.is_error options.warning_config code in
        let diag = Compiler_error.diagnostic_of_warning warning_info in
        let diag =
          if is_error then { diag with Compiler_error.severity = Compiler_error.Error }
          else diag
        in
        Printf.eprintf "%s" (render_diagnostic diag);
        if is_error then has_error := true
      end
    ) type_warnings;

    (* Process analysis diagnostics with rich rendering *)
    List.iter (fun diag ->
      Printf.eprintf "%s" (render_diagnostic diag);
      if diag.Compiler_error.severity = Compiler_error.Error then
        has_error := true
    ) analysis_diagnostics;

    if !has_error then
      Error "Compilation failed due to warnings treated as errors"
    else
      Ok lua_code
  with
  | Compiler_error.Error err ->
    let diag = Compiler_error.diagnostic_of_error err in
    Error (render_diagnostic diag)

  | Typing.Unification.Unification_error { expected; actual; location; message; trace } ->
    let diag = diagnostic_of_unification_error ~expected ~actual ~location ~message ~trace in
    Error (render_diagnostic diag)

let compile_file options filename =
  try
    let ic = open_in filename in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    compile_string options filename source
  with
  | Sys_error msg -> Error msg
