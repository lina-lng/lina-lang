open Common

type options = {
  dump_ast : bool;
  dump_typed : bool;
  dump_lambda : bool;
}

let default_options = {
  dump_ast = false;
  dump_typed = false;
  dump_lambda = false;
}

let compile_string options _filename source =
  try
    let ast = Parsing.Parse.structure_from_string source in
    if options.dump_ast then begin
      Printf.eprintf "=== AST ===\n%s\n\n"
        (Parsing.Syntax_tree.show_structure ast)
    end;

    Typing.Types.reset_level ();
    let typed_ast, _env = Typing.Inference.infer_structure Typing.Environment.initial ast in
    if options.dump_typed then begin
      Printf.eprintf "=== Typed AST ===\n";
      Printf.eprintf "(typed tree dump not implemented)\n\n"
    end;

    let lambda = Lambda.translate_structure typed_ast in
    if options.dump_lambda then begin
      Printf.eprintf "=== Lambda ===\n";
      Printf.eprintf "(lambda dump not implemented)\n\n"
    end;

    let lua_ast = Lua.Codegen.generate lambda in
    let lua_code = Lua.Printer.print_chunk lua_ast in
    Ok lua_code
  with
  | Compiler_error.Error err ->
    Error (Compiler_error.report_to_string err)
  | Typing.Unification.Unification_error { expected; actual; location; message } ->
    let loc_str =
      if Location.is_none location then ""
      else Printf.sprintf "File \"%s\", line %d, characters %d-%d:\n"
        location.start_pos.filename
        location.start_pos.line
        location.start_pos.column
        location.end_pos.column
    in
    Error (Printf.sprintf "%sType error: %s\nExpected: %s\nActual: %s"
      loc_str message
      (Typing.Types.type_expression_to_string expected)
      (Typing.Types.type_expression_to_string actual))

let compile_file options filename =
  try
    let ic = open_in filename in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    compile_string options filename source
  with
  | Sys_error msg -> Error msg
