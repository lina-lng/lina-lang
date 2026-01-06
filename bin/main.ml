open Cmdliner

let compile input_file output_file dump_ast dump_typed dump_lambda =
  let options = Driver.Pipeline.{
    dump_ast;
    dump_typed;
    dump_lambda;
  } in
  match Driver.Pipeline.compile_file options input_file with
  | Ok lua_code ->
    begin match output_file with
    | Some filename ->
      let oc = open_out filename in
      output_string oc lua_code;
      close_out oc;
      `Ok ()
    | None ->
      print_string lua_code;
      `Ok ()
    end
  | Error msg ->
    Printf.eprintf "%s\n" msg;
    `Error (false, "Compilation failed")

let input_file =
  let doc = "The Lina source file to compile." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let output_file =
  let doc = "Write output to $(docv) instead of stdout." in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)

let dump_ast =
  let doc = "Dump the parsed AST to stderr." in
  Arg.(value & flag & info ["dump-ast"] ~doc)

let dump_typed =
  let doc = "Dump the typed AST to stderr." in
  Arg.(value & flag & info ["dump-typed"] ~doc)

let dump_lambda =
  let doc = "Dump the lambda IR to stderr." in
  Arg.(value & flag & info ["dump-lambda"] ~doc)

let cmd =
  let doc = "Compile Lina source code to Lua" in
  let info = Cmd.info "linac" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(ret (const compile $ input_file $ output_file $ dump_ast $ dump_typed $ dump_lambda))

let () = exit (Cmd.eval cmd)
