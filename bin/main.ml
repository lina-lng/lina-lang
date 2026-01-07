open Cmdliner

let compile_single options input_file output_file =
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

let compile_multi options input_files output_dir =
  let project_options = Driver.Multifile.{
    output_dir;
    entry_point = None;
    pipeline_options = options;
  } in
  match Driver.Multifile.compile_project project_options input_files with
  | Ok outputs ->
    begin match output_dir with
    | Some dir ->
      (* Create output directory if needed *)
      if not (Sys.file_exists dir) then
        Sys.mkdir dir 0o755;
      (* Write each module to its own file *)
      List.iter (fun (module_name, lua_code) ->
        let filename = Filename.concat dir (String.lowercase_ascii module_name ^ ".lua") in
        let oc = open_out filename in
        output_string oc lua_code;
        close_out oc;
        Printf.printf "Wrote %s\n" filename
      ) outputs;
      `Ok ()
    | None ->
      (* Print all modules to stdout with separators *)
      List.iter (fun (module_name, lua_code) ->
        Printf.printf "-- ========== %s ==========\n%s\n" module_name lua_code
      ) outputs;
      `Ok ()
    end
  | Error msg ->
    Printf.eprintf "%s\n" msg;
    `Error (false, "Compilation failed")

let compile input_file output_file multi_files output_dir dump_ast dump_typed dump_lambda =
  let options = Driver.Pipeline.{
    dump_ast;
    dump_typed;
    dump_lambda;
  } in
  match multi_files with
  | [] ->
    (* Single file mode *)
    compile_single options input_file output_file
  | files ->
    (* Multi-file mode *)
    compile_multi options (input_file :: files) output_dir

let input_file =
  let doc = "The Lina source file to compile." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let output_file =
  let doc = "Write output to $(docv) instead of stdout (single-file mode)." in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)

let multi_files =
  let doc = "Additional files for multi-file compilation." in
  Arg.(value & opt_all file [] & info ["m"; "multi"] ~docv:"FILE" ~doc)

let output_dir =
  let doc = "Output directory for multi-file compilation." in
  Arg.(value & opt (some string) None & info ["d"; "outdir"] ~docv:"DIR" ~doc)

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
  let man = [
    `S Manpage.s_description;
    `P "Compiles Lina source files to Lua. In single-file mode, compiles one \
        file to stdout or a specified output file. In multi-file mode (using \
        -m flags), compiles multiple files with dependency tracking.";
    `S Manpage.s_examples;
    `P "Single file:";
    `Pre "  linac file.lina";
    `Pre "  linac file.lina -o output.lua";
    `P "Multi-file project:";
    `Pre "  linac main.lina -m lib.lina -m util.lina -d dist/";
  ] in
  let info = Cmd.info "linac" ~version:"0.1.0" ~doc ~man in
  Cmd.v info Term.(ret (const compile $ input_file $ output_file $ multi_files $ output_dir $ dump_ast $ dump_typed $ dump_lambda))

let () = exit (Cmd.eval cmd)
