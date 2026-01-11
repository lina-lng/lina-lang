open Cmdliner

(** {1 Compile Command} *)

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

let compile_input_file =
  let doc = "The Lina source file to compile." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let compile_output_file =
  let doc = "Write output to $(docv) instead of stdout (single-file mode)." in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)

let compile_multi_files =
  let doc = "Additional files for multi-file compilation." in
  Arg.(value & opt_all file [] & info ["m"; "multi"] ~docv:"FILE" ~doc)

let compile_output_dir =
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

let compile_cmd =
  let doc = "Compile Lina source code to Lua" in
  let man = [
    `S Manpage.s_description;
    `P "Compiles Lina source files to Lua. In single-file mode, compiles one \
        file to stdout or a specified output file. In multi-file mode (using \
        -m flags), compiles multiple files with dependency tracking.";
    `S Manpage.s_examples;
    `P "Single file:";
    `Pre "  linac compile file.lina";
    `Pre "  linac compile file.lina -o output.lua";
    `P "Multi-file project:";
    `Pre "  linac compile main.lina -m lib.lina -m util.lina -d dist/";
  ] in
  let info = Cmd.info "compile" ~doc ~man in
  Cmd.v info Term.(ret (const compile $ compile_input_file $ compile_output_file $
                        compile_multi_files $ compile_output_dir $
                        dump_ast $ dump_typed $ dump_lambda))

(** {1 Format Command} *)

let format_file input_file in_place check width =
  let config = Lina_format.Formatter.{
    line_width = width;
    indent_size = 2;
  } in
  if check then begin
    (* Check mode: exit 1 if file needs formatting *)
    match Lina_format.Formatter.check_file_cst ~config input_file with
    | Ok true ->
        `Ok ()
    | Ok false ->
        Printf.eprintf "%s: needs formatting\n" input_file;
        `Error (false, "File needs formatting")
    | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        `Error (false, "Check failed")
  end else if in_place then begin
    (* In-place mode: modify file *)
    match Lina_format.Formatter.format_file_cst ~config ~in_place:true input_file with
    | Ok _ ->
        Printf.printf "Formatted %s\n" input_file;
        `Ok ()
    | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        `Error (false, "Formatting failed")
  end else begin
    (* Default: output to stdout *)
    match Lina_format.Formatter.format_file_cst ~config input_file with
    | Ok formatted ->
        print_string formatted;
        `Ok ()
    | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        `Error (false, "Formatting failed")
  end

let format_input_file =
  let doc = "The Lina source file to format." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let format_in_place =
  let doc = "Modify the file in place instead of printing to stdout." in
  Arg.(value & flag & info ["i"; "in-place"] ~doc)

let format_check =
  let doc = "Check if the file is formatted (exit 1 if not). Useful for CI." in
  Arg.(value & flag & info ["check"] ~doc)

let format_width =
  let doc = "Target line width for formatting." in
  Arg.(value & opt int 80 & info ["w"; "width"] ~docv:"WIDTH" ~doc)

let format_cmd =
  let doc = "Format Lina source code" in
  let man = [
    `S Manpage.s_description;
    `P "Formats Lina source files using a consistent style. By default, \
        outputs formatted code to stdout. Use -i to modify files in place, \
        or --check to verify formatting without modifying files.";
    `S Manpage.s_examples;
    `P "Format to stdout:";
    `Pre "  linac format file.lina";
    `P "Format in place:";
    `Pre "  linac format -i file.lina";
    `P "Check formatting (for CI):";
    `Pre "  linac format --check file.lina";
    `P "Custom line width:";
    `Pre "  linac format -w 100 file.lina";
  ] in
  let info = Cmd.info "format" ~doc ~man in
  Cmd.v info Term.(ret (const format_file $ format_input_file $
                        format_in_place $ format_check $ format_width))

(** {1 Main Command Group} *)

let default_cmd =
  let doc = "Lina compiler and tools" in
  let man = [
    `S Manpage.s_description;
    `P "Lina is an ML-family language that compiles to Lua.";
    `S Manpage.s_commands;
    `P "Use $(b,linac compile) to compile Lina source to Lua.";
    `P "Use $(b,linac format) to format Lina source code.";
  ] in
  let info = Cmd.info "linac" ~version:"0.1.0" ~doc ~man in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [compile_cmd; format_cmd]

let () = exit (Cmd.eval default_cmd)
