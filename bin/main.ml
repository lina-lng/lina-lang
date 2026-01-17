open Cmdliner

(** {1 Common Options} *)

type error_format = Human | Short | Json

let error_format_conv =
  let parse = function
    | "human" -> Ok Human
    | "short" -> Ok Short
    | "json" -> Ok Json
    | s -> Error (`Msg (Printf.sprintf "Unknown error format: %s" s))
  in
  let print fmt = function
    | Human -> Format.fprintf fmt "human"
    | Short -> Format.fprintf fmt "short"
    | Json -> Format.fprintf fmt "json"
  in
  Arg.conv (parse, print)

type color_choice = Auto | Always | Never

let color_conv =
  let parse = function
    | "auto" -> Ok Auto
    | "always" -> Ok Always
    | "never" -> Ok Never
    | s -> Error (`Msg (Printf.sprintf "Unknown color choice: %s" s))
  in
  let print fmt = function
    | Auto -> Format.fprintf fmt "auto"
    | Always -> Format.fprintf fmt "always"
    | Never -> Format.fprintf fmt "never"
  in
  Arg.conv (parse, print)

let error_format_arg =
  let doc = "Output format for errors and warnings. $(docv) must be one of \
             $(b,human) (default, colored output), $(b,short) (one line per diagnostic), \
             or $(b,json) (machine-readable)." in
  Arg.(value & opt error_format_conv Human & info ["error-format"] ~docv:"FORMAT" ~doc)

let color_arg =
  let doc = "When to use colors. $(docv) must be $(b,auto) (default), $(b,always), \
             or $(b,never). Respects NO_COLOR and FORCE_COLOR environment variables." in
  Arg.(value & opt color_conv Auto & info ["color"] ~docv:"WHEN" ~doc)

let warning_spec_arg =
  let doc = "Warning configuration. Can be specified multiple times. \
             Use $(b,+name) to enable, $(b,-name) to disable, $(b,name=level) to set level \
             (allow/warn/deny/forbid). Use $(b,+all) or $(b,-all) for all warnings. \
             Examples: $(b,-W +all), $(b,-W -unused), $(b,-W shadowing=deny)." in
  Arg.(value & opt_all string [] & info ["W"] ~docv:"SPEC" ~doc)

let warn_error_arg =
  let doc = "Treat warnings as errors. Use $(b,+all) to treat all warnings as errors." in
  Arg.(value & opt (some string) None & info ["warn-error"] ~docv:"SPEC" ~doc)

let relaxed_arg =
  let doc = "Use relaxed mode: treat unused code warnings as warnings instead of errors. \
             By default, Lina uses strict mode where unused code is an error." in
  Arg.(value & flag & info ["relaxed"] ~doc)

(** {1 Compile Command} *)

let to_render_format = function
  | Human -> Driver.Diagnostic_render.Human
  | Short -> Driver.Diagnostic_render.Short
  | Json -> Driver.Diagnostic_render.Json

let to_color_choice = function
  | Auto -> Driver.Diagnostic_render.Auto
  | Always -> Driver.Diagnostic_render.Always
  | Never -> Driver.Diagnostic_render.Never

(** Parse warning specs and build configuration. Returns error message on failure.
    Strict mode (unused code = error) is the default. Use relaxed=true to opt out. *)
let parse_warning_config ~relaxed warning_specs warn_error_spec =
  let base = if relaxed then Common.Warning_config.relaxed else Common.Warning_config.default in
  match Common.Warning_config.parse_specs base warning_specs with
  | Error msg -> Error msg
  | Ok config ->
    match warn_error_spec with
    | None -> Ok config
    | Some spec ->
      if spec = "+all" || spec = "all" then
        Ok (Common.Warning_config.warn_error_all config)
      else
        Common.Warning_config.parse_spec config (spec ^ "=deny")

let compile_single options error_fmt color input_file output_file =
  let sources = Driver.Diagnostic_render.create_source_cache () in
  (* Load source file for error display *)
  let _ = Driver.Diagnostic_render.load_source_file sources input_file in

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
    (* Convert to diagnostic and render with new system *)
    let terminal = Driver.Diagnostic_render.detect_terminal (to_color_choice color) in
    let format = to_render_format error_fmt in
    let diag = Common.Compiler_error.(
      error ~code:Common.Error_code.e_type_mismatch msg
      |> with_primary_label ~span:Common.Location.none
    ) in
    let rendered = Driver.Diagnostic_render.render_diagnostic
      ~format ~terminal ~sources diag
    in
    Printf.eprintf "%s\n" rendered;
    `Error (false, "Compilation failed")

let compile_multi options error_fmt color input_files output_dir =
  let sources = Driver.Diagnostic_render.create_source_cache () in
  (* Load all source files for error display *)
  List.iter (fun f -> ignore (Driver.Diagnostic_render.load_source_file sources f)) input_files;

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
    let terminal = Driver.Diagnostic_render.detect_terminal (to_color_choice color) in
    let format = to_render_format error_fmt in
    let diag = Common.Compiler_error.(
      error ~code:Common.Error_code.e_type_mismatch msg
      |> with_primary_label ~span:Common.Location.none
    ) in
    let rendered = Driver.Diagnostic_render.render_diagnostic
      ~format ~terminal ~sources diag
    in
    Printf.eprintf "%s\n" rendered;
    `Error (false, "Compilation failed")

let compile input_file output_file multi_files output_dir error_fmt color
    warning_specs warn_error_spec relaxed dump_ast dump_typed dump_lambda =
  (* Parse warning configuration *)
  match parse_warning_config ~relaxed warning_specs warn_error_spec with
  | Error msg ->
    Printf.eprintf "Error in warning configuration: %s\n" msg;
    `Error (false, "Invalid warning configuration")
  | Ok warning_config ->
    let options = Driver.Pipeline.{
      dump_ast;
      dump_typed;
      dump_lambda;
      warning_config;
    } in
    match multi_files with
    | [] ->
      (* Single file mode *)
      compile_single options error_fmt color input_file output_file
    | files ->
      (* Multi-file mode *)
      compile_multi options error_fmt color (input_file :: files) output_dir

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
    `P "JSON error output (for tooling):";
    `Pre "  linac compile file.lina --error-format=json";
    `P "Warning configuration:";
    `Pre "  linac compile file.lina -W +all -W -unused";
    `Pre "  linac compile file.lina --warn-error +all";
    `P "Note: Strict mode (unused code = error) is the default.";
    `P "Configure [preset = \"relaxed\"] in lina.toml to treat unused code as warnings.";
  ] in
  let info = Cmd.info "compile" ~doc ~man in
  Cmd.v info Term.(ret (const compile $ compile_input_file $ compile_output_file $
                        compile_multi_files $ compile_output_dir $
                        error_format_arg $ color_arg $
                        warning_spec_arg $ warn_error_arg $ relaxed_arg $
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

(** {1 Explain Command} *)

let explain_code color code_str =
  match Common.Error_code.of_string code_str with
  | None ->
    Printf.eprintf "Unknown error code: %s\n" code_str;
    Printf.eprintf "Error codes have the format E0001 (errors) or W0001 (warnings).\n";
    `Error (false, "Unknown error code")
  | Some code ->
    match Driver.Explain.get_explanation code with
    | Some explanation ->
      let use_color = match color with
        | Always -> true
        | Never -> false
        | Auto ->
          let term = Driver.Diagnostic_render.detect_terminal Driver.Diagnostic_render.Auto in
          term.use_color
      in
      print_endline (Driver.Explain.format_explanation ~color:use_color code explanation);
      `Ok ()
    | None ->
      (* Fallback for codes without detailed explanations *)
      let description = Common.Error_code.description code in
      let code_name = Common.Error_code.to_string code in
      Printf.printf "%s: %s\n\n" code_name description;
      if Common.Error_code.is_error code then
        Printf.printf "This is a compiler error that prevents compilation.\n"
      else
        Printf.printf "This is a warning that may indicate a potential issue.\n";
      `Ok ()

let explain_code_arg =
  let doc = "The error or warning code to explain (e.g., E0001, W0002)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"CODE" ~doc)

let explain_cmd =
  let doc = "Explain an error or warning code" in
  let man = [
    `S Manpage.s_description;
    `P "Shows detailed documentation for a specific error or warning code. \
        Error codes start with 'E' (e.g., E0001) and warning codes start with \
        'W' (e.g., W0001).";
    `S Manpage.s_examples;
    `Pre "  linac explain E0001";
    `Pre "  linac explain W0002";
  ] in
  let info = Cmd.info "explain" ~doc ~man in
  Cmd.v info Term.(ret (const explain_code $ color_arg $ explain_code_arg))

(** {1 Main Command Group} *)

let default_cmd =
  let doc = "Lina compiler and tools" in
  let man = [
    `S Manpage.s_description;
    `P "Lina is an ML-family language that compiles to Lua.";
    `S Manpage.s_commands;
    `P "Use $(b,linac compile) to compile Lina source to Lua.";
    `P "Use $(b,linac format) to format Lina source code.";
    `P "Use $(b,linac explain) to get help with error codes.";
    `S "ERROR FORMATS";
    `P "The --error-format flag controls how errors are displayed:";
    `I ("$(b,human)", "Colored, formatted output for terminals (default)");
    `I ("$(b,short)", "One line per diagnostic, suitable for grep");
    `I ("$(b,json)", "Machine-readable JSON for IDE integration");
  ] in
  let info = Cmd.info "linac" ~version:"0.1.0" ~doc ~man in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [compile_cmd; format_cmd; explain_cmd]

let () = exit (Cmd.eval default_cmd)
