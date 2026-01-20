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
  let doc = "Output format for errors and warnings." in
  Arg.(value & opt error_format_conv Human & info ["error-format"] ~docv:"FORMAT" ~doc)

let color_arg =
  let doc = "When to use colors (auto, always, never)." in
  Arg.(value & opt color_conv Auto & info ["color"] ~docv:"WHEN" ~doc)

let verbose_arg =
  let doc = "Verbose output." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let warning_spec_arg =
  let doc = "Warning configuration. Use $(b,+name) to enable, $(b,-name) to disable, \
             $(b,name=level) to set level (allow/warn/deny/forbid). \
             Examples: $(b,-W +all), $(b,-W -unused), $(b,-W shadowing=deny)." in
  Arg.(value & opt_all string [] & info ["W"] ~docv:"SPEC" ~doc)

let warn_error_arg =
  let doc = "Treat warnings as errors. Use $(b,+all) to treat all warnings as errors." in
  Arg.(value & opt (some string) None & info ["warn-error"] ~docv:"SPEC" ~doc)

(** Parse warning specs and build configuration.
    Takes a base config (from file) and applies CLI overrides on top. *)
let parse_warning_config ?(base = Common.Warning_config.default) warning_specs warn_error_spec =
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

(** {1 Project Discovery} *)

let find_project_root () =
  let rec search dir =
    let toml_path = Filename.concat dir "lina.toml" in
    if Sys.file_exists toml_path then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None (* reached root *)
      else search parent
  in
  search (Sys.getcwd ())

(** {1 Multi-file Project Support} *)

(** Parse a file and extract module name and imports. *)
let parse_file_info filename =
  try
    let source = In_channel.with_open_text filename In_channel.input_all in
    let ast = Parsing.Parse.structure_from_string ~filename source in
    let module_name = Driver.Multifile.module_name_of_filename filename in
    let imports = Driver.Multifile.extract_imports ast in
    Ok (module_name, imports, source, filename)
  with
  | Common.Compiler_error.Error err ->
    Error (Common.Compiler_error.report_to_string err)
  | exn ->
    Error (Printf.sprintf "Error parsing %s: %s" filename (Printexc.to_string exn))

(** Compile a single source file and return module binding. *)
let compile_single_file env module_name source filename =
  Typing.Types.reset_type_variable_id ();

  let ast = Parsing.Parse.structure_from_string ~filename source in
  let ctx = Typing.Typing_context.create env in
  let typed_ast, _ctx = Typing.Inference.infer_structure ctx ast in

  let signature = Typing.Structure_infer.signature_of_typed_structure typed_ast in
  let module_type = Typing.Module_types.ModTypeSig signature in
  let module_id = Common.Identifier.create module_name in
  let binding = Typing.Module_types.{
    binding_name = module_name;
    binding_id = module_id;
    binding_type = module_type;
    binding_alias = None;
  } in

  let lambda = Lambda.translate_structure typed_ast in
  let lua_ast = Lua.Codegen.generate lambda in
  let lua_code = Lua.Printer.print_chunk lua_ast in

  let exports = Driver.Compile_utils.exported_names_of_signature signature in
  (binding, lua_code, exports)

(** {1 Build Command} *)

(** [relative_path ~base path] computes the relative path from [base] to [path].
    Assumes [path] starts with [base]. *)
let relative_path ~base path =
  let base_len = String.length base in
  let path_len = String.length path in
  if path_len > base_len && String.sub path 0 base_len = base then
    (* Skip base and the following separator *)
    let start = if path.[base_len] = '/' then base_len + 1 else base_len in
    String.sub path start (path_len - start)
  else
    Filename.basename path

(** [ensure_directory_exists path] creates the directory for [path] if needed. *)
let rec ensure_directory_exists path =
  let dir = Filename.dirname path in
  if dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    ensure_directory_exists dir;
    Sys.mkdir dir 0o755
  end

(** [mkdir_p dir] creates a directory and all parent directories if needed. *)
let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    let parent = Filename.dirname dir in
    if parent <> dir && parent <> "." && parent <> "/" then
      mkdir_p parent;
    Sys.mkdir dir 0o755
  end

let build_cmd_impl verbose error_fmt _color warning_specs warn_error_spec =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found. Run 'lina init' to create a project.\n";
    `Error (false, "No project found")
  | Some root ->
    (* Load project configuration *)
    let config = match Driver.Config.load_project_config root with
      | Ok cfg -> cfg
      | Error msg ->
        if verbose then Printf.eprintf "Warning: %s, using defaults\n" msg;
        Driver.Config.default_config (Filename.basename root)
    in
    (* Get base warning config from file, then apply CLI overrides *)
    let file_warning_config = Driver.Config.to_warning_config config.warnings in
    match parse_warning_config ~base:file_warning_config warning_specs warn_error_spec with
    | Error msg ->
      Printf.eprintf "Error in warning configuration: %s\n" msg;
      `Error (false, "Invalid warning configuration")
    | Ok warning_config ->
    let src_dir = Filename.concat root config.build.source_dir in
    if not (Sys.file_exists src_dir) then begin
      Printf.eprintf "Error: No %s/ directory found in project.\n" config.build.source_dir;
      `Error (false, "No src directory")
    end else begin
      let files = Driver.Compile_utils.find_lina_files src_dir in
      if files = [] then begin
        Printf.eprintf "Error: No .lina files found in %s/.\n" config.build.source_dir;
        `Error (false, "No source files")
      end else begin
        if verbose then
          Printf.printf "Building %s v%s (%d file(s))...\n"
            config.package.name config.package.version (List.length files);

        let build_dir = Filename.concat root config.build.output_dir in
        if not (Sys.file_exists build_dir) then
          Sys.mkdir build_dir 0o755;

        (* Load stdlib and project dependencies *)
        let base_env = Driver.Stdlib_loader.initial_with_stdlib () in
        let env, dep_prelude =
          if config.dependencies.lina = [] then
            (base_env, "")
          else begin
            if verbose then
              Printf.printf "Loading %d project dependenc%s...\n"
                (List.length config.dependencies.lina)
                (if List.length config.dependencies.lina = 1 then "y" else "ies");
            match Driver.Project_loader.load_all_dependencies
                    ~root ~deps:config.dependencies.lina ~env:base_env with
            | Ok result -> (result.env, result.lua_prelude)
            | Error msg ->
              Printf.eprintf "Error loading project dependencies: %s\n" msg;
              (base_env, "")
          end
        in

        let _options = Driver.Pipeline.{
          dump_ast = false;
          dump_typed = false;
          dump_lambda = false;
          warning_config;
        } in

        let sources = Driver.Diagnostic_render.create_source_cache () in

        (* Parse all files and extract imports for multi-file support *)
        let parse_results = List.map (fun file ->
          ignore (Driver.Diagnostic_render.load_source_file sources file);
          parse_file_info file
        ) files in

        let errors, parsed_files = List.partition_map (function
          | Error err -> Left err
          | Ok info -> Right info
        ) parse_results in

        if errors <> [] then begin
          List.iter (fun err -> Printf.eprintf "%s\n" err) errors;
          `Error (false, "Parse failed")
        end else begin
          (* Topologically sort files by imports *)
          let get_name (name, _, _, _) = name in
          let get_deps (_, imports, _, _) = imports in

          match Driver.Compile_utils.topological_sort parsed_files get_name get_deps with
          | Error cycle_msg ->
            Printf.eprintf "Error: %s\n" cycle_msg;
            `Error (false, "Build failed due to cyclic dependency")
          | Ok sorted_files ->
            if verbose then
              Printf.printf "Compilation order: %s\n"
                (String.concat " -> " (List.map (fun (name, _, _, _) -> name) sorted_files));

            (* Compile files in order, accumulating modules in environment *)
            let rec compile_all current_env compiled_count = function
              | [] ->
                Printf.printf "Build complete: %d file(s) compiled to %s/\n"
                  compiled_count build_dir;
                `Ok ()
              | (module_name, _imports, source, filename) :: rest ->
                try
                  let binding, lua_code, exports = compile_single_file current_env module_name source filename in

                  (* Handle warnings *)
                  let warnings = Common.Compiler_error.get_warnings () in
                  if warnings <> [] then begin
                    let terminal = Driver.Diagnostic_render.detect_terminal
                      (match _color with Auto -> Driver.Diagnostic_render.Auto
                                      | Always -> Driver.Diagnostic_render.Always
                                      | Never -> Driver.Diagnostic_render.Never) in
                    let format = match error_fmt with
                      | Human -> Driver.Diagnostic_render.Human
                      | Short -> Driver.Diagnostic_render.Short
                      | Json -> Driver.Diagnostic_render.Json
                    in

                    let has_fatal_warning = ref false in
                    List.iter (fun warning ->
                      let code = Common.Compiler_error.warning_code warning in
                      if Common.Warning_config.is_error warning_config code then
                        has_fatal_warning := true;
                      let diag = Common.Compiler_error.diagnostic_of_warning warning in
                      let rendered = Driver.Diagnostic_render.render_diagnostic
                        ~format ~terminal ~sources diag
                      in
                      Printf.eprintf "%s\n" rendered
                    ) warnings;

                    if !has_fatal_warning then begin
                      Printf.eprintf "\nCompilation failed due to warnings treated as errors\n";
                      `Error (false, "Build failed")
                    end else begin
                      (* Add module to environment for subsequent files *)
                      let new_env = Typing.Environment.add_module module_name binding current_env in

                      (* Write output file *)
                      let rel_path = relative_path ~base:src_dir filename in
                      let lua_rel_path = Filename.chop_suffix rel_path ".lina" ^ ".lua" in
                      let output_path = Filename.concat build_dir lua_rel_path in
                      ensure_directory_exists output_path;

                      let full_lua = dep_prelude ^ Driver.Compile_utils.wrap_module_lua module_name lua_code exports in
                      let oc = open_out output_path in
                      output_string oc full_lua;
                      close_out oc;
                      if verbose then Printf.printf "  %s -> %s\n" filename output_path;

                      compile_all new_env (compiled_count + 1) rest
                    end
                  end else begin
                    (* No warnings - add module to environment and continue *)
                    let new_env = Typing.Environment.add_module module_name binding current_env in

                    (* Write output file *)
                    let rel_path = relative_path ~base:src_dir filename in
                    let lua_rel_path = Filename.chop_suffix rel_path ".lina" ^ ".lua" in
                    let output_path = Filename.concat build_dir lua_rel_path in
                    ensure_directory_exists output_path;

                    let full_lua = dep_prelude ^ Driver.Compile_utils.wrap_module_lua module_name lua_code exports in
                    let oc = open_out output_path in
                    output_string oc full_lua;
                    close_out oc;
                    if verbose then Printf.printf "  %s -> %s\n" filename output_path;

                    compile_all new_env (compiled_count + 1) rest
                  end
                with
                | Common.Compiler_error.Error err ->
                  let terminal = Driver.Diagnostic_render.detect_terminal
                    (match _color with Auto -> Driver.Diagnostic_render.Auto
                                    | Always -> Driver.Diagnostic_render.Always
                                    | Never -> Driver.Diagnostic_render.Never) in
                  let format = match error_fmt with
                    | Human -> Driver.Diagnostic_render.Human
                    | Short -> Driver.Diagnostic_render.Short
                    | Json -> Driver.Diagnostic_render.Json
                  in
                  let diag = Common.Compiler_error.diagnostic_of_error err in
                  let rendered = Driver.Diagnostic_render.render_diagnostic
                    ~format ~terminal ~sources diag
                  in
                  Printf.eprintf "%s\n" rendered;
                  `Error (false, "Build failed")
                | exn ->
                  Printf.eprintf "Error compiling %s: %s\n" filename (Printexc.to_string exn);
                  `Error (false, "Build failed")
            in

            compile_all env 0 sorted_files
        end
      end
    end

let build_cmd =
  let doc = "Build the project" in
  let man = [
    `S Manpage.s_description;
    `P "Compiles all Lina source files in the src/ directory to Lua. \
        Output files are written to _build/.";
    `S Manpage.s_examples;
    `Pre "  lina build";
    `Pre "  lina build -W +all -W -unused";
    `Pre "  lina build --warn-error +all";
  ] in
  let info = Cmd.info "build" ~doc ~man in
  Cmd.v info Term.(ret (const build_cmd_impl $ verbose_arg $ error_format_arg $ color_arg $
                        warning_spec_arg $ warn_error_arg))

(** {1 Check Command} *)

let check_cmd_impl verbose error_fmt _color warning_specs warn_error_spec =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    (* Load project configuration for warning settings *)
    let config = match Driver.Config.load_project_config root with
      | Ok cfg -> cfg
      | Error _ -> Driver.Config.default_config (Filename.basename root)
    in
    let file_warning_config = Driver.Config.to_warning_config config.warnings in
    match parse_warning_config ~base:file_warning_config warning_specs warn_error_spec with
    | Error msg ->
      Printf.eprintf "Error in warning configuration: %s\n" msg;
      `Error (false, "Invalid warning configuration")
    | Ok warning_config ->
    let src_dir = Filename.concat root "src" in
    if not (Sys.file_exists src_dir) then begin
      Printf.eprintf "Error: No src/ directory found.\n";
      `Error (false, "No src directory")
    end else begin
      let files = Driver.Compile_utils.find_lina_files src_dir in
      if verbose then
        Printf.printf "Type-checking %d file(s)...\n" (List.length files);

      let has_error = ref false in
      let sources = Driver.Diagnostic_render.create_source_cache () in

      List.iter (fun file ->
        ignore (Driver.Diagnostic_render.load_source_file sources file);
        (* For check, we still run compile but don't write output *)
        let options = Driver.Pipeline.{
          dump_ast = false;
          dump_typed = false;
          dump_lambda = false;
          warning_config;
        } in
        match Driver.Pipeline.compile_file options file with
        | Ok _ ->
          if verbose then Printf.printf "  %s: ok\n" file
        | Error msg ->
          has_error := true;
          let terminal = Driver.Diagnostic_render.detect_terminal
            (match _color with Auto -> Driver.Diagnostic_render.Auto
                            | Always -> Driver.Diagnostic_render.Always
                            | Never -> Driver.Diagnostic_render.Never) in
          let format = match error_fmt with
            | Human -> Driver.Diagnostic_render.Human
            | Short -> Driver.Diagnostic_render.Short
            | Json -> Driver.Diagnostic_render.Json
          in
          let diag = Common.Compiler_error.(
            error ~code:Common.Error_code.e_type_mismatch msg
            |> with_primary_label ~span:Common.Location.none
          ) in
          let rendered = Driver.Diagnostic_render.render_diagnostic
            ~format ~terminal ~sources diag
          in
          Printf.eprintf "%s\n" rendered
      ) files;

      if !has_error then
        `Error (false, "Check failed")
      else begin
        Printf.printf "All %d file(s) type-check successfully.\n" (List.length files);
        `Ok ()
      end
    end

let check_cmd =
  let doc = "Type-check the project without generating code" in
  let info = Cmd.info "check" ~doc in
  Cmd.v info Term.(ret (const check_cmd_impl $ verbose_arg $ error_format_arg $ color_arg $
                        warning_spec_arg $ warn_error_arg))

(** {1 Init Command} *)

let init_cmd_impl name =
  let dir = match name with Some n -> n | None -> Sys.getcwd () in
  let project_name = Filename.basename dir in

  (* Create directory if name was provided (including parent directories) *)
  (match name with
  | Some n when not (Sys.file_exists n) -> mkdir_p n
  | _ -> ());

  (* Create src directory *)
  let src_dir = Filename.concat dir "src" in
  if not (Sys.file_exists src_dir) then
    Sys.mkdir src_dir 0o755;

  (* Create lina.toml *)
  let toml_path = Filename.concat dir "lina.toml" in
  if Sys.file_exists toml_path then begin
    Printf.eprintf "Error: lina.toml already exists.\n";
    `Error (false, "Project already initialized")
  end else begin
    let toml_content = Printf.sprintf {|[package]
name = "%s"
version = "0.1.0"
edition = "2025"

[build]
target = "lua53"
source_dir = "src"
output_dir = "_build"

[dependencies.luarocks]
# Add LuaRocks dependencies here
# Example: luasocket = "^3.1"

[warnings]
default = "warn"
|} project_name in
    let oc = open_out toml_path in
    output_string oc toml_content;
    close_out oc;

    (* Create main.lina *)
    let main_path = Filename.concat src_dir "main.lina" in
    if not (Sys.file_exists main_path) then begin
      let main_content = {|-- Main entry point

let main () =
  print "Hello, Lina!"

let _ = main ()
|} in
      let oc = open_out main_path in
      output_string oc main_content;
      close_out oc
    end;

    (* Create .gitignore *)
    let gitignore_path = Filename.concat dir ".gitignore" in
    if not (Sys.file_exists gitignore_path) then begin
      let gitignore_content = {|_build/
*.lua
.lina/
|} in
      let oc = open_out gitignore_path in
      output_string oc gitignore_content;
      close_out oc
    end;

    Printf.printf "Created new Lina project in %s/\n" dir;
    Printf.printf "\n";
    Printf.printf "  cd %s\n" (if name = None then "." else project_name);
    Printf.printf "  lina build\n";
    Printf.printf "\n";
    `Ok ()
  end

let init_name_arg =
  let doc = "Project name (creates a new directory). If not specified, \
             initializes in the current directory." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let init_cmd =
  let doc = "Initialize a new Lina project" in
  let man = [
    `S Manpage.s_description;
    `P "Creates a new Lina project with a lina.toml configuration file, \
        src/ directory, and a sample main.lina file.";
    `S Manpage.s_examples;
    `Pre "  lina init myproject    # Create new project directory";
    `Pre "  lina init              # Initialize in current directory";
  ] in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info Term.(ret (const init_cmd_impl $ init_name_arg))

(** {1 Clean Command} *)

(** [remove_directory_recursive dir] removes a directory and all its contents. *)
let rec remove_directory_recursive dir =
  let entries = Sys.readdir dir |> Array.to_list in
  List.iter (fun entry ->
    let path = Filename.concat dir entry in
    if Sys.is_directory path then
      remove_directory_recursive path
    else
      Sys.remove path
  ) entries;
  Sys.rmdir dir

let clean_cmd_impl verbose =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    let build_dir = Filename.concat root "_build" in
    if Sys.file_exists build_dir then begin
      if verbose then Printf.printf "Removing %s/\n" build_dir;
      remove_directory_recursive build_dir;
      Printf.printf "Cleaned build artifacts.\n";
      `Ok ()
    end else begin
      Printf.printf "Nothing to clean.\n";
      `Ok ()
    end

let clean_cmd =
  let doc = "Remove build artifacts" in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(ret (const clean_cmd_impl $ verbose_arg))

(** {1 Format Command} *)

let format_cmd_impl in_place check width =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    let src_dir = Filename.concat root "src" in
    let files = Driver.Compile_utils.find_lina_files src_dir in
    let config = Lina_format.Formatter.{
      line_width = width;
      indent_size = 2;
    } in

    let needs_formatting = ref false in

    List.iter (fun file ->
      if check then begin
        match Lina_format.Formatter.check_file_cst ~config file with
        | Ok true -> ()
        | Ok false ->
          Printf.eprintf "%s: needs formatting\n" file;
          needs_formatting := true
        | Error msg ->
          Printf.eprintf "Error formatting %s: %s\n" file msg;
          needs_formatting := true
      end else if in_place then begin
        match Lina_format.Formatter.format_file_cst ~config ~in_place:true file with
        | Ok _ -> Printf.printf "Formatted %s\n" file
        | Error msg -> Printf.eprintf "Error formatting %s: %s\n" file msg
      end else begin
        match Lina_format.Formatter.format_file_cst ~config file with
        | Ok formatted -> print_string formatted
        | Error msg -> Printf.eprintf "Error: %s\n" msg
      end
    ) files;

    if check && !needs_formatting then
      `Error (false, "Some files need formatting")
    else
      `Ok ()

let format_in_place =
  let doc = "Modify files in place." in
  Arg.(value & flag & info ["i"; "in-place"] ~doc)

let format_check =
  let doc = "Check formatting without modifying files." in
  Arg.(value & flag & info ["check"] ~doc)

let format_width =
  let doc = "Target line width." in
  Arg.(value & opt int 80 & info ["w"; "width"] ~docv:"WIDTH" ~doc)

let format_cmd =
  let doc = "Format all source files in the project" in
  let info = Cmd.info "format" ~doc in
  Cmd.v info Term.(ret (const format_cmd_impl $ format_in_place $ format_check $ format_width))

(** {1 Watch Command} *)

let watch_cmd_impl verbose error_fmt _color warning_specs warn_error_spec =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    let config = match Driver.Config.load_project_config root with
      | Ok cfg -> cfg
      | Error _ -> Driver.Config.default_config (Filename.basename root)
    in
    (* Get warning config from file, then apply CLI overrides *)
    let file_warning_config = Driver.Config.to_warning_config config.warnings in
    match parse_warning_config ~base:file_warning_config warning_specs warn_error_spec with
    | Error msg ->
      Printf.eprintf "Error in warning configuration: %s\n" msg;
      `Error (false, "Invalid warning configuration")
    | Ok _warning_config ->
    let src_dir = Filename.concat root config.build.source_dir in
    if not (Sys.file_exists src_dir) then begin
      Printf.eprintf "Error: No %s/ directory found.\n" config.build.source_dir;
      `Error (false, "No src directory")
    end else begin
      Printf.printf "Watching %s for changes (Ctrl+C to stop)...\n\n" src_dir;
      let watcher = Driver.File_watcher.create ~dir:src_dir ~extensions:[".lina"] in
      let previous = ref (Driver.File_watcher.get_current_state watcher) in

      (* Initial build *)
      let _ = build_cmd_impl verbose error_fmt _color warning_specs warn_error_spec in

      (* Watch loop *)
      let rec loop () =
        let _changes = Driver.File_watcher.wait_for_changes watcher
          ~previous:!previous ~poll_interval_ms:500 in
        previous := Driver.File_watcher.get_current_state watcher;
        Driver.File_watcher.clear_screen ();
        Printf.printf "[%s] Changes detected, rebuilding...\n\n"
          (let t = Unix.localtime (Unix.time ()) in
           Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec);
        let _ = build_cmd_impl verbose error_fmt _color warning_specs warn_error_spec in
        Printf.printf "\nWatching for changes...\n";
        loop ()
      in
      loop ()
    end

let watch_cmd =
  let doc = "Watch for changes and rebuild automatically" in
  let man = [
    `S Manpage.s_description;
    `P "Watches the source directory for changes and automatically rebuilds \
        when files are modified. Press Ctrl+C to stop.";
  ] in
  let info = Cmd.info "watch" ~doc ~man in
  Cmd.v info Term.(ret (const watch_cmd_impl $ verbose_arg $ error_format_arg $ color_arg $
                        warning_spec_arg $ warn_error_arg))

(** {1 Run Command} *)

let run_cmd_impl verbose warning_specs warn_error_spec =
  (* First build, then run with luajit/lua *)
  match build_cmd_impl verbose Human Auto warning_specs warn_error_spec with
  | `Ok () ->
    (match find_project_root () with
    | None -> `Error (false, "No project found")
    | Some root ->
      let main_lua = Filename.concat root "_build/main.lua" in
      if Sys.file_exists main_lua then begin
        Printf.printf "Running %s...\n\n" main_lua;
        let code = Sys.command (Printf.sprintf "luajit %s || lua %s" main_lua main_lua) in
        if code = 0 then `Ok () else `Error (false, "Execution failed")
      end else begin
        Printf.eprintf "Error: No main.lua found. Create src/main.lina.\n";
        `Error (false, "No main.lua")
      end)
  | err -> err

let run_cmd =
  let doc = "Build and run the project" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(ret (const run_cmd_impl $ verbose_arg $ warning_spec_arg $ warn_error_arg))

(** {1 REPL Command} *)

let repl_cmd_impl () =
  let state = Driver.Repl.create () in
  Driver.Repl.run state;
  `Ok ()

let repl_cmd =
  let doc = "Start an interactive REPL session" in
  let man = [
    `S Manpage.s_description;
    `P "Starts an interactive Read-Eval-Print-Loop for exploring Lina. \
        Type expressions to evaluate them, or use commands:";
    `P ":type <expr>  - Show the type of an expression";
    `P ":load <file>  - Load a Lina source file";
    `P ":env          - Show all bindings";
    `P ":clear        - Reset the environment";
    `P ":help         - Show help";
    `P ":quit         - Exit the REPL";
    `S "LINE EDITING";
    `P "For line editing and history support, use rlwrap:";
    `Pre "  rlwrap lina repl";
    `S Manpage.s_examples;
    `Pre "  lina repl";
    `Pre "  let x = 42";
    `Pre "  x + 1";
    `Pre "  :type x";
  ] in
  let info = Cmd.info "repl" ~doc ~man in
  Cmd.v info Term.(ret (const repl_cmd_impl $ const ()))

(** {1 Deps Commands} *)

let deps_install_impl verbose =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    let config = match Driver.Config.load_project_config root with
      | Ok cfg -> cfg
      | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1
    in

    if not (Package.Luarocks.is_available ()) then begin
      Printf.eprintf "Error: luarocks is not installed or not in PATH.\n";
      `Error (false, "luarocks not available")
    end else begin
      if verbose then
        Printf.printf "Installing dependencies for %s...\n" config.package.name;

      match Package.Installer.install_all ~root ~dependencies:config.dependencies () with
      | Ok result ->
        let bindings_dir = Package.Installer.bindings_dir root in
        if not (Sys.file_exists bindings_dir) then
          Unix.mkdir bindings_dir 0o755;

        let _ = Package.Binding_generator.generate_all
          ~lockfile:result.lockfile ~output_dir:bindings_dir () in

        Printf.printf "Installed %d package(s), %d skipped.\n"
          result.installed_count result.skipped_count;
        `Ok ()
      | Error (Package.Installer.LuarocksNotAvailable) ->
        Printf.eprintf "Error: luarocks is not available.\n";
        `Error (false, "luarocks not available")
      | Error (Package.Installer.InstallFailed (name, msg)) ->
        Printf.eprintf "Error installing %s: %s\n" name msg;
        `Error (false, "Installation failed")
      | Error (Package.Installer.DirectoryError msg) ->
        Printf.eprintf "Error: %s\n" msg;
        `Error (false, "Directory error")
    end

let deps_install_cmd =
  let doc = "Install dependencies from lina.toml" in
  let man = [
    `S Manpage.s_description;
    `P "Installs all LuaRocks dependencies specified in lina.toml. \
        Creates a .lina directory with installed packages and generates \
        type bindings for use in Lina code.";
    `S Manpage.s_examples;
    `Pre "  lina deps install";
  ] in
  let info = Cmd.info "install" ~doc ~man in
  Cmd.v info Term.(ret (const deps_install_impl $ verbose_arg))

let deps_list_impl () =
  match find_project_root () with
  | None ->
    Printf.eprintf "Error: No lina.toml found.\n";
    `Error (false, "No project found")
  | Some root ->
    let lock_path = Package.Installer.lockfile_path root in
    if not (Sys.file_exists lock_path) then begin
      Printf.printf "No dependencies installed yet. Run 'lina deps install' first.\n";
      `Ok ()
    end else
      match Package.Lockfile.load lock_path with
      | Error msg ->
        Printf.eprintf "Error reading lockfile: %s\n" msg;
        `Error (false, "Lockfile error")
      | Ok lockfile ->
        if lockfile.packages = [] then
          Printf.printf "No packages installed.\n"
        else begin
          Printf.printf "Installed packages:\n";
          List.iter (fun (pkg : Package.Lockfile.entry) ->
            Printf.printf "  %s@%s\n" pkg.name pkg.version
          ) lockfile.packages
        end;
        `Ok ()

let deps_list_cmd =
  let doc = "List installed dependencies" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(ret (const deps_list_impl $ const ()))

let deps_cmd =
  let doc = "Manage project dependencies" in
  let man = [
    `S Manpage.s_description;
    `P "Commands for managing LuaRocks dependencies.";
    `S Manpage.s_commands;
    `P "$(b,lina deps install) - Install all dependencies";
    `P "$(b,lina deps list) - List installed packages";
  ] in
  let info = Cmd.info "deps" ~doc ~man in
  Cmd.group info [
    deps_install_cmd;
    deps_list_cmd;
  ]

(** {1 Main Command Group} *)

let default_cmd =
  let doc = "Lina build tool" in
  let man = [
    `S Manpage.s_description;
    `P "Lina is an ML-family language that compiles to Lua. \
        This tool manages Lina projects.";
    `S Manpage.s_commands;
    `P "$(b,lina build) - Compile the project";
    `P "$(b,lina check) - Type-check without compiling";
    `P "$(b,lina run) - Build and run the project";
    `P "$(b,lina watch) - Watch for changes and rebuild";
    `P "$(b,lina repl) - Start an interactive session";
    `P "$(b,lina init) - Create a new project";
    `P "$(b,lina format) - Format source files";
    `P "$(b,lina clean) - Remove build artifacts";
    `P "$(b,lina deps) - Manage dependencies";
    `S "PROJECT STRUCTURE";
    `P "A Lina project has the following structure:";
    `Pre "  myproject/\n  ├── lina.toml      # Project configuration\n  ├── src/           # Source files\n  │   └── main.lina\n  └── _build/        # Compiled output";
  ] in
  let info = Cmd.info "lina" ~version:"0.1.0" ~doc ~man in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [
    build_cmd;
    check_cmd;
    run_cmd;
    watch_cmd;
    repl_cmd;
    init_cmd;
    format_cmd;
    clean_cmd;
    deps_cmd;
  ]

let () = exit (Cmd.eval default_cmd)
