(** Project dependency loader. *)

type load_result = {
  env : Typing.Environment.t;
  lua_prelude : string;
}

let resolve_dependency_path ~root (dep : Config.lina_dependency) =
  let path = dep.lina_path in
  let abs_path =
    if Filename.is_relative path then Filename.concat root path
    else path
  in

  if Sys.file_exists abs_path && Sys.is_directory abs_path then Ok abs_path
  else Error (Printf.sprintf "Dependency path not found: %s" abs_path)

let parse_file filename =
  try
    let source = In_channel.with_open_text filename In_channel.input_all in
    let ast = Parsing.Parse.structure_from_string ~filename source in
    let module_name = Multifile.module_name_of_filename filename in
    let imports = Multifile.extract_imports ast in
    Ok (module_name, ast, imports, source)
  with
  | Common.Compiler_error.Error err ->
      Error (Common.Compiler_error.report_to_string err)
  | exn ->
      Error (Printf.sprintf "Error parsing %s: %s" filename (Printexc.to_string exn))

let compile_single_file env module_name source filename =
  let compile_result = Compile_utils.compile_module ~env ~module_name ~filename ~source () in

  let exports = Compile_utils.exported_names_of_signature compile_result.signature in
  let lua_code = Option.get compile_result.lua_code in

  (compile_result.binding, lua_code, exports)

let compile_project_files env source_dir =
  let files = Compile_utils.find_lina_files source_dir in

  if files = [] then
    Error (Printf.sprintf "No .lina files found in %s" source_dir)
  else
    let parse_results = List.map (fun filename ->
      match parse_file filename with
      | Ok (name, ast, imports, source) -> Ok (name, ast, imports, source, filename)
      | Error err -> Error err
    ) files in

    let errors, parsed_files = List.partition_map (function
      | Error err -> Left err
      | Ok file -> Right file
    ) parse_results in

    if errors <> [] then
      Error (String.concat "\n" errors)
    else
      let get_name (name, _, _, _, _) = name in
      let get_deps (_, _, imports, _, _) = imports in

      match Compile_utils.topological_sort parsed_files get_name get_deps with
      | Error err -> Error err
      | Ok sorted_files ->
          let rec compile_all env lua_parts bindings = function
            | [] -> Ok (env, lua_parts, bindings)
            | (module_name, _ast, _imports, source, filename) :: rest ->
                try
                  let binding, lua_code, exports = compile_single_file env module_name source filename in
                  let updated_env = Typing.Environment.add_module module_name binding env in
                  let wrapped_lua = Compile_utils.wrap_module_lua module_name lua_code exports in
                  compile_all updated_env (wrapped_lua :: lua_parts) ((module_name, binding) :: bindings) rest
                with
                | Common.Compiler_error.Error err ->
                    Error (Printf.sprintf "Error compiling %s: %s" filename (Common.Compiler_error.report_to_string err))
                | exn ->
                    Error (Printf.sprintf "Error compiling %s: %s" filename (Printexc.to_string exn))
          in
          compile_all env [] [] sorted_files

let create_project_module_binding project_name sub_bindings =
  let open Typing.Module_types in
  let signature_items = List.map (fun (name, binding) ->
    SigModule (name, binding.binding_type)
  ) sub_bindings in

  let module_type = ModTypeSig signature_items in
  let module_id = Common.Identifier.create project_name in

  { binding_name = project_name;
    binding_id = module_id;
    binding_type = module_type;
    binding_alias = None }

let generate_project_lua project_name sub_modules lua_parts =
  let module_code = String.concat "\n" (List.rev lua_parts) in

  let project_fields = List.map (fun (name, _) ->
    let safe_name = Lua.Identifier_mangle.sanitize_name name in
    Printf.sprintf "[\"%s\"] = %s" name safe_name
  ) sub_modules in

  let safe_project_name = Lua.Identifier_mangle.sanitize_name project_name in
  let project_table = Printf.sprintf "local %s = {%s};\n"
    safe_project_name (String.concat ", " project_fields)
  in

  module_code ^ "\n" ^ project_table

(** Replaces dashes with underscores and capitalizes the first letter. *)
let capitalize_name name =
  if String.length name = 0 then name
  else
    let normalized = String.map (fun c -> if c = '-' then '_' else c) name in
    let first = Char.uppercase_ascii normalized.[0] in
    String.make 1 first ^ String.sub normalized 1 (String.length normalized - 1)

let load_project_dependency_from_path ~dep_path ~(dep : Config.lina_dependency) ~env =
  let config_path = Package.Types.config_path dep_path in

  if not (Sys.file_exists config_path) then
    Error (Printf.sprintf "Dependency %s has no lina.toml at %s" dep.lina_name dep_path)
  else

  match Config.load_project_config dep_path with
  | Error err -> Error (Printf.sprintf "Error loading dependency config: %s" err)
  | Ok config ->
      let source_dir = Filename.concat dep_path config.build.source_dir in

      match compile_project_files env source_dir with
      | Error err -> Error err
      | Ok (env_with_subs, lua_parts, sub_bindings) ->
          let project_name = capitalize_name dep.lina_name in

          if List.length sub_bindings = 1 then
            let _, binding = List.hd sub_bindings in
            let project_id = Common.Identifier.create project_name in
            let binding = Typing.Module_types.{
              binding with binding_name = project_name; binding_id = project_id
            } in
            let final_env = Typing.Environment.add_module project_name binding env in
            let lua_code = String.concat "\n" (List.rev lua_parts) in

            let renamed_lua =
              let original_name = fst (List.hd sub_bindings) in
              if original_name <> project_name then
                let safe_project = Lua.Identifier_mangle.sanitize_name project_name in
                let safe_original = Lua.Identifier_mangle.sanitize_name original_name in
                lua_code ^ Printf.sprintf "\nlocal %s = %s;\n" safe_project safe_original
              else
                lua_code
            in
            Ok { env = final_env; lua_prelude = renamed_lua }
          else
            let project_binding = create_project_module_binding project_name sub_bindings in
            let final_env = Typing.Environment.add_module project_name project_binding env_with_subs in
            let lua_code = generate_project_lua project_name sub_bindings lua_parts in
            Ok { env = final_env; lua_prelude = lua_code }

let load_project_dependency ~root ~(dep : Config.lina_dependency) ~env =
  match resolve_dependency_path ~root dep with
  | Error err -> Error err
  | Ok dep_path -> load_project_dependency_from_path ~dep_path ~dep ~env

let rec collect_all_deps ~root ~visited (deps : Config.lina_dependency list) =
  let open Package.Types in
  List.fold_left (fun acc dep ->
    if List.mem dep.lina_name visited then acc
    else
      match resolve_dependency_path ~root dep with
      | Error _ when dep.lina_optional -> acc
      | Error err -> (dep.lina_name, Error err) :: acc
      | Ok dep_path ->
          let cfg_path = config_path dep_path in
          if not (Sys.file_exists cfg_path) then
            (dep.lina_name, Error (Printf.sprintf "No lina.toml at %s" dep_path)) :: acc
          else
            match Config.load_project_config dep_path with
            | Error err -> (dep.lina_name, Error err) :: acc
            | Ok config ->
                let sub_deps = config.Config.dependencies.lina in
                let sub_dep_names = List.map (fun d -> d.lina_name) sub_deps in
                let new_visited = dep.lina_name :: visited in
                let transitive = collect_all_deps ~root:dep_path ~visited:new_visited sub_deps in
                (dep.lina_name, Ok (dep, dep_path, sub_dep_names)) :: transitive @ acc
  ) [] deps

let topological_sort_deps deps_with_results =
  let valid_deps = List.filter_map (function
    | (name, Ok (dep, path, sub_deps)) -> Some (name, dep, path, sub_deps)
    | _ -> None
  ) deps_with_results in

  let get_name (name, _, _, _) = name in
  let get_deps (_, _, _, sub_deps) = sub_deps in

  match Compile_utils.topological_sort valid_deps get_name get_deps with
  | Error msg -> Error msg
  | Ok sorted -> Ok (List.map (fun (name, dep, path, _) -> (name, dep, path)) sorted)

(** Resolves transitive dependencies and loads them in topologically sorted order. *)
let load_all_dependencies ~root ~(deps : Config.lina_dependency list) ~env =
  let all_deps = collect_all_deps ~root ~visited:[] deps in

  let errors = List.filter_map (function
    | (dep_name, Error err) -> Some (Printf.sprintf "%s: %s" dep_name err)
    | _ -> None
  ) all_deps in

  if errors <> [] then
    Error (String.concat "\n" errors)
  else

  match topological_sort_deps all_deps with
  | Error err -> Error err
  | Ok sorted_deps ->
      let rec load_all env preludes processed = function
        | [] -> Ok { env; lua_prelude = String.concat "\n" (List.rev preludes) }
        | (dep_name, _, _) :: rest when List.mem dep_name processed ->
            load_all env preludes processed rest
        | (dep_name, dep, dep_path) :: rest ->
            let capitalized = capitalize_name dep_name in

            if Option.is_some (Typing.Environment.find_module capitalized env) then
              Printf.eprintf "Warning: Dependency '%s' shadows stdlib module '%s'\n%!" dep_name capitalized;

            match load_project_dependency_from_path ~dep_path ~dep ~env with
            | Error _ when dep.Package.Types.lina_optional ->
                load_all env preludes (dep_name :: processed) rest
            | Error err -> Error err
            | Ok load_result ->
                load_all load_result.env (load_result.lua_prelude :: preludes) (dep_name :: processed) rest
      in
      load_all env [] [] sorted_deps
