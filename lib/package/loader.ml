(** Package loader implementation.

    Follows the same pattern as stdlib_loader.ml for loading modules
    into the typing environment. *)

type load_result = {
  env : Typing.Environment.t;
  lua_prelude : string;
}

let bindings_dir = Types.bindings_dir

let binding_files root =
  let dir = bindings_dir root in
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter Types.is_lina_file
    |> List.sort String.compare
  else
    []

let compile_binding_file env path =
  let module_name = Types.module_name_of_file path in

  try
    let source = In_channel.with_open_text path In_channel.input_all in
    let filename = Printf.sprintf "<binding/%s>" module_name in

    let result = Typing.Structure_infer.compile_to_module_binding
      ~env ~module_name ~filename ~source ()
    in

    Ok (module_name, result.binding)
  with exn ->
    Error (Printf.sprintf "Error loading %s: %s" path (Printexc.to_string exn))

let load_single_binding ~root env filename =
  let path = Filename.concat (bindings_dir root) filename in
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "Binding file not found: %s" path)
  else
    match compile_binding_file env path with
    | Ok (name, binding) ->
      Ok (Typing.Environment.add_module name binding env)
    | Error err -> Error err

let generate_lua_prelude root =
  let lina_dir = Filename.concat root ".lina" in
  let lua_dir = Filename.concat lina_dir "share/lua/5.1" in
  let lib_dir = Filename.concat lina_dir "lib/lua/5.1" in

  let lines = [
    "-- Package path setup";
    Printf.sprintf {|package.path = package.path .. ";%s/?.lua;%s/?/init.lua"|} lua_dir lua_dir;
    Printf.sprintf {|package.cpath = package.cpath .. ";%s/?.so"|} lib_dir;
    "";
  ] in

  String.concat "\n" lines

let load_packages ~root env =
  let files = binding_files root in

  if files = [] then
    Ok { env; lua_prelude = "" }
  else
    let rec load_all env = function
      | [] -> Ok env

      | file :: rest ->
        match load_single_binding ~root env file with
        | Ok new_env -> load_all new_env rest
        | Error err -> Error err
    in

    match load_all env files with
    | Ok final_env ->
      let lua_prelude = generate_lua_prelude root in
      Ok { env = final_env; lua_prelude }
    | Error err -> Error err
