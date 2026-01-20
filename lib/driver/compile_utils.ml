(** Compilation utilities shared across stdlib, project, and package loaders. *)

type compile_result = {
  module_name : string;
  binding : Typing.Module_types.module_binding;
  signature : Typing.Module_types.signature_item list;
  lua_code : string option;
}

let compile_module ~env ~module_name ~filename ~source ?(generate_lua = true) () =
  let result = Typing.Structure_infer.compile_to_module_binding
    ~env ~module_name ~filename ~source ()
  in

  let lua_code =
    if generate_lua then
      let lambda = Lambda.translate_structure result.typed_ast in
      let lua_ast = Lua.Codegen.generate lambda in
      Some (Lua.Printer.print_chunk lua_ast)
    else
      None
  in

  { module_name; binding = result.binding; signature = result.signature; lua_code }

let exported_names_of_signature signature =
  List.filter_map (function
    | Typing.Module_types.SigValue (name, _) -> Some name
    | Typing.Module_types.SigType _ -> None
    | Typing.Module_types.SigModule _ -> None
    | Typing.Module_types.SigModuleType _ -> None
    | Typing.Module_types.SigExtensionConstructor _ -> None
  ) signature

let wrap_module_lua module_name lua_code exports =
  let return_fields = List.map (fun export_name ->
    let lua_name = Lua.Identifier_mangle.sanitize_name export_name in
    Printf.sprintf "[\"%s\"] = %s" export_name lua_name
  ) exports in

  let return_table = "return {" ^ String.concat ", " return_fields ^ "}" in

  Printf.sprintf "local %s = (function()\n%s\n%s\nend)();\n"
    module_name lua_code return_table

let find_lina_files dir =
  let rec walk acc path =
    if Sys.is_directory path then
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        walk acc (Filename.concat path entry)
      ) acc entries
    else if Package.Types.is_lina_file path then
      path :: acc
    else
      acc
  in

  if Sys.file_exists dir && Sys.is_directory dir then
    List.sort String.compare (walk [] dir)
  else
    []

let topological_sort items get_name get_deps =
  let item_map = List.fold_left (fun acc item ->
    let name = get_name item in
    (name, item) :: acc
  ) [] items in

  let find_item name = List.assoc_opt name item_map in

  let visited = Hashtbl.create (List.length items) in
  let in_progress = Hashtbl.create (List.length items) in
  let result = ref [] in

  let rec visit name =
    if Hashtbl.mem visited name then
      Ok ()
    else if Hashtbl.mem in_progress name then
      Error (Printf.sprintf "Cyclic dependency involving %s" name)
    else begin
      match find_item name with
      | None -> Ok ()
      | Some item ->
        Hashtbl.add in_progress name true;
        let deps = get_deps item in

        let deps_result = List.fold_left (fun acc dep_name ->
          match acc with
          | Error _ -> acc
          | Ok () -> visit dep_name
        ) (Ok ()) deps in

        match deps_result with
        | Error _ as err -> err
        | Ok () ->
          Hashtbl.remove in_progress name;
          Hashtbl.add visited name true;
          result := item :: !result;
          Ok ()
    end
  in

  let sort_result = List.fold_left (fun acc item ->
    match acc with
    | Error _ -> acc
    | Ok () -> visit (get_name item)
  ) (Ok ()) items in

  match sort_result with
  | Error _ as err -> err
  | Ok () -> Ok (List.rev !result)
