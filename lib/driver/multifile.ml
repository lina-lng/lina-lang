open Common

(** Module name from filename: list.lina -> List *)
let module_name_of_filename filename =
  let base = Filename.basename filename in
  let name = Filename.remove_extension base in
  String.capitalize_ascii name

(** Extract external module references from a structure.
    Returns list of module names that are referenced but not defined. *)
let rec extract_imports (structure : Parsing.Syntax_tree.structure) : string list =
  let defined = ref [] in
  let referenced = ref [] in
  List.iter (extract_from_item defined referenced) structure;
  (* Return referenced modules that aren't defined locally *)
  List.filter (fun m -> not (List.mem m !defined)) (List.sort_uniq String.compare !referenced)

and extract_from_longident referenced (longident : Parsing.Syntax_tree.longident) =
  let open Parsing.Syntax_tree in
  let rec find_root (lid : longident_desc) =
    match lid with
    | Lident name ->
      (* Check if this looks like a module name (uppercase first letter) *)
      if String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0] then
        referenced := name :: !referenced
    | Ldot (parent, _) -> find_root parent.Location.value
  in
  find_root longident.Location.value

and extract_from_item defined referenced (item : Parsing.Syntax_tree.structure_item) =
  let open Parsing.Syntax_tree in
  match item.Location.value with
  | StructureValue (_, bindings) ->
    List.iter (fun b -> extract_from_expr referenced b.binding_expression) bindings
  | StructureType _ -> ()
  | StructureModule mb ->
    defined := mb.module_name.Location.value :: !defined;
    extract_from_module_expr referenced mb.module_expr
  | StructureModuleType _ -> ()
  | StructureOpen path ->
    let modules = path.Location.value in
    begin match modules with
    | name :: _ -> referenced := name :: !referenced
    | [] -> ()
    end
  | StructureInclude mexpr ->
    extract_from_module_expr referenced mexpr
  | StructureExternal _ ->
    (* External declarations don't reference other modules *)
    ()
  | StructureRecModule rec_bindings ->
    (* Each recursive module binding defines a module and may reference others *)
    List.iter (fun (binding : Parsing.Syntax_tree.rec_module_binding) ->
      defined := binding.rec_module_name.Location.value :: !defined;
      extract_from_module_expr referenced binding.rec_module_expr
    ) rec_bindings
  | StructureTypeExtension ext ->
    (* Type extensions may reference other modules via the type path *)
    extract_from_longident referenced ext.extension_type_name
  | StructureError _ ->
    (* Error nodes don't reference other modules *)
    ()

and extract_from_module_expr referenced (mexpr : Parsing.Syntax_tree.module_expression) =
  let open Parsing.Syntax_tree in
  match mexpr.Location.value with
  | ModuleStructure items ->
    List.iter (extract_from_item (ref []) referenced) items
  | ModulePath path ->
    let modules = path.Location.value in
    begin match modules with
    | name :: _ -> referenced := name :: !referenced
    | [] -> ()
    end
  | ModuleFunctor (_, body) ->
    extract_from_module_expr referenced body
  | ModuleApply (f, arg) ->
    extract_from_module_expr referenced f;
    extract_from_module_expr referenced arg
  | ModuleConstraint (inner, _) ->
    extract_from_module_expr referenced inner
  | ModuleUnpack (e, _) ->
    extract_from_expr referenced e

and extract_from_expr referenced (expr : Parsing.Syntax_tree.expression) =
  let open Parsing.Syntax_tree in
  match expr.Location.value with
  | ExpressionVariable _ -> ()
  | ExpressionConstant _ -> ()
  | ExpressionTuple es ->
    List.iter (extract_from_expr referenced) es
  | ExpressionConstructor (_, arg) ->
    Option.iter (extract_from_expr referenced) arg
  | ExpressionApply (f, labeled_args) ->
    extract_from_expr referenced f;
    List.iter (fun (_, e) -> extract_from_expr referenced e) labeled_args
  | ExpressionFunction (_, body) ->
    extract_from_expr referenced body
  | ExpressionLet (_, bindings, body) ->
    List.iter (fun b -> extract_from_expr referenced b.binding_expression) bindings;
    extract_from_expr referenced body
  | ExpressionIf (c, t, e) ->
    extract_from_expr referenced c;
    extract_from_expr referenced t;
    Option.iter (extract_from_expr referenced) e
  | ExpressionSequence (a, b) ->
    extract_from_expr referenced a;
    extract_from_expr referenced b
  | ExpressionConstraint (e, _) ->
    extract_from_expr referenced e
  | ExpressionRecord fields ->
    List.iter (fun f -> extract_from_expr referenced f.field_value) fields
  | ExpressionRecordAccess (e, _) ->
    extract_from_expr referenced e
  | ExpressionRecordUpdate (base, fields) ->
    extract_from_expr referenced base;
    List.iter (fun f -> extract_from_expr referenced f.field_value) fields
  | ExpressionMatch (scrutinee, arms) ->
    extract_from_expr referenced scrutinee;
    List.iter (fun arm ->
      extract_from_expr referenced arm.arm_expression;
      Option.iter (extract_from_expr referenced) arm.arm_guard
    ) arms
  | ExpressionModuleAccess (path, _) ->
    (* This is where external module references are! *)
    let modules = path.Location.value in
    begin match modules with
    | name :: _ -> referenced := name :: !referenced
    | [] -> ()
    end
  | ExpressionRef inner ->
    extract_from_expr referenced inner
  | ExpressionDeref inner ->
    extract_from_expr referenced inner
  | ExpressionAssign (ref_expr, value_expr) ->
    extract_from_expr referenced ref_expr;
    extract_from_expr referenced value_expr
  | ExpressionPolyVariant (_, arg) ->
    Option.iter (extract_from_expr referenced) arg
  | ExpressionPack (me, _) ->
    extract_from_module_expr referenced me
  | ExpressionLetModule (_, me, body) ->
    extract_from_module_expr referenced me;
    extract_from_expr referenced body
  | ExpressionAssert e ->
    extract_from_expr referenced e
  | ExpressionWhile (cond, body) ->
    extract_from_expr referenced cond;
    extract_from_expr referenced body
  | ExpressionFor (_, start_e, end_e, _, body) ->
    extract_from_expr referenced start_e;
    extract_from_expr referenced end_e;
    extract_from_expr referenced body
  | ExpressionLetOp (_, bindings, body) ->
    List.iter (fun b -> extract_from_expr referenced b.letop_expression) bindings;
    extract_from_expr referenced body
  | ExpressionError _ ->
    ()

(** Build dependency graph *)
type file_info = {
  filename : string;
  module_name : string;
  ast : Parsing.Syntax_tree.structure;
  imports : string list;
}

let parse_file filename =
  try
    let ic = open_in filename in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let ast = Parsing.Parse.structure_from_string source in
    let module_name = module_name_of_filename filename in
    let imports = extract_imports ast in
    Ok { filename; module_name; ast; imports }
  with
  | Sys_error msg -> Error msg
  | Compiler_error.Error err -> Error (Compiler_error.report_to_string err)

(** Topological sort of files by dependencies *)
let topological_sort (files : file_info list) : (file_info list, string) result =
  let module_map = List.fold_left (fun acc f ->
    (f.module_name, f) :: acc
  ) [] files in
  let find_file name = List.assoc_opt name module_map in

  let visited = Hashtbl.create (List.length files) in
  let in_progress = Hashtbl.create (List.length files) in
  let result = ref [] in

  let rec visit name =
    if Hashtbl.mem visited name then Ok ()
    else if Hashtbl.mem in_progress name then
      Error (Printf.sprintf "Cyclic dependency involving module %s" name)
    else begin
      match find_file name with
      | None -> Ok ()  (* External module, not in our files *)
      | Some file_info ->
        Hashtbl.add in_progress name true;
        let deps_result = List.fold_left (fun acc dep ->
          match acc with
          | Error _ -> acc
          | Ok () -> visit dep
        ) (Ok ()) file_info.imports in
        match deps_result with
        | Error _ as e -> e
        | Ok () ->
          Hashtbl.remove in_progress name;
          Hashtbl.add visited name true;
          result := file_info :: !result;
          Ok ()
    end
  in
  let sort_result = List.fold_left (fun acc f ->
    match acc with
    | Error _ -> acc
    | Ok () -> visit f.module_name
  ) (Ok ()) files in
  match sort_result with
  | Error _ as e -> e
  | Ok () -> Ok (List.rev !result)

(** Compile multiple files *)
type project_options = {
  output_dir : string option;
  entry_point : string option;
  pipeline_options : Pipeline.options;
}

let compile_project options files =
  (* Parse all files *)
  let parse_results = List.map parse_file files in
  let errors, file_infos = List.partition_map (function
    | Error e -> Left e
    | Ok info -> Right info
  ) parse_results in
  if errors <> [] then
    Error (String.concat "\n" errors)
  else begin
    (* Topological sort *)
    match topological_sort file_infos with
    | Error e -> Error e
    | Ok sorted_files ->
      (* Compile each file in order *)
      let compile_one file_info =
        let source_file = file_info.filename in
        let ic = open_in source_file in
        let source = really_input_string ic (in_channel_length ic) in
        close_in ic;

        (* Generate require statements for imports *)
        let requires = List.filter_map (fun imp ->
          (* Only require modules that are in our project *)
          if List.exists (fun f -> f.module_name = imp) sorted_files then
            Some (Printf.sprintf "local %s = require(\"%s\")" imp (String.lowercase_ascii imp))
          else
            None
        ) file_info.imports in
        let require_block = String.concat "\n" requires in

        (* Compile the source *)
        match Pipeline.compile_string options.pipeline_options source_file source with
        | Error e -> Error (Printf.sprintf "%s: %s" source_file e)
        | Ok lua_code ->
          (* Build module wrapper *)
          let module_name = file_info.module_name in
          let full_code = if require_block = "" then
            Printf.sprintf "-- Module: %s\nlocal %s = {}\n%s\nreturn %s\n"
              module_name module_name lua_code module_name
          else
            Printf.sprintf "-- Module: %s\n%s\n\nlocal %s = {}\n%s\nreturn %s\n"
              module_name require_block module_name lua_code module_name
          in
          Ok (file_info.module_name, full_code)
      in
      let results = List.map compile_one sorted_files in
      let errors, outputs = List.partition_map (function
        | Error e -> Left e
        | Ok out -> Right out
      ) results in
      if errors <> [] then
        Error (String.concat "\n" errors)
      else
        Ok outputs
  end
