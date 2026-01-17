(** Project configuration implementation. *)

type package = {
  name : string;
  version : string;
  edition : string;
}

type build = {
  target : string;
  source_dir : string;
  output_dir : string;
}

type preset = Strict | Relaxed

type path_override = {
  path_pattern : string;
  path_preset : preset option;
  path_overrides : (string * Common.Warning_config.level) list;
}

type warnings = {
  preset : preset;
  overrides : (string * Common.Warning_config.level) list;
  by_path : path_override list;
}

type t = {
  package : package;
  build : build;
  warnings : warnings;
}

let default_build = {
  target = "lua53";
  source_dir = "src";
  output_dir = "_build";
}

let default_warnings = {
  preset = Strict;
  overrides = [];
  by_path = [];
}

let default_config name = {
  package = { name; version = "0.1.0"; edition = "2025" };
  build = default_build;
  warnings = default_warnings;
}

(** Parse warning level from string. *)
let parse_warning_level str =
  match String.lowercase_ascii str with
  | "allow" | "ignore" | "off" -> Some Common.Warning_config.Allow
  | "warn" | "warning" | "on" -> Some Common.Warning_config.Warn
  | "deny" | "error" -> Some Common.Warning_config.Deny
  | "forbid" | "fatal" -> Some Common.Warning_config.Forbid
  | _ -> None

(** Get string value from TOML table. *)
let get_string table key =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string key) table with
  | Some (Toml.Types.TString s) -> Some s
  | _ -> None

(** Get string value with default. *)
let get_string_or table key default =
  match get_string table key with
  | Some s -> s
  | None -> default

(** Parse [package] section. *)
let parse_package table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "package") table with
  | Some (Toml.Types.TTable pkg_table) ->
    let name = get_string_or pkg_table "name" "unnamed" in
    let version = get_string_or pkg_table "version" "0.1.0" in
    let edition = get_string_or pkg_table "edition" "2025" in
    Ok { name; version; edition }
  | Some _ -> Error "Invalid [package] section: expected table"
  | None -> Error "Missing [package] section"

(** Parse [build] section. *)
let parse_build table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "build") table with
  | Some (Toml.Types.TTable build_table) ->
    let target = get_string_or build_table "target" "lua53" in
    let source_dir = get_string_or build_table "source_dir" "src" in
    let output_dir = get_string_or build_table "output_dir" "_build" in
    Ok { target; source_dir; output_dir }
  | Some _ -> Error "Invalid [build] section: expected table"
  | None -> Ok default_build

(** Parse preset from string. Defaults to Strict. *)
let parse_preset str =
  match String.lowercase_ascii str with
  | "strict" -> Strict
  | "relaxed" -> Relaxed
  | _ -> Strict

(** Parse a single path override entry. *)
let parse_path_override pattern override_table =
  let path_preset =
    match get_string override_table "preset" with
    | Some s -> Some (parse_preset s)
    | None -> None
  in
  let path_overrides =
    Toml.Types.Table.fold (fun key value acc ->
      let key_str = Toml.Types.Table.Key.to_string key in
      if key_str = "preset" then acc
      else match value with
        | Toml.Types.TString level_str ->
          (match parse_warning_level level_str with
           | Some level -> (key_str, level) :: acc
           | None -> acc)
        | _ -> acc
    ) override_table []
  in
  { path_pattern = pattern; path_preset; path_overrides }

(** Parse [warnings.by-path] section. *)
let parse_by_path warn_table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "by-path") warn_table with
  | Some (Toml.Types.TTable by_path_table) ->
    Toml.Types.Table.fold (fun key value acc ->
      let pattern = Toml.Types.Table.Key.to_string key in
      match value with
      | Toml.Types.TTable override_table ->
        parse_path_override pattern override_table :: acc
      | _ -> acc
    ) by_path_table []
  | _ -> []

(** Parse [warnings] section. *)
let parse_warnings table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "warnings") table with
  | Some (Toml.Types.TTable warn_table) ->
    let preset =
      match get_string warn_table "preset" with
      | Some s -> parse_preset s
      | None -> Strict
    in
    let overrides =
      Toml.Types.Table.fold (fun key value acc ->
        let key_str = Toml.Types.Table.Key.to_string key in
        if key_str = "preset" || key_str = "by-path" then acc
        else match value with
          | Toml.Types.TString level_str ->
            (match parse_warning_level level_str with
             | Some level -> (key_str, level) :: acc
             | None -> acc)
          | _ -> acc
      ) warn_table []
    in
    let by_path = parse_by_path warn_table in
    Ok { preset; overrides; by_path }
  | Some _ -> Error "Invalid [warnings] section: expected table"
  | None -> Ok default_warnings

let load_file path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    match Toml.Parser.from_string content with
    | `Ok table ->
      (match parse_package table with
       | Error e -> Error e
       | Ok package ->
         match parse_build table with
         | Error e -> Error e
         | Ok build ->
           match parse_warnings table with
           | Error e -> Error e
           | Ok warnings ->
             Ok { package; build; warnings })
    | `Error (msg, _loc) ->
      Error (Printf.sprintf "TOML parse error: %s" msg)
  with
  | Sys_error msg -> Error (Printf.sprintf "Cannot read file: %s" msg)

let load_project_config root =
  let config_path = Filename.concat root "lina.toml" in
  if Sys.file_exists config_path then
    load_file config_path
  else
    Error "No lina.toml found in project root"

(** Simple glob pattern matching. Supports * and ** wildcards. *)
let glob_match pattern path =
  let rec match_parts pattern_parts path_parts =
    match pattern_parts, path_parts with
    | [], [] -> true
    | ["**"], _ -> true
    | "**" :: rest, _ :: path_rest ->
      match_parts pattern_parts path_rest || match_parts rest path_parts
    | "*" :: rest, _ :: path_rest ->
      match_parts rest path_rest
    | p :: rest, path_p :: path_rest when p = path_p ->
      match_parts rest path_rest
    | _ :: _, [] -> false
    | [], _ :: _ -> false
    | _ :: _, _ :: _ -> false
  in
  let pattern_parts = String.split_on_char '/' pattern in
  let path_parts = String.split_on_char '/' path in
  match_parts pattern_parts path_parts

(** Find matching path override for a file. *)
let find_path_override warnings file_path =
  List.find_opt (fun override ->
    glob_match override.path_pattern file_path
  ) warnings.by_path

let to_warning_config warnings =
  let base = match warnings.preset with
    | Strict -> Common.Warning_config.default
    | Relaxed -> Common.Warning_config.relaxed
  in
  List.fold_left (fun config (name, level) ->
    match Common.Warning_config.parse_spec config (name ^ "=" ^ Common.Warning_config.level_to_string level) with
    | Ok config -> config
    | Error _ -> config
  ) base warnings.overrides

(** Get warning config for a specific file, applying path-specific overrides. *)
let warning_config_for_file warnings file_path =
  let base_config = to_warning_config warnings in
  match find_path_override warnings file_path with
  | None -> base_config
  | Some override ->
    let with_preset = match override.path_preset with
      | Some Strict -> Common.Warning_config.default
      | Some Relaxed -> Common.Warning_config.relaxed
      | None -> base_config
    in
    List.fold_left (fun config (name, level) ->
      match Common.Warning_config.parse_spec config (name ^ "=" ^ Common.Warning_config.level_to_string level) with
      | Ok config -> config
      | Error _ -> config
    ) with_preset override.path_overrides
