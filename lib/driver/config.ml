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

type warnings = {
  default_level : Warning_config.level;
  overrides : (string * Warning_config.level) list;
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
  default_level = Warning_config.Warn;
  overrides = [];
}

let default_config name = {
  package = { name; version = "0.1.0"; edition = "2025" };
  build = default_build;
  warnings = default_warnings;
}

(** Parse warning level from string. *)
let parse_warning_level str =
  match String.lowercase_ascii str with
  | "allow" | "ignore" | "off" -> Some Warning_config.Allow
  | "warn" | "warning" | "on" -> Some Warning_config.Warn
  | "deny" | "error" -> Some Warning_config.Deny
  | "forbid" | "fatal" -> Some Warning_config.Forbid
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

(** Parse [warnings] section. *)
let parse_warnings table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "warnings") table with
  | Some (Toml.Types.TTable warn_table) ->
    let default_level =
      match get_string warn_table "default" with
      | Some s ->
        (match parse_warning_level s with
         | Some level -> level
         | None -> Warning_config.Warn)
      | None -> Warning_config.Warn
    in
    (* Parse per-warning overrides *)
    let overrides =
      Toml.Types.Table.fold (fun key value acc ->
        let key_str = Toml.Types.Table.Key.to_string key in
        if key_str = "default" then acc
        else match value with
          | Toml.Types.TString level_str ->
            (match parse_warning_level level_str with
             | Some level -> (key_str, level) :: acc
             | None -> acc)
          | _ -> acc
      ) warn_table []
    in
    Ok { default_level; overrides }
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

let to_warning_config warnings =
  let base = match warnings.default_level with
    | Warning_config.Allow -> Warning_config.disable_all Warning_config.default
    | Warning_config.Warn -> Warning_config.default
    | Warning_config.Deny -> Warning_config.warn_error_all Warning_config.default
    | Warning_config.Forbid -> Warning_config.warn_error_all Warning_config.default
  in
  (* Apply overrides *)
  List.fold_left (fun config (name, level) ->
    match Warning_config.parse_spec config (name ^ "=" ^ Warning_config.level_to_string level) with
    | Ok config -> config
    | Error _ -> config  (* Silently ignore invalid warning names *)
  ) base warnings.overrides
