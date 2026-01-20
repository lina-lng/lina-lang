(** LuaRocks CLI wrapper. *)

type error =
  | NotInstalled
  | CommandFailed of { command : string; exit_code : int; stderr : string }
  | ParseError of string
[@@deriving show]

type package_info = {
  name : string;
  version : string;
  install_path : string;
}
[@@deriving show, eq]

type command_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

let run_command program args =
  let cmd_str = String.concat " " (program :: args) in
  let env = Unix.environment () in
  let stdout_chan, stdin_chan, stderr_chan = Unix.open_process_full cmd_str env in

  close_out stdin_chan;

  let stdout_content = In_channel.input_all stdout_chan in
  let stderr_content = In_channel.input_all stderr_chan in

  let status = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan) in
  let exit_code = match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED signal -> 128 + signal
  in

  { exit_code; stdout = stdout_content; stderr = stderr_content }

let is_available () =
  let result = run_command "luarocks" ["--version"] in
  result.exit_code = 0

let extract_version_number output =
  let trimmed = String.trim output in
  match String.split_on_char ' ' trimmed with
  | "LuaRocks" :: version_str :: _ ->
      let is_version_char c = (c >= '0' && c <= '9') || c = '.' in
      if String.length version_str > 0 && String.for_all is_version_char version_str
      then Some version_str
      else None
  | _ -> None

let version () =
  let result = run_command "luarocks" ["--version"] in

  if result.exit_code <> 0 then
    Error NotInstalled
  else
    match extract_version_number result.stdout with
    | Some ver -> Ok ver
    | None -> Error (ParseError (Printf.sprintf "Cannot parse version from: %s%s" result.stdout result.stderr))

let install ~name ?version ~tree () =
  let version_arg = match version with
    | Some ver -> [ver]
    | None -> []
  in
  let args = ["install"; name] @ version_arg @ ["--tree"; tree] in
  let result = run_command "luarocks" args in

  if result.exit_code = 0 then Ok ()
  else Error (CommandFailed {
    command = "luarocks " ^ String.concat " " args;
    exit_code = result.exit_code;
    stderr = result.stderr;
  })

let remove ~name ~tree () =
  let args = ["remove"; name; "--tree"; tree] in
  let result = run_command "luarocks" args in

  if result.exit_code = 0 then Ok ()
  else Error (CommandFailed {
    command = "luarocks " ^ String.concat " " args;
    exit_code = result.exit_code;
    stderr = result.stderr;
  })

let parse_porcelain_line line =
  let trimmed = String.trim line in
  if trimmed = "" then None
  else
    match String.split_on_char '\t' trimmed with
    | pkg_name :: pkg_version :: _ -> Some { name = pkg_name; version = pkg_version; install_path = "" }
    | [pkg_name] -> Some { name = pkg_name; version = ""; install_path = "" }
    | [] -> None

let list_installed ~tree =
  let args = ["list"; "--tree"; tree; "--porcelain"] in
  let result = run_command "luarocks" args in

  if result.exit_code <> 0 then
    Error (CommandFailed {
      command = "luarocks " ^ String.concat " " args;
      exit_code = result.exit_code;
      stderr = result.stderr;
    })
  else
    let lines = String.split_on_char '\n' result.stdout in
    let packages = List.filter_map (fun line ->
      match parse_porcelain_line line with
      | Some pkg -> Some { pkg with install_path = tree }
      | None -> None
    ) lines in
    Ok packages

let extract_package_version output =
  let first_line = match String.split_on_char '\n' output with
    | line :: _ -> String.trim line
    | [] -> ""
  in

  match String.index_opt first_line ' ' with
  | None -> ""
  | Some space_idx ->
      let after_space = String.sub first_line (space_idx + 1) (String.length first_line - space_idx - 1) in
      let version_str = String.trim after_space in
      let is_version_char c =
        (c >= '0' && c <= '9') || c = '.' || c = '-' || (c >= 'a' && c <= 'z')
      in
      let end_idx = ref 0 in
      while !end_idx < String.length version_str && is_version_char version_str.[!end_idx] do
        incr end_idx
      done;
      if !end_idx > 0 then String.sub version_str 0 !end_idx else ""

let show ~name ~tree () =
  let args = ["show"; name; "--tree"; tree] in
  let result = run_command "luarocks" args in

  if result.exit_code <> 0 then Ok None
  else
    let pkg_version = extract_package_version result.stdout in
    Ok (Some { name; version = pkg_version; install_path = tree })

let get_paths ~tree : Types.lua_paths =
  let lib_dir = Filename.concat tree "share/lua/5.1" in
  let clib_dir = Filename.concat tree "lib/lua/5.1" in

  let lua_path =
    if Sys.file_exists lib_dir then
      [Filename.concat lib_dir "?.lua";
       Filename.concat lib_dir "?/init.lua"]
    else []
  in

  let lua_cpath =
    if Sys.file_exists clib_dir then
      [Filename.concat clib_dir "?.so"]
    else []
  in

  { Types.lua_path; lua_cpath }
