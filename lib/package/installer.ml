(** Package installer. *)

type install_result = {
  lockfile : Lockfile.t;
  installed_count : int;
  skipped_count : int;
}
[@@deriving show]

type error =
  | LuarocksNotAvailable
  | InstallFailed of string * string
  | DirectoryError of string
[@@deriving show]

let lina_dir = Types.lina_dir
let bindings_dir = Types.bindings_dir
let lockfile_path = Types.lockfile_path

let ensure_dirs root =
  let dirs = [lina_dir root; bindings_dir root] in

  try
    List.iter (fun dir ->
      if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
    ) dirs;
    Ok ()
  with Unix.Unix_error (err, _, path) ->
    Error (DirectoryError (Printf.sprintf "Cannot create %s: %s" path (Unix.error_message err)))

let get_lua_paths root =
  Luarocks.get_paths ~tree:(lina_dir root)

let install_one ~tree ~existing_lock (dep : Types.luarocks_dependency) =
  let pkg_name = dep.name in
  let version_str = dep.version_constraint in

  match Lockfile.find_package pkg_name existing_lock with
  | Some entry -> `Skipped entry
  | None ->
      let version = if version_str = "*" || version_str = "" then None else Some version_str in

      match Luarocks.install ~name:pkg_name ?version ~tree () with
      | Error err -> `Failed (pkg_name, Luarocks.show_error err)
      | Ok () ->
          let actual_version = match Luarocks.show ~name:pkg_name ~tree () with
            | Ok (Some info) -> info.Luarocks.version
            | _ -> Option.value version ~default:"unknown"
          in
          let entry = Lockfile.{ name = pkg_name; version = actual_version; checksum = "" } in
          `Installed entry

let load_existing_lockfile root =
  let path = lockfile_path root in
  if Sys.file_exists path then
    match Lockfile.load path with
    | Ok lock -> lock
    | Error _ -> Lockfile.empty
  else
    Lockfile.empty

let install_all ~root ~(dependencies : Types.dependencies) ?(include_dev = false) () =
  if not (Luarocks.is_available ()) then
    Error LuarocksNotAvailable
  else

  match ensure_dirs root with
  | Error err -> Error err
  | Ok () ->
      let tree = lina_dir root in
      let existing_lock = load_existing_lockfile root in

      let deps_to_install =
        if include_dev then dependencies.luarocks @ dependencies.dev_luarocks
        else dependencies.luarocks
      in

      let rec install_loop lockfile installed skipped = function
        | [] -> Ok { lockfile; installed_count = installed; skipped_count = skipped }
        | dep :: rest when dep.Types.optional ->
            install_loop lockfile installed (skipped + 1) rest
        | dep :: rest ->
            match install_one ~tree ~existing_lock:lockfile dep with
            | `Installed entry ->
                let new_lock = Lockfile.add_package entry lockfile in
                install_loop new_lock (installed + 1) skipped rest
            | `Skipped _ ->
                install_loop lockfile installed (skipped + 1) rest
            | `Failed (pkg_name, msg) ->
                Error (InstallFailed (pkg_name, msg))
      in

      match install_loop existing_lock 0 0 deps_to_install with
      | Error err -> Error err
      | Ok install_result ->
          let lock_path = lockfile_path root in
          match Lockfile.save lock_path install_result.lockfile with
          | Ok () -> Ok install_result
          | Error msg -> Error (DirectoryError msg)

let ensure_installed ~root ~dependencies () =
  match install_all ~root ~dependencies () with
  | Ok install_result -> Ok install_result.lockfile
  | Error err -> Error err
