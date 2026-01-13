(** File watching implementation using polling. *)

type file_state = {
  path : string;
  mtime : float;
}

type t = {
  dir : string;
  extensions : string list;
}

let create ~dir ~extensions = { dir; extensions }

(** Check if a file has one of the watched extensions. *)
let has_extension extensions path =
  List.exists (fun ext -> Filename.check_suffix path ext) extensions

(** Recursively collect all watched files in a directory. *)
let rec collect_files extensions acc dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    let entries = Sys.readdir dir |> Array.to_list in
    List.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        collect_files extensions acc path
      else if has_extension extensions path then
        path :: acc
      else
        acc
    ) acc entries
  else
    acc

(** Get modification time of a file, or 0.0 if not accessible. *)
let get_mtime path =
  try (Unix.stat path).Unix.st_mtime
  with Unix.Unix_error _ -> 0.0

let get_current_state watcher =
  let files = collect_files watcher.extensions [] watcher.dir in
  List.map (fun path -> { path; mtime = get_mtime path }) files

let detect_changes watcher ~previous =
  let current = get_current_state watcher in
  let prev_map =
    List.fold_left (fun map state ->
      Hashtbl.add map state.path state.mtime; map
    ) (Hashtbl.create 64) previous
  in
  (* Find new or modified files *)
  let changed = List.filter (fun state ->
    match Hashtbl.find_opt prev_map state.path with
    | None -> true  (* New file *)
    | Some prev_mtime -> state.mtime > prev_mtime  (* Modified *)
  ) current in
  (* Find deleted files *)
  let current_paths =
    List.fold_left (fun set state ->
      Hashtbl.add set state.path (); set
    ) (Hashtbl.create 64) current
  in
  let deleted = List.filter (fun state ->
    not (Hashtbl.mem current_paths state.path)
  ) previous in
  List.map (fun s -> s.path) changed @ List.map (fun s -> s.path) deleted

let wait_for_changes watcher ~previous ~poll_interval_ms =
  let poll_interval_sec = float_of_int poll_interval_ms /. 1000.0 in
  let rec loop () =
    let changes = detect_changes watcher ~previous in
    if changes <> [] then
      changes
    else begin
      Unix.sleepf poll_interval_sec;
      loop ()
    end
  in
  loop ()

let clear_screen () =
  (* ANSI escape sequence to clear screen and move cursor to top-left *)
  print_string "\027[2J\027[H";
  flush stdout
