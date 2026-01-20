(** Lockfile implementation.

    Format (lina.lock):
    {v
    version = 1

    [[package]]
    name = "luasocket"
    version = "3.1.0-1"
    checksum = "abc123..."
    v} *)

type entry = {
  name : string;
  version : string;
  checksum : string;
}
[@@deriving show, eq]

type t = {
  lockfile_version : int;
  packages : entry list;
}
[@@deriving show, eq]

let empty = {
  lockfile_version = 1;
  packages = [];
}

let find_package name lockfile =
  List.find_opt (fun entry -> entry.name = name) lockfile.packages

let has_package name lockfile =
  List.exists (fun entry -> entry.name = name) lockfile.packages

let add_package entry lockfile =
  let packages =
    entry :: List.filter (fun existing -> existing.name <> entry.name) lockfile.packages
  in
  { lockfile with packages }

let remove_package name lockfile =
  let packages = List.filter (fun entry -> entry.name <> name) lockfile.packages in
  { lockfile with packages }

let get_string table key =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string key) table with
  | Some (Toml.Types.TString s) -> Some s
  | _ -> None

let get_int table key =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string key) table with
  | Some (Toml.Types.TInt i) -> Some i
  | _ -> None

let parse_entry table =
  match get_string table "name", get_string table "version", get_string table "checksum" with
  | Some name, Some version, Some checksum ->
    Ok { name; version; checksum }
  | Some name, Some version, None ->
    Ok { name; version; checksum = "" }
  | _ ->
    Error "Invalid package entry: missing name or version"

let parse_packages table =
  match Toml.Types.Table.find_opt (Toml.Types.Table.Key.of_string "package") table with
  | Some (Toml.Types.TArray (Toml.Types.NodeTable tables)) ->
    let rec parse_all acc = function
      | [] -> Ok (List.rev acc)
      | tbl :: rest ->
        match parse_entry tbl with
        | Ok entry -> parse_all (entry :: acc) rest
        | Error err -> Error err
    in
    parse_all [] tables
  | Some _ -> Error "Invalid [[package]] section: expected array of tables"
  | None -> Ok []

let load path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    match Toml.Parser.from_string content with
    | `Ok table ->
      let lockfile_version = get_int table "version" |> Option.value ~default:1 in
      (match parse_packages table with
       | Ok packages -> Ok { lockfile_version; packages }
       | Error err -> Error err)
    | `Error (msg, _loc) ->
      Error (Printf.sprintf "TOML parse error: %s" msg)
  with
  | Sys_error msg -> Error (Printf.sprintf "Cannot read file: %s" msg)

let to_string lockfile =
  let buffer = Buffer.create 256 in

  Buffer.add_string buffer "# Auto-generated lockfile. Do not edit.\n";
  Buffer.add_string buffer (Printf.sprintf "version = %d\n\n" lockfile.lockfile_version);

  List.iter (fun entry ->
    Buffer.add_string buffer "[[package]]\n";
    Buffer.add_string buffer (Printf.sprintf "name = %S\n" entry.name);
    Buffer.add_string buffer (Printf.sprintf "version = %S\n" entry.version);
    if entry.checksum <> "" then
      Buffer.add_string buffer (Printf.sprintf "checksum = %S\n" entry.checksum);
    Buffer.add_char buffer '\n';
  ) lockfile.packages;

  Buffer.contents buffer

let save path lockfile =
  try
    let content = to_string lockfile in
    Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content
    );
    Ok ()
  with
  | Sys_error msg -> Error (Printf.sprintf "Cannot write file: %s" msg)
