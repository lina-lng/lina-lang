(** Semantic versioning. *)

type t = {
  major : int;
  minor : int;
  patch : int;
  revision : int option;
}
[@@deriving show, eq]

let make major minor patch =
  { major; minor; patch; revision = None }

let make_with_revision major minor patch revision =
  { major; minor; patch; revision = Some revision }

let parse_int_string str =
  if String.length str = 0 then None
  else if String.for_all (fun c -> c >= '0' && c <= '9') str then
    Some (int_of_string str)
  else
    None

let parse str =
  let str = String.trim str in

  let dot_parts = String.split_on_char '.' str in
  match dot_parts with
  | [major_s; minor_s; patch_part] ->
      let dash_parts = String.split_on_char '-' patch_part in
      (match dash_parts with
       | [patch_s] ->
           (match parse_int_string major_s, parse_int_string minor_s, parse_int_string patch_s with
            | Some major, Some minor, Some patch ->
                Ok { major; minor; patch; revision = None }
            | _ -> Error (Printf.sprintf "Invalid version format: %s" str))
       | [patch_s; rev_s] ->
           (match parse_int_string major_s, parse_int_string minor_s,
                  parse_int_string patch_s, parse_int_string rev_s with
            | Some major, Some minor, Some patch, Some rev ->
                Ok { major; minor; patch; revision = Some rev }
            | _ -> Error (Printf.sprintf "Invalid version format: %s" str))
       | _ -> Error (Printf.sprintf "Invalid version format: %s" str))
  | _ -> Error (Printf.sprintf "Invalid version format: %s" str)

let compare version_a version_b =
  let major_cmp = Int.compare version_a.major version_b.major in
  if major_cmp <> 0 then major_cmp else

  let minor_cmp = Int.compare version_a.minor version_b.minor in
  if minor_cmp <> 0 then minor_cmp else

  let patch_cmp = Int.compare version_a.patch version_b.patch in
  if patch_cmp <> 0 then patch_cmp else

  match version_a.revision, version_b.revision with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some rev_a, Some rev_b -> Int.compare rev_a rev_b

let to_string version =
  let base = Printf.sprintf "%d.%d.%d" version.major version.minor version.patch in
  match version.revision with
  | None -> base
  | Some rev -> Printf.sprintf "%s-%d" base rev

type constraint_ =
  | Any
  | Exact of t
  | GreaterEqual of t
  | Greater of t
  | LessEqual of t
  | Less of t
  | Caret of t
  | Tilde of t
  | And of constraint_ * constraint_
[@@deriving show, eq]

let constraint_operators = [
  (">=", fun v -> GreaterEqual v);
  ("<=", fun v -> LessEqual v);
  (">",  fun v -> Greater v);
  ("<",  fun v -> Less v);
  ("^",  fun v -> Caret v);
  ("~",  fun v -> Tilde v);
  ("=",  fun v -> Exact v);
]

let parse_single_constraint str =
  let str = String.trim str in

  if str = "*" || str = "" then
    Ok Any
  else
    let try_operator (prefix, make_constraint) =
      let prefix_len = String.length prefix in
      if String.length str >= prefix_len && String.sub str 0 prefix_len = prefix then
        let version_str = String.sub str prefix_len (String.length str - prefix_len) in
        Some (Result.map make_constraint (parse version_str))
      else
        None
    in

    match List.find_map try_operator constraint_operators with
    | Some result -> result
    | None -> Result.map (fun v -> Exact v) (parse str)

let parse_constraint str =
  let parts = String.split_on_char ',' str in
  let parsed_parts = List.map parse_single_constraint parts in

  let rec combine_results results =
    match results with
    | [] -> Ok Any
    | [single] -> single
    | first :: rest ->
      match first, combine_results rest with
      | Ok constraint_a, Ok constraint_b -> Ok (And (constraint_a, constraint_b))
      | Error err, _ -> Error err
      | _, Error err -> Error err
  in
  combine_results parsed_parts

let rec satisfies version constraint_ =
  match constraint_ with
  | Any -> true
  | Exact target -> compare version target = 0
  | GreaterEqual target -> compare version target >= 0
  | Greater target -> compare version target > 0
  | LessEqual target -> compare version target <= 0
  | Less target -> compare version target < 0
  | Caret target ->
    compare version target >= 0 &&
    version.major = target.major &&
    (target.major > 0 || version.minor = target.minor)
  | Tilde target ->
    compare version target >= 0 &&
    version.major = target.major &&
    version.minor = target.minor
  | And (constraint_a, constraint_b) ->
    satisfies version constraint_a && satisfies version constraint_b

let rec constraint_to_string constraint_ =
  match constraint_ with
  | Any -> "*"
  | Exact version -> to_string version
  | GreaterEqual version -> ">=" ^ to_string version
  | Greater version -> ">" ^ to_string version
  | LessEqual version -> "<=" ^ to_string version
  | Less version -> "<" ^ to_string version
  | Caret version -> "^" ^ to_string version
  | Tilde version -> "~" ^ to_string version
  | And (constraint_a, constraint_b) ->
    constraint_to_string constraint_a ^ ", " ^ constraint_to_string constraint_b
