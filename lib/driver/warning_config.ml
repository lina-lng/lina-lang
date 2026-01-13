(** Warning configuration system implementation. *)

open Common

type level =
  | Allow
  | Warn
  | Deny
  | Forbid

module CodeMap = Map.Make(struct
  type t = Error_code.t
  let compare = Error_code.compare
end)

type t = {
  levels : level CodeMap.t;
  default_level : level;
}

let default = {
  levels = CodeMap.empty;
  default_level = Warn;
}

let set_level config code level =
  { config with levels = CodeMap.add code level config.levels }

let get_level config code =
  match CodeMap.find_opt code config.levels with
  | Some level -> level
  | None -> config.default_level

let set_all_warnings config level =
  let all_warnings = List.filter Error_code.is_warning (Error_code.all_codes ()) in
  List.fold_left (fun cfg code -> set_level cfg code level) config all_warnings

let enable_all config = set_all_warnings config Warn

let disable_all config = set_all_warnings config Allow

let warn_error_all config = set_all_warnings config Deny

(** Map of warning names to codes for command-line parsing. *)
let warning_names = [
  ("unused", Error_code.w_unused_variable);
  ("unused-variable", Error_code.w_unused_variable);
  ("non-exhaustive", Error_code.w_non_exhaustive);
  ("redundant", Error_code.w_redundant_pattern);
  ("redundant-pattern", Error_code.w_redundant_pattern);
  ("shadowing", Error_code.w_shadowing);
]

let find_warning_by_name name =
  (* Try exact name match first *)
  match List.assoc_opt (String.lowercase_ascii name) warning_names with
  | Some code -> Some code
  | None ->
    (* Try parsing as code (W0001) *)
    match Error_code.of_string (String.uppercase_ascii name) with
    | Some code when Error_code.is_warning code -> Some code
    | _ -> None

let level_to_string = function
  | Allow -> "allow"
  | Warn -> "warn"
  | Deny -> "deny"
  | Forbid -> "forbid"

let level_of_string = function
  | "allow" | "ignore" | "off" -> Some Allow
  | "warn" | "warning" | "on" -> Some Warn
  | "deny" | "error" -> Some Deny
  | "forbid" | "fatal" -> Some Forbid
  | _ -> None

let parse_spec config spec =
  let spec = String.trim spec in
  if spec = "" then Ok config
  else if spec = "+all" || spec = "all" then
    Ok (enable_all config)
  else if spec = "-all" then
    Ok (disable_all config)
  else
    (* Check for level assignment: name=level *)
    match String.split_on_char '=' spec with
    | [name; level_str] ->
      begin match find_warning_by_name name with
      | None -> Error (Printf.sprintf "Unknown warning: %s" name)
      | Some code ->
        match level_of_string (String.lowercase_ascii level_str) with
        | None -> Error (Printf.sprintf "Unknown level: %s (use allow/warn/deny/forbid)" level_str)
        | Some level -> Ok (set_level config code level)
      end
    | [name] ->
      (* +name enables, -name disables *)
      let enable, name =
        if String.length name > 0 && name.[0] = '+' then
          (true, String.sub name 1 (String.length name - 1))
        else if String.length name > 0 && name.[0] = '-' then
          (false, String.sub name 1 (String.length name - 1))
        else
          (true, name)  (* Default to enable *)
      in
      begin match find_warning_by_name name with
      | None -> Error (Printf.sprintf "Unknown warning: %s" name)
      | Some code ->
        let level = if enable then Warn else Allow in
        Ok (set_level config code level)
      end
    | _ -> Error (Printf.sprintf "Invalid warning spec: %s" spec)

let parse_specs config specs =
  List.fold_left (fun result spec ->
    match result with
    | Error _ -> result
    | Ok cfg -> parse_spec cfg spec
  ) (Ok config) specs

let should_report config code =
  match get_level config code with
  | Allow -> false
  | Warn | Deny | Forbid -> true

let is_error config code =
  match get_level config code with
  | Allow | Warn -> false
  | Deny | Forbid -> true

let is_fatal config code =
  match get_level config code with
  | Allow | Warn | Deny -> false
  | Forbid -> true
