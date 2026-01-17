(** Lua identifier mangling for reserved keywords.

    See {!Identifier_mangle} for documentation. *)

open Common

(** Lua 5.1+ reserved keywords that must be mangled.
    Uses a hash table for O(1) lookup instead of O(n) list search. *)
let lua_keywords_set : (string, unit) Hashtbl.t =
  let table = Hashtbl.create 32 in
  List.iter (fun keyword -> Hashtbl.add table keyword ())
    ["and"; "break"; "do"; "else"; "elseif"; "end"; "false"; "for";
     "function"; "goto"; "if"; "in"; "local"; "nil"; "not"; "or";
     "repeat"; "return"; "then"; "true"; "until"; "while"];
  table

let is_lua_keyword name = Hashtbl.mem lua_keywords_set name

(** Sanitize a name to be a valid Lua identifier.
    Replaces special characters with underscores or descriptive suffixes. *)
let sanitize_name name =
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char buf c
    | '(' -> Buffer.add_string buf "_lp_"
    | ')' -> Buffer.add_string buf "_rp_"
    | '*' -> Buffer.add_string buf "_star_"
    | '+' -> Buffer.add_string buf "_plus_"
    | '-' -> Buffer.add_string buf "_minus_"
    | '/' -> Buffer.add_string buf "_slash_"
    | '&' -> Buffer.add_string buf "_amp_"
    | '|' -> Buffer.add_string buf "_bar_"
    | '!' -> Buffer.add_string buf "_bang_"
    | '?' -> Buffer.add_string buf "_qmark_"
    | '=' -> Buffer.add_string buf "_eq_"
    | '<' -> Buffer.add_string buf "_lt_"
    | '>' -> Buffer.add_string buf "_gt_"
    | '@' -> Buffer.add_string buf "_at_"
    | '^' -> Buffer.add_string buf "_caret_"
    | '~' -> Buffer.add_string buf "_tilde_"
    | '.' -> Buffer.add_string buf "_dot_"
    | ':' -> Buffer.add_string buf "_colon_"
    | '$' -> Buffer.add_string buf "_dollar_"
    | '%' -> Buffer.add_string buf "_percent_"
    | ' ' -> ()  (* Skip spaces *)
    | _ -> Buffer.add_char buf '_'  (* Replace other chars with underscore *)
  ) name;
  let result = Buffer.contents buf in
  (* Ensure the name starts with a letter or underscore *)
  if String.length result = 0 then "_anon"
  else if result.[0] >= '0' && result.[0] <= '9' then "_" ^ result
  else result

(** Get the base name for an identifier (sanitized, with keyword handling, no stamp).
    Used by smart name generation to track name usage. *)
let get_base_name (id : Identifier.t) : string =
  let name = Identifier.name id in
  let sanitized = sanitize_name name in
  if is_lua_keyword sanitized then "_" ^ sanitized else sanitized

let mangle_identifier (id : Identifier.t) : Lua_ast.identifier =
  let base_name = get_base_name id in
  let stamp = Identifier.stamp id in
  if stamp = 0 then base_name
  else Printf.sprintf "%s_%d" base_name stamp
