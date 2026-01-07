(** Lua identifier mangling for reserved keywords.

    See {!Identifier_mangle} for documentation. *)

open Common

(** Lua 5.1+ reserved keywords that must be mangled. *)
let lua_reserved_keywords = [
  "and"; "break"; "do"; "else"; "elseif"; "end"; "false"; "for";
  "function"; "goto"; "if"; "in"; "local"; "nil"; "not"; "or";
  "repeat"; "return"; "then"; "true"; "until"; "while"
]

let is_lua_keyword name = List.mem name lua_reserved_keywords

let mangle_identifier (id : Identifier.t) : Lua_ast.identifier =
  let name = Identifier.name id in
  let stamp = Identifier.stamp id in
  let base_name = if is_lua_keyword name then "_" ^ name else name in
  if stamp = 0 then base_name
  else Printf.sprintf "%s_%d" base_name stamp
