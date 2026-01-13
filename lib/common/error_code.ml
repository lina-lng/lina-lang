type kind = Error | Warning

type t = {
  kind : kind;
  number : int;
  description : string;
}

let make_error number description = { kind = Error; number; description }
let make_warning number description = { kind = Warning; number; description }

(* Error codes E0001-E9999 *)
let e_type_mismatch = make_error 1 "type mismatch"
let e_unbound_value = make_error 2 "unbound value"
let e_unbound_type = make_error 3 "unbound type"
let e_unbound_module = make_error 4 "unbound module"
let e_unbound_constructor = make_error 5 "unbound constructor"
let e_pattern_not_exhaustive = make_error 6 "non-exhaustive pattern matching"
let e_redundant_pattern = make_error 7 "redundant pattern"
let e_occurs_check = make_error 8 "infinite type (occurs check)"
let e_arity_mismatch = make_error 9 "arity mismatch"
let e_signature_mismatch = make_error 10 "signature mismatch"
let e_syntax_error = make_error 11 "syntax error"
let e_lexer_error = make_error 12 "lexical error"
let e_unbound_module_type = make_error 13 "unbound module type"
let e_unbound_field = make_error 14 "unbound record field"
let e_duplicate_definition = make_error 15 "duplicate definition"
let e_recursive_type = make_error 16 "recursive type without indirection"

(* Warning codes W0001-W9999 *)
let w_unused_variable = make_warning 1 "unused variable"
let w_non_exhaustive = make_warning 2 "non-exhaustive pattern matching"
let w_redundant_pattern = make_warning 3 "redundant pattern"
let w_shadowing = make_warning 4 "shadowed binding"

let to_string code =
  let prefix = match code.kind with Error -> "E" | Warning -> "W" in
  Printf.sprintf "%s%04d" prefix code.number

let to_int code = code.number

let is_error code = code.kind = Error
let is_warning code = code.kind = Warning

let description code = code.description

let all_codes () = [
  (* Errors *)
  e_type_mismatch;
  e_unbound_value;
  e_unbound_type;
  e_unbound_module;
  e_unbound_constructor;
  e_pattern_not_exhaustive;
  e_redundant_pattern;
  e_occurs_check;
  e_arity_mismatch;
  e_signature_mismatch;
  e_syntax_error;
  e_lexer_error;
  e_unbound_module_type;
  e_unbound_field;
  e_duplicate_definition;
  e_recursive_type;
  (* Warnings *)
  w_unused_variable;
  w_non_exhaustive;
  w_redundant_pattern;
  w_shadowing;
]

let of_string str =
  if String.length str < 2 then None
  else
    let prefix = str.[0] in
    let num_str = String.sub str 1 (String.length str - 1) in
    match int_of_string_opt num_str with
    | None -> None
    | Some number ->
      let kind = match prefix with
        | 'E' | 'e' -> Some Error
        | 'W' | 'w' -> Some Warning
        | _ -> None
      in
      match kind with
      | None -> None
      | Some kind ->
        (* Look up the actual code to get the description *)
        List.find_opt (fun code -> code.kind = kind && code.number = number) (all_codes ())

let pp fmt code = Format.fprintf fmt "%s" (to_string code)

let compare a b =
  match compare a.kind b.kind with
  | 0 -> Int.compare a.number b.number
  | cmp -> cmp

let equal a b = compare a b = 0
