(* Or-patterns: basic usage *)
(* Expected: ACCEPT - simple A | B pattern should work *)

type color = Red | Green | Blue

let is_primary = function
  | Red | Blue -> true
  | Green -> false

let () = if is_primary Red then print_string "yes" else print_string "no"
