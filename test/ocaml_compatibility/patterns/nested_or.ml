(* Or-patterns: nested in tuple *)
(* Expected: ACCEPT - or-patterns can be nested in tuples *)

type ab = A | B
type cd = C | D

let both_first = function
  | ((A | B), (C | D)) -> true

let () = if both_first (A, C) then print_string "yes" else print_string "no"
