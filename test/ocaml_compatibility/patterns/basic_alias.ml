(* Alias patterns: basic usage *)
(* Expected: ACCEPT - x as y binds both x and y *)

let extract_both = function
  | (x as y) -> x + y

let () = print_int (extract_both 21)
