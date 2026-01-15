(* Or-patterns in function expression *)

type color = Red | Green | Blue

let is_primary = function
  | Red | Blue -> true
  | Green -> false

let () = print_string (string_of_bool (is_primary Red)); print_newline ()
let () = print_string (string_of_bool (is_primary Green)); print_newline ()
let () = print_string (string_of_bool (is_primary Blue)); print_newline ()
