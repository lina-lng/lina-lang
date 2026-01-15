(* Or-pattern with guard *)

type t = A of int | B of int | C

let is_positive x =
  match x with
  | A n | B n when n > 0 -> true
  | A _ | B _ -> false
  | C -> false

let () = print_string (string_of_bool (is_positive (A 5))); print_newline ()
let () = print_string (string_of_bool (is_positive (A (-1)))); print_newline ()
let () = print_string (string_of_bool (is_positive (B 10))); print_newline ()
let () = print_string (string_of_bool (is_positive C)); print_newline ()
