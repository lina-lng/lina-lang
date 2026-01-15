(* Or-patterns: different variables in branches *)
(* Expected: REJECT - x and y are different variable names *)

type result = Ok of int | Error of int

let get_value = function
  | Ok x | Error y -> x

let () = print_int (get_value (Ok 42))
