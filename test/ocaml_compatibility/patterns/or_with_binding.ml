(* Or-patterns: both branches bind same variable *)
(* Expected: ACCEPT - x is bound in both branches with same type *)

type result = Ok of int | Error of int

let get_value = function
  | Ok x | Error x -> x

let () = print_int (get_value (Ok 42))
