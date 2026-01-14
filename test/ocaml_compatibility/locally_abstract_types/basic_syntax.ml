(* Locally abstract types: basic syntax *)
(* Expected: ACCEPT - (type a) introduces locally abstract type *)

let id (type a) (x : a) : a = x

let int_id = id 42
let bool_id = id true

let () = print_int int_id
