(* Tuple: nested tuple access *)
(* Expected: ACCEPT - can access nested tuples *)

let nested = ((1, 2), (3, 4))
let inner = fst nested
let x = fst inner
let () = print_int x
