(* Tuple: fst only works on pairs *)
(* Expected: REJECT - cannot use fst on triple *)

let triple = (1, 2, 3)
let x = fst triple
let () = print_int x
