(* Tuple: fst is polymorphic *)
(* Expected: ACCEPT - fst works on any pair type *)

let get_first p = fst p
let x = get_first (1, "a")
let y = get_first ("hello", true)
let () = print_int x
