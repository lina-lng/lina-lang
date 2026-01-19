(* Tuple: fst accessor *)
(* Expected: ACCEPT - fst returns first element *)

let pair = (42, "hello")
let x = fst pair
let () = print_int x
