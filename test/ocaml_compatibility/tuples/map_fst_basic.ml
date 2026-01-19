(* Tuple: map_fst operation *)
(* Expected: ACCEPT - map_fst transforms first element *)

let map_fst f (a, b) = (f a, b)
let pair = (5, 10)
let mapped = map_fst (fun x -> x * 2) pair
let x = fst mapped
let () = print_int x
