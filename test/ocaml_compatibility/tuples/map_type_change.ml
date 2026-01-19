(* Tuple: map changes types *)
(* Expected: ACCEPT - map can change element types *)

let map f g (a, b) = (f a, g b)
let pair = (42, 10)
let mapped = map (fun n -> n > 0) (fun n -> n * 2) pair
let b = fst mapped
let () = print_int (if b then 1 else 0)
