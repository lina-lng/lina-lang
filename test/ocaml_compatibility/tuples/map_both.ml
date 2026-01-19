(* Tuple: map operation on both elements *)
(* Expected: ACCEPT - map transforms both elements *)

let map f g (a, b) = (f a, g b)
let pair = (3, 4)
let mapped = map (fun x -> x * 2) (fun y -> y * 3) pair
let x = fst mapped
let y = snd mapped
let () = print_int (x + y)
