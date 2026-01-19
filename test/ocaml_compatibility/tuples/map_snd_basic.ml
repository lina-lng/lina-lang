(* Tuple: map_snd operation *)
(* Expected: ACCEPT - map_snd transforms second element *)

let map_snd f (a, b) = (a, f b)
let pair = (5, 10)
let mapped = map_snd (fun x -> x + 1) pair
let y = snd mapped
let () = print_int y
