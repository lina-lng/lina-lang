(* Tuple: make constructor *)
(* Expected: ACCEPT - make creates a pair *)

let make a b = (a, b)
let pair = make 1 2
let x = fst pair
let y = snd pair
let () = print_int (x + y)
