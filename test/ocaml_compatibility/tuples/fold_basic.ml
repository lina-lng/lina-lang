(* Tuple: fold operation *)
(* Expected: ACCEPT - fold combines elements *)

let fold f (a, b) = f a b
let pair = (3, 7)
let sum = fold (fun a b -> a + b) pair
let () = print_int sum
