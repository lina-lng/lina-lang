(* Tuple: iter requires same type *)
(* Expected: ACCEPT - iter works on homogeneous pairs *)

let iter f (a, b) = f a; f b
let pair = (1, 2)
let result = ref 0
let () = iter (fun x -> result := !result + x) pair
let () = print_int !result
