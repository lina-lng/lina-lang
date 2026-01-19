(* Tuple: iter operation *)
(* Expected: ACCEPT - iter applies function to both elements *)

let iter f (a, b) = f a; f b
let counter = ref 0
let pair = (10, 20)
let () = iter (fun x -> counter := !counter + x) pair
let () = print_int !counter
