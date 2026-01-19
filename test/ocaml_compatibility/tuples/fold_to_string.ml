(* Tuple: fold with type change *)
(* Expected: ACCEPT - fold can produce different type *)

let fold f (a, b) = f a b
let pair = (5, 3)
let combined = fold (fun a b -> a * 10 + b) pair
let () = print_int combined
