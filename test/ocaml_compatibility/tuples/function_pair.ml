(* Tuple: pair of functions *)
(* Expected: ACCEPT - can store functions in pairs *)

let fn_pair = ((fun x -> x + 1), (fun x -> x * 2))
let add_one = fst fn_pair
let result = add_one 5
let () = print_int result
