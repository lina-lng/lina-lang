(* Tuple: to_list conversion *)
(* Expected: ACCEPT - to_list converts pair to list *)

let to_list (a, b) = [a; b]
let pair = (1, 2)
let lst = to_list pair
let len = List.length lst
let () = print_int len
