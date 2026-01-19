(* Tuple: to_list requires same type *)
(* Expected: ACCEPT - to_list works on homogeneous pairs *)

let to_list (a, b) = [a; b]
let pair = (10, 20)
let lst = to_list pair
let first = match lst with x :: _ -> x | [] -> 0
let () = print_int first
