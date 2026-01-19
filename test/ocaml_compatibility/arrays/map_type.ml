(** Test: Array.map type inference
    Expected: ACCEPT

    Verifies Array.map has type ('a -> 'b) -> 'a array -> 'b array *)

let int_arr = Array.make 3 1
let str_arr = Array.map (fun x -> if x > 0 then "positive" else "zero") int_arr

let double_arr = Array.map (fun x -> x * 2) int_arr

let () = print_int (Array.length str_arr)
let () = print_int (Array.get double_arr 0)
