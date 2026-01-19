(** Test: Array.fold_left type inference
    Expected: ACCEPT

    Verifies Array.fold_left has type ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a *)

let int_arr = Array.init 5 (fun i -> i + 1)
let sum = Array.fold_left (fun acc x -> acc + x) 0 int_arr

let str_arr = Array.init 3 (fun i -> if i = 0 then "a" else if i = 1 then "b" else "c")
let concat = Array.fold_left (fun acc s -> acc ^ s) "" str_arr

let () = print_int sum
let () = print_string concat
