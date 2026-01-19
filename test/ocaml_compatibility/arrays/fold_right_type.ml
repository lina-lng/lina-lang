(** Test: Array.fold_right type inference
    Expected: ACCEPT

    Verifies Array.fold_right has type ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b *)

let arr = Array.init 3 (fun i -> i + 1)
let diff = Array.fold_right (fun x acc -> x - acc) arr 0

let () = print_int diff
