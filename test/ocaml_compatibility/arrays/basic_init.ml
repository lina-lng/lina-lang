(** Test: Array.init basic usage
    Expected: ACCEPT

    Verifies that Array.init has type int -> (int -> 'a) -> 'a array *)

let squares = Array.init 5 (fun i -> i * i)
let strings = Array.init 3 (fun i -> if i = 0 then "a" else "b")

let () = print_int (Array.length squares)
let () = print_int (Array.length strings)
