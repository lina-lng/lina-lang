(** Test: Array.exists and Array.for_all types
    Expected: ACCEPT

    Verifies both have type ('a -> bool) -> 'a array -> bool *)

let arr = Array.init 5 (fun i -> i)

let has_positive = Array.exists (fun x -> x > 0) arr
let all_non_negative = Array.for_all (fun x -> x >= 0) arr

let () = print_string (if has_positive then "true" else "false")
let () = print_string (if all_non_negative then "true" else "false")
