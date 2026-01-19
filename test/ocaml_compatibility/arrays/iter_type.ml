(** Test: Array.iter type inference
    Expected: ACCEPT

    Verifies Array.iter has type ('a -> unit) -> 'a array -> unit *)

let arr = Array.init 3 (fun i -> i * 10)
let () = Array.iter (fun x -> print_int x) arr
