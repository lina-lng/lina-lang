(** Test: Array.iteri type inference
    Expected: ACCEPT

    Verifies Array.iteri has type (int -> 'a -> unit) -> 'a array -> unit *)

let arr = Array.make 3 "x"
let () = Array.iteri (fun i s -> print_int i) arr
