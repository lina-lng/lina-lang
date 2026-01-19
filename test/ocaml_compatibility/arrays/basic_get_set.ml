(** Test: Array get and set operations
    Expected: ACCEPT

    Verifies array access operations have correct types. *)

let arr = Array.make 3 0
let () = Array.set arr 0 42
let x = Array.get arr 0
let () = print_int x
