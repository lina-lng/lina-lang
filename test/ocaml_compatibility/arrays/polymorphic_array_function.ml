(** Test: Polymorphic functions over arrays
    Expected: ACCEPT

    Functions that don't mutate arrays can be polymorphic. *)

let array_length arr = Array.length arr

let len_int = array_length (Array.make 3 0)
let len_str = array_length (Array.make 5 "x")

let () = print_int len_int
let () = print_int len_str
