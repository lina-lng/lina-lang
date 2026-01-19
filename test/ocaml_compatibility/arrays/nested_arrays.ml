(** Test: Nested arrays
    Expected: ACCEPT

    Verifies arrays can contain arrays. *)

let matrix = Array.init 2 (fun i -> Array.init 3 (fun j -> i + j))
let row0 = Array.get matrix 0
let elem = Array.get row0 2

let () = print_int elem
let () = print_int (Array.length matrix)
let () = print_int (Array.length row0)
