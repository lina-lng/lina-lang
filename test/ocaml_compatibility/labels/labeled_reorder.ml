(* Labeled arguments can be reordered *)

let make_pair ~first ~second = (first, second)

let pair1 = make_pair ~first:1 ~second:2
let pair2 = make_pair ~second:20 ~first:10

let (a, b) = pair1
let (c, d) = pair2

let () = print_int a; print_newline ()
let () = print_int b; print_newline ()
let () = print_int c; print_newline ()
let () = print_int d; print_newline ()
