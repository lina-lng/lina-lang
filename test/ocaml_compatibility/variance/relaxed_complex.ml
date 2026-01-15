(* Complex relaxed value restriction with partial application *)

let pair x y = (x, y)
let pair_with_10 = pair 10

(* pair_with_10 should be polymorphic due to relaxed value restriction *)
let int_pair = pair_with_10 20
let str_pair = pair_with_10 "hello"

let (a, b) = int_pair
let (c, d) = str_pair

let () = print_int a; print_newline ()
let () = print_int b; print_newline ()
let () = print_int c; print_newline ()
let () = print_string d; print_newline ()
