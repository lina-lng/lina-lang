(* Partial application with labeled arguments *)

let add3 ~x ~y ~z = x + y + z

let add_to_10 = add3 ~x:10
let add_10_and_20 = add_to_10 ~y:20

let result = add_10_and_20 ~z:30

let () = print_int result; print_newline ()
