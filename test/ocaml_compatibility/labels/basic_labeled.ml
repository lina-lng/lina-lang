(* Basic labeled arguments *)
let add ~x ~y = x + y
let result1 = add ~x:1 ~y:2
let result2 = add ~y:10 ~x:5
let () = print_int result1; print_newline ()
let () = print_int result2; print_newline ()
