(* Label punning: ~x means ~x:x *)

let make_point ~x ~y = (x, y)

let x = 10
let y = 20

let point = make_point ~x ~y

let (a, b) = point
let () = print_int a; print_newline ()
let () = print_int b; print_newline ()
