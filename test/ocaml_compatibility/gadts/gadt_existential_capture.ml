(* GADT capturing existential type with function *)

type showable = Show : 'a * ('a -> string) -> showable

let show (Show (x, f)) = f x

let int_show = Show (42, fun n -> "int")
let str_show = Show ("hello", fun s -> s)

let () = print_string (show int_show); print_newline ()
let () = print_string (show str_show); print_newline ()
