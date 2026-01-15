(* Optional arguments with default values *)

let greet ?(greeting="Hello") name =
  greeting ^ " " ^ name

let msg1 = greet "World"
let msg2 = greet ~greeting:"Hi" "Friend"

let () = print_string msg1; print_newline ()
let () = print_string msg2; print_newline ()
