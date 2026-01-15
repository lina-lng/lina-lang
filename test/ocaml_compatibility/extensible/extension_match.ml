(* Pattern matching on extension constructors with wildcard *)

type msg = ..
type msg += Hello | Goodbye of string

let handle m =
  match m with
  | Hello -> "hi"
  | Goodbye name -> "bye " ^ name
  | _ -> "?"

let () = print_string (handle Hello); print_newline ()
let () = print_string (handle (Goodbye "friend")); print_newline ()
