(* Multiple extension constructors added to extensible type *)

type event = ..

type event += Click of int * int
type event += KeyPress of string
type event += Scroll of int

let describe_event e =
  match e with
  | Click (x, y) -> "click"
  | KeyPress k -> "key"
  | Scroll n -> "scroll"
  | _ -> "unknown"

let () = print_string (describe_event (Click (10, 20))); print_newline ()
let () = print_string (describe_event (KeyPress "a")); print_newline ()
let () = print_string (describe_event (Scroll 5)); print_newline ()
