(* Basic extensible variant test *)
type ext = ..

type ext += A
type ext += B of int
type ext += C of string * int

let f x =
  match x with
  | A -> 1
  | B n -> n
  | C (_, n) -> n
  | _ -> 0

let () = print_int (f A); print_newline ()
let () = print_int (f (B 42)); print_newline ()
let () = print_int (f (C ("hello", 100))); print_newline ()
