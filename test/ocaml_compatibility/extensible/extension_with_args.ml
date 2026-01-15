(* Extension constructor with complex payload *)

type expr = ..

type expr += Int of int
type expr += Add of expr * expr
type expr += Mul of expr * expr

let rec eval e =
  match e with
  | Int n -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b
  | _ -> 0

let test = Add (Int 2, Mul (Int 3, Int 4))
let () = print_int (eval test); print_newline ()
