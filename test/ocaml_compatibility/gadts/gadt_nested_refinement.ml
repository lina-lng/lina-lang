(* Nested pattern match with multiple type refinements *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Add : int expr * int expr -> int expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
  | If (cond, then_e, else_e) ->
    (match eval cond with
     | true -> eval then_e
     | false -> eval else_e)
  | Add (left, right) -> eval left + eval right

let test = If (Bool true, Int 1, Int 2)
let () = print_int (eval test); print_newline ()

let test2 = Add (Int 10, If (Bool false, Int 5, Int 20))
let () = print_int (eval test2); print_newline ()
