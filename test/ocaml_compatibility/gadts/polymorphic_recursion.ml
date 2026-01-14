(* GADTs: polymorphic recursion with type annotation *)
(* Expected: ACCEPT - 'type a.' annotation enables polymorphic recursion *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * int expr * int expr -> int expr

(* Polymorphic recursion: eval is called at different type instantiations *)
(* eval (Bool ...) : bool, eval (Int ...) : int within same recursive call *)
let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
  | Add (x, y) -> eval x + eval y
  | If (cond, then_e, else_e) ->
    if eval cond then eval then_e else eval else_e

let result = eval (If (Bool true, Add (Int 10, Int 32), Int 0))
let () = print_int result
