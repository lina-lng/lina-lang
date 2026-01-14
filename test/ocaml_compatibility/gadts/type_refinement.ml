(* GADTs: type refinement in pattern matching *)
(* Expected: ACCEPT - matching refines type variable *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

(* In the Int branch, we know the result type is int *)
(* In the Bool branch, we know the result type is bool *)
let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n    (* n : int, return type refined to int *)
  | Bool b -> b   (* b : bool, return type refined to bool *)

let int_result = eval (Int 42)
let bool_result = eval (Bool true)

let () = print_int int_result
