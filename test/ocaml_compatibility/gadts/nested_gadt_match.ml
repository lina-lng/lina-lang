(* GADTs: nested pattern matching with type refinement *)
(* Expected: ACCEPT - nested patterns preserve type refinement *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
  | Pair (x, y) -> (eval x, eval y)

(* Nested matching *)
let get_first_int (e : (int * bool) expr) : int =
  match e with
  | Pair (Int n, Bool _) -> n

let pair_expr = Pair (Int 42, Bool true)
let result = get_first_int pair_expr

let () = print_int result
