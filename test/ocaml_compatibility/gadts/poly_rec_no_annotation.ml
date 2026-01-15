(* REJECT: Polymorphic recursion without annotation *)

type _ expr =
  | Int : int -> int expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

(* This should fail - no explicit type annotation for polymorphic recursion *)
let rec eval e =
  match e with
  | Int n -> n
  | Pair (a, b) -> (eval a, eval b)
