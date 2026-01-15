(* REJECT: Wrong type annotation for GADT function *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

(* Wrong annotation - claims to return int but Bool case returns bool *)
let rec eval : type a. a expr -> int = function
  | Int n -> n
  | Bool b -> b  (* Type error: expected int, got bool *)
