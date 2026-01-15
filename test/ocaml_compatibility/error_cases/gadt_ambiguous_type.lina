(* REJECT: Ambiguous GADT return type *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

(* Return type is ambiguous - no annotation to resolve it *)
let ambiguous e =
  match e with
  | Int n -> n
  | Bool b -> b
