(* REJECT: Variables in or-pattern with incompatible types *)

type t = A of int | B of string

(* This should fail - x has different types in each branch *)
let f v =
  match v with
  | A x | B x -> x
