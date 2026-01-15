(* REJECT: Different arities in or-pattern *)

type t = A of int | B of int * int

let bad x =
  match x with
  | A n | B (n, _) -> n  (* Different structure *)
