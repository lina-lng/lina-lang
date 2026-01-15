(* REJECT: Variable bound twice in pattern - violates linearity *)

let bad_linear x =
  match x with
  | (a, a) -> a  (* a is bound twice *)
