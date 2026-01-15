(* REJECT: Same variable name used twice in pattern *)

let bad_pattern pair =
  match pair with
  | (x, x) -> x  (* x bound twice - should fail *)
