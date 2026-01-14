(* GADTs: existential type variable must not escape *)
(* Expected: REJECT - existential 'a cannot escape its scope *)

type any_expr = Any : 'a -> any_expr

(* Trying to return the existentially bound value *)
(* The type 'a is bound inside Any and cannot escape *)
let unwrap (Any x) = x
