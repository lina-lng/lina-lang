(* REJECT: Extension of non-extensible type *)

type regular = A | B

(* Cannot extend a non-extensible type *)
type regular += C
