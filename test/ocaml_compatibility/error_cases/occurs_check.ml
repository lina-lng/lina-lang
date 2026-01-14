(* Error: occurs check - infinite type *)
(* Expected: REJECT - both compilers detect infinite type *)

let rec f x = f
