(* Top-level let-in expression: basic usage *)
(* Expected: ACCEPT - let-in at top level evaluates and returns result *)

let _ = let x = 1 in let y = 2 in x + y
