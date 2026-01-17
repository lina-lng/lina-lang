(* Top-level let-in expression: nested function definitions *)
(* Expected: ACCEPT - functions can be defined and used in let-in *)

let _ =
  let add = fun x -> fun y -> x + y in
  let double = fun n -> add n n in
  double 21
