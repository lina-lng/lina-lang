(* Top-level let-in expression: with tuple patterns *)
(* Expected: ACCEPT - tuple patterns work in let-in *)

let _ =
  let pair = (1, 2) in
  let (a, b) = pair in
  a + b
