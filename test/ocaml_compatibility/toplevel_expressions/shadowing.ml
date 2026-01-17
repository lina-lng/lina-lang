(* Top-level let-in expression: shadowing *)
(* Expected: ACCEPT - inner x shadows outer x *)

let _ =
  let x = 1 in
  let x = x + 10 in
  let x = x * 2 in
  x
