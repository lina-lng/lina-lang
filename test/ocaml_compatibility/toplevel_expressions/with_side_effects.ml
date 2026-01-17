(* Top-level let-in expression: with side effects *)
(* Expected: ACCEPT - side effects are executed *)

let () =
  let _ = print_int 1 in
  let _ = print_int 2 in
  print_int 3
