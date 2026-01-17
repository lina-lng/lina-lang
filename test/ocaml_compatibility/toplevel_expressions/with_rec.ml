(* Top-level let-in expression: with recursive function *)
(* Expected: ACCEPT - recursive let works in let-in *)

let _ =
  let rec fact n =
    if n <= 1 then 1
    else n * fact (n - 1)
  in
  fact 5
