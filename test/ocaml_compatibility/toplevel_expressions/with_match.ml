(* Top-level let-in expression: with match *)
(* Expected: ACCEPT - match expressions work in let-in *)

let _ =
  let opt = Some 42 in
  let result = match opt with
    | None -> 0
    | Some x -> x
  in
  result
