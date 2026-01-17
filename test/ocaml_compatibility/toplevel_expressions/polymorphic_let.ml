(* Top-level let-in expression: polymorphic let *)
(* Expected: ACCEPT - let-in preserves polymorphism *)

let _ =
  let id = fun x -> x in
  let a = id 42 in
  let _b = id true in
  a
