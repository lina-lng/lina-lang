(* Result: type mismatch between Ok and Error branches *)
(* Expected: REJECT - cannot unify int with string *)

let bad r = match r with
  | Ok x -> x + 1
  | Error e -> e ^ "!"
