(* Result: wrong argument type in result context *)
(* Expected: REJECT - expecting int result but got string *)

let f : (int, string) result -> int = fun r ->
  match r with
  | Ok x -> x
  | Error _ -> 0

let bad = f (Ok "not an int")
