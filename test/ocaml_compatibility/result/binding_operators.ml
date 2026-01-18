(* Result: binding operators let* and and* *)
(* Expected: ACCEPT - binding operators work with result *)

let ( let* ) r f = match r with
  | Ok x -> f x
  | Error e -> Error e

let ( and* ) r1 r2 = match r1, r2 with
  | Ok x, Ok y -> Ok (x, y)
  | Error e, _ -> Error e
  | _, Error e -> Error e

let computation =
  let* x = Ok 10 in
  let* y = Ok 20 in
  Ok (x + y)

let parallel =
  let* a = Ok 5
  and* b = Ok 7 in
  Ok (a * b)

let get r = match r with
  | Ok x -> x
  | Error _ -> 0

let () = print_int (get computation)
