(* Result: monadic bind function *)
(* Expected: ACCEPT - bind chains result computations *)

let bind r f = match r with
  | Ok x -> f x
  | Error e -> Error e

let safe_div a b =
  if b = 0 then Error "division by zero"
  else Ok (a / b)

let computation =
  bind (safe_div 100 10) (fun x ->
    bind (safe_div x 2) (fun y ->
      Ok y))

let get r = match r with
  | Ok x -> x
  | Error _ -> 0

let () = print_int (get computation)
