(* Result: result in recursive function *)
(* Expected: ACCEPT - recursive functions can return result *)

let rec safe_factorial n =
  if n < 0 then Error "negative input"
  else if n = 0 then Ok 1
  else match safe_factorial (n - 1) with
    | Error e -> Error e
    | Ok r -> Ok (n * r)

let test = safe_factorial 5

let get r = match r with
  | Ok x -> x
  | Error _ -> 0

let () = print_int (get test)
