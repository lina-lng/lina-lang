(* Result: result with unit type *)
(* Expected: ACCEPT - (unit, 'e) result is valid for side effects *)

let validate x =
  if x > 0 then Ok ()
  else Error "must be positive"

let a = validate 42
let b = validate (-1)

let is_ok r = match r with
  | Ok _ -> true
  | Error _ -> false

let () = print_string (string_of_bool (is_ok a))
