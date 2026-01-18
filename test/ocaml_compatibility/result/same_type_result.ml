(* Result: same type for Ok and Error *)
(* Expected: ACCEPT - ('a, 'a) result is valid *)

let identity_or_error x =
  if x > 0 then Ok x
  else Error x

let a = identity_or_error 42
let b = identity_or_error (-5)

let extract r = match r with
  | Ok x -> x
  | Error e -> e

let () = print_int (extract a)
