(* Result: Error is covariant - relaxed value restriction *)
(* Expected: ACCEPT - Error type parameter is covariant *)

let id x = x
let err = Error (id "failed")

let is_error r = match r with
  | Ok _ -> false
  | Error _ -> true

let () = print_string (string_of_bool (is_error err))
