(* Result: basic Error constructor *)
(* Expected: ACCEPT - Error creates a result value *)

let x = Error "something went wrong"
let y = Error 404

let is_error r = match r with
  | Ok _ -> false
  | Error _ -> true

let () = print_string (string_of_bool (is_error x))
