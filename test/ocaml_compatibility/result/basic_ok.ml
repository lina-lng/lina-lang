(* Result: basic Ok constructor *)
(* Expected: ACCEPT - Ok creates a result value *)

let x = Ok 42
let y = Ok "hello"

let get_ok r = match r with
  | Ok v -> v
  | Error _ -> 0

let () = print_int (get_ok x)
