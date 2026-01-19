(* Pattern matching with list literals *)
(* Expected: ACCEPT - pattern match on specific list structure *)

let classify lst =
  match lst with
  | [] -> "empty"
  | [_] -> "singleton"
  | [_; _] -> "pair"
  | _ -> "many"

let () = print_string (classify [])
let () = print_string (classify [1])
let () = print_string (classify [1; 2])
let () = print_string (classify [1; 2; 3])
