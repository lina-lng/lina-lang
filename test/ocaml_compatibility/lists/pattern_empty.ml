(* Pattern matching: empty list *)
(* Expected: ACCEPT - match on [] should work *)

let is_empty lst =
  match lst with
  | [] -> true
  | _ -> false

let () = print_string (if is_empty [] then "yes" else "no")
