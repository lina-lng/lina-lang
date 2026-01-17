(* Binding operators: basic let* *)
(* Expected: ACCEPT - basic let* usage *)

type 'a option = None | Some of 'a

let ( let* ) opt f = match opt with
  | None -> None
  | Some x -> f x

let result =
  let* x = Some 10 in
  let* y = Some 20 in
  Some (x + y)

let _ = match result with
  | None -> print_endline "none"
  | Some n -> print_int n
