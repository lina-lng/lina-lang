(* Binding operators: complex pattern *)
(* Expected: ACCEPT - tuple pattern in let* *)

type 'a option = None | Some of 'a

let ( let* ) opt f = match opt with
  | None -> None
  | Some x -> f x

let result =
  let* (x, y) = Some (10, 20) in
  Some (x + y)

let _ = match result with
  | None -> print_endline "none"
  | Some n -> print_int n
