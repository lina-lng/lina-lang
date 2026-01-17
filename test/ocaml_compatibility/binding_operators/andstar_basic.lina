(* Binding operators: basic and* *)
(* Expected: ACCEPT - and* with let* *)

type 'a option = None | Some of 'a

let ( let* ) opt f = match opt with
  | None -> None
  | Some x -> f x

let ( and* ) a b = match a with
  | None -> None
  | Some x -> match b with
    | None -> None
    | Some y -> Some (x, y)

let result =
  let* x = Some 10
  and* y = Some 20 in
  Some (x + y)

let _ = match result with
  | None -> print_endline "none"
  | Some n -> print_int n
