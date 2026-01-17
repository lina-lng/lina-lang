(* Binding operators: basic let+ *)
(* Expected: ACCEPT - basic let+ (map) usage *)

type 'a option = None | Some of 'a

let ( let+ ) opt f = match opt with
  | None -> None
  | Some x -> Some (f x)

let result =
  let+ x = Some 5 in
  x * 2

let _ = match result with
  | None -> print_endline "none"
  | Some n -> print_int n
