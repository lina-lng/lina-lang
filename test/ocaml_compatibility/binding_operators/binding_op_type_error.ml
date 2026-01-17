(* Binding operators: type mismatch *)
(* Expected: REJECT - operator signature mismatch *)

type 'a option = None | Some of 'a

let ( let* ) x f = x + f x

let result =
  let* x = Some 10 in
  Some x
