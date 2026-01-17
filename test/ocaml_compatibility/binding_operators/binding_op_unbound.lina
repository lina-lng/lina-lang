(* Binding operators: undefined operator *)
(* Expected: REJECT - ( let* ) not defined *)

type 'a option = None | Some of 'a

let result =
  let* x = Some 10 in
  Some x
