(* Locally abstract types: multiple type parameters *)
(* Expected: ACCEPT - multiple locally abstract types *)

let const (type a) (type b) (x : a) (y : b) : a = x

let result = const 42 "ignored"

let () = print_int result
