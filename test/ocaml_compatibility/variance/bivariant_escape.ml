(* REJECT: Bivariant (phantom) type should not allow unsafe casts *)

type 'a phantom = unit

let coerce : 'a phantom -> 'b phantom = fun x -> x

(* This would be unsound if phantom was truly bivariant in unsafe ways *)
type 'a wrapper = { data : 'a }

let int_wrapper : int wrapper = { data = 42 }
let str_wrapper : string wrapper = { data = "hello" }

(* The following should work for phantom types but not for real types *)
let p1 : int phantom = ()
let p2 : string phantom = coerce p1
