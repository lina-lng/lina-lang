(* Relaxed value restriction: bivariant (unused) type vars generalized *)
(* Expected: ACCEPT - unused type parameter is bivariant *)

(* Phantom type - 'a is never used in the representation *)
type 'a phantom = Phantom of int

let make_phantom () = Phantom 42

(* make_phantom () is NOT a value *)
(* But 'a is bivariant (unused), so it can be generalized *)
let p = make_phantom ()

(* Should be polymorphic - 'a is phantom *)
let p1 : int phantom = p
let p2 : bool phantom = p
let p3 : string phantom = p

let () = print_int 0
