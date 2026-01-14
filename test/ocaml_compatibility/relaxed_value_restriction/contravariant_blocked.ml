(* Relaxed value restriction: contravariant type vars NOT generalized *)
(* Expected: REJECT - contravariant position blocks generalization *)

type 'a consumer = Consumer of ('a -> unit)

let make_consumer () = Consumer (fun _ -> ())

(* make_consumer () is NOT a value *)
(* 'a consumer has 'a in contravariant position (inside function arg) *)
(* Relaxed VR does NOT allow generalization of contravariant vars *)
let consumer = make_consumer ()

(* Should NOT be polymorphic *)
let Consumer f1 = (consumer : int consumer)
let Consumer f2 = (consumer : bool consumer)

let () = f1 42
