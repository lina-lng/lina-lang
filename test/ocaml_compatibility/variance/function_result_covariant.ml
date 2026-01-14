(* Variance: function result position is covariant *)
(* Expected: ACCEPT - 'a in (unit -> 'a) is covariant *)

type 'a producer = MkProducer of (unit -> 'a)

let id x = x

(* id (MkProducer (fun () -> [])) is NOT a value *)
(* 'a producer has 'a in covariant position (function result) *)
(* Covariant vars can be generalized with relaxed VR *)
let p = id (MkProducer (fun () -> []))

(* Should work - covariant position allows generalization *)
let _ = (p : int list producer)
let _ = (p : bool list producer)

let () = print_int 0
