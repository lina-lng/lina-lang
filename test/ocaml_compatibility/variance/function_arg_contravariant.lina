(* Variance: function argument position is contravariant *)
(* Expected: REJECT - 'a in ('a -> unit) is contravariant *)

type 'a consumer = MkConsumer of ('a -> unit)

let id x = x

(* id (MkConsumer (fun _ -> ())) is NOT a value *)
(* 'a consumer has 'a in contravariant position *)
(* Contravariant vars cannot be generalized with relaxed VR *)
let c = id (MkConsumer (fun _ -> ()))

(* Should fail - contravariant position *)
let _ = (c : int consumer)
let _ = (c : bool consumer)
