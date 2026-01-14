(* Variance: ref is invariant in its type parameter *)
(* Expected: REJECT - ref is invariant, cannot generalize *)

let id x = x

(* ref (id []) is NOT a value *)
(* ref is invariant, so relaxed VR doesn't help *)
let r = ref (id [])

(* Should fail - r is monomorphic after first use *)
let _ = (r : int list ref)
let _ = (r : bool list ref)
