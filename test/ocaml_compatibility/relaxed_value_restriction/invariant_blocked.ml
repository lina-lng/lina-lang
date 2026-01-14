(* Relaxed value restriction: invariant type vars NOT generalized *)
(* Expected: REJECT - ref is invariant, blocks generalization *)

let make_ref_list () = ref []

(* make_ref_list () is NOT a value *)
(* 'a list ref is invariant in 'a (ref is invariant) *)
(* Relaxed VR does NOT help with invariant types *)
let r = make_ref_list ()

(* Should NOT be polymorphic - first use fixes type *)
let () = r := [1]
let () = r := [true]
