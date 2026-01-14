(* Variance: explicit variance annotation must match inferred *)
(* Expected: ACCEPT - covariant annotation matches inferred variance *)

(* Wrapper that is covariant - explicit annotation *)
type +'a wrapper = Wrap of 'a list

let id x = x

(* With explicit covariant annotation, relaxed VR should work *)
let w = id (Wrap [])

let _ = (w : int wrapper)
let _ = (w : bool wrapper)

let () = print_int 0
