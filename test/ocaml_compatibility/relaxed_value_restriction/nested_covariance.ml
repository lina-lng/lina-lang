(* Relaxed value restriction: nested covariant types generalized *)
(* Expected: ACCEPT - option of list is covariant, generalized *)

let id x = x

(* id (Some []) is NOT a value *)
(* But 'a option and 'a list are both covariant *)
(* So 'a in 'a list option is covariant and can be generalized *)
let maybe_empty = id (Some [])

(* Should be polymorphic *)
let opt1 : int list option = maybe_empty
let opt2 : bool list option = maybe_empty

let () = print_int 0
