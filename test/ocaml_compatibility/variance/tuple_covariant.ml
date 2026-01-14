(* Variance: tuple is covariant in all parameters *)
(* Expected: ACCEPT - ('a * 'b) is covariant in both 'a and 'b *)

let id x = x

(* id ([], []) is NOT a value *)
(* Tuple is covariant in both positions *)
(* So relaxed VR allows generalization *)
let pair = id ([], [])

(* Should be polymorphic in both components *)
let _ = (pair : (int list * bool list))
let _ = (pair : (string list * float list))

let () = print_int 0
