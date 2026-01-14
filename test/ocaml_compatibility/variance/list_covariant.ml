(* Variance: list is covariant in its type parameter *)
(* Expected: ACCEPT - list should be inferred as covariant *)

(* If list is covariant, we can use it in covariant positions *)
(* This test verifies the variance is correctly inferred *)

let id x = x

(* Non-value that produces 'a list - works with relaxed VR if covariant *)
let empty = id []

let int_empty : int list = empty
let bool_empty : bool list = empty

let () = print_int 0
