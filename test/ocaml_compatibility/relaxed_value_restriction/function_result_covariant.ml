(* Relaxed value restriction: function result type is covariant *)
(* Expected: ACCEPT - unit -> 'a list is covariant in 'a *)

let make_thunk () = fun () -> []

(* make_thunk () returns unit -> 'a list *)
(* The result type 'a list is in covariant position of the function *)
(* So 'a is covariant overall and can be generalized *)
let thunk = make_thunk ()

(* Should be polymorphic *)
let int_list = (thunk () : int list)
let bool_list = (thunk () : bool list)

let () = print_int 0
