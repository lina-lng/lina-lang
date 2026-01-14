(* Value restriction: sequence expression is NOT a syntactic value *)
(* Expected: REJECT - sequence (;) is not a value, 'a -> 'a is invariant *)

(* Sequence expression is NOT a syntactic value *)
let f =
  print_int 0;
  fun x -> x

(* 'a -> 'a is invariant (contravariant + covariant = invariant) *)
(* So relaxed VR doesn't help - this should not be polymorphic *)
let int_result = f 42
let bool_result = f true

let () = print_int int_result
