(* Value restriction: match expression is NOT a syntactic value *)
(* Expected: REJECT - match is not a value, 'a -> 'a is invariant in 'a *)

let cond = true

(* Match expression is NOT a syntactic value *)
let f = match cond with
  | true -> (fun x -> x)
  | false -> (fun x -> x)

(* 'a -> 'a is invariant (contravariant + covariant = invariant) *)
(* So relaxed VR doesn't help - this should not be polymorphic *)
let int_result = f 42
let bool_result = f true

let () = print_int int_result
