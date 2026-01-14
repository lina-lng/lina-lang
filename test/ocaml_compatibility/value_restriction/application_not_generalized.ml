(* Value restriction: function application is NOT a syntactic value *)
(* Expected: REJECT - r should have weak/mono type, not polymorphic *)
(* NOTE: With relaxed value restriction, list is covariant so this might
   actually be polymorphic. This tests that both compilers behave the same. *)

let id x = x

(* Application (id []) is NOT a syntactic value *)
(* With strict value restriction, this gets weak type '_a list *)
let r = id []

(* Trying to use at different types should fail if not generalized *)
let _ = match r with [] -> 0 | x :: _ -> x
let _ = match r with [] -> true | x :: _ -> x
