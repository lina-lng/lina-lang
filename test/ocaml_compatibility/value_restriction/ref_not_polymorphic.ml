(* Value restriction: ref is invariant, NEVER polymorphic *)
(* Expected: REJECT - ref is invariant, so type variable cannot be generalized *)

(* ref [] is an application (not a value) AND ref is invariant *)
(* Even with relaxed value restriction, this cannot be polymorphic *)
let r = ref []

(* First use fixes the type *)
let () = r := [1]

(* Second use at different type MUST fail *)
let () = r := [true]
