(* Assert: has type unit *)
(* Expected: ACCEPT - assert e has type unit *)

let check_positive x =
  assert (x > 0);
  x * 2

let result : int = check_positive 5
let _ = print_int result
