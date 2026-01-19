(* Modules: use module operator in infix position *)
(* Expected: ACCEPT - use imported operator as infix *)

module Arith = struct
  let ( +. ) x y = x + y + 1
end

let ( +. ) = Arith.( +. )

let result = 10 +. 20

let _ = print_int result
