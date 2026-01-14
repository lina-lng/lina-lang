(* Modules: strengthening preserves type equality *)
(* Expected: ACCEPT - module N = M preserves N.t = M.t *)

module M = struct
  type t = int
  let x = 42
end

(* N is an alias for M - strengthening ensures N.t = M.t *)
module N = M

(* These should type-check due to strengthening *)
let f (a : M.t) : N.t = a
let g (b : N.t) : M.t = b

let result = f M.x
let () = print_int result
