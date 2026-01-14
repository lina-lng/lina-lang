(* Level propagation: unification updates levels correctly *)
(* Expected: ACCEPT - type vars unified at correct level *)

let f x =
  let g y = (x, y) in
  (* g's type involves x's type, levels must be correct *)
  let pair1 = g 1 in
  let pair2 = g true in
  pair1

(* f should be polymorphic *)
let r1 = f "hello"
let r2 = f 42

let () = print_int 0
