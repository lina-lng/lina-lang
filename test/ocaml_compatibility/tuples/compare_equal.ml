(* Tuple: compare equal pairs *)
(* Expected: ACCEPT - compare returns 0 for equal pairs *)

let compare cmp_fst cmp_snd (a1, b1) (a2, b2) =
  let c = cmp_fst a1 a2 in
  if c <> 0 then c else cmp_snd b1 b2
let int_cmp a b = if a < b then -1 else if a > b then 1 else 0
let p1 = (1, 2)
let p2 = (1, 2)
let c = compare int_cmp int_cmp p1 p2
let () = print_int c
