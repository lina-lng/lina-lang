(* Tuple: equal operation *)
(* Expected: ACCEPT - equal compares pairs *)

let equal eq_fst eq_snd (a1, b1) (a2, b2) = eq_fst a1 a2 && eq_snd b1 b2
let int_eq a b = a = b
let p1 = (1, 2)
let p2 = (1, 2)
let eq = equal int_eq int_eq p1 p2
let () = print_int (if eq then 1 else 0)
