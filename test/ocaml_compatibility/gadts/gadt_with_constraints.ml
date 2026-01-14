(* GADTs: with explicit type constraints *)
(* Expected: ACCEPT - GADT with type constraints *)

type (_, _) eq = Refl : ('a, 'a) eq

(* Use type equality witness *)
let cast : type a b. (a, b) eq -> a -> b = fun eq x ->
  match eq with
  | Refl -> x

let int_val : int = cast Refl 42
let () = print_int int_val
