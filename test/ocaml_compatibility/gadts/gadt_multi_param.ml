(* GADT with multiple type parameters - equality witness *)

type ('a, 'b) eq = Refl : ('a, 'a) eq

let cast : type a b. (a, b) eq -> a -> b = fun eq x ->
  match eq with
  | Refl -> x

let int_id = cast Refl 42
let () = print_int int_id; print_newline ()
