(* Mutually recursive GADTs with different type parameters *)

type _ even =
  | Zero : int even
  | SuccE : 'a odd -> 'a even

and _ odd =
  | One : int odd
  | SuccO : 'a even -> 'a odd

let rec even_val : type a. a even -> int = function
  | Zero -> 0
  | SuccE o -> 1 + odd_val o

and odd_val : type a. a odd -> int = function
  | One -> 1
  | SuccO e -> 1 + even_val e

let four = SuccE (SuccO (SuccE (SuccO Zero)))
let () = print_int (even_val four); print_newline ()
