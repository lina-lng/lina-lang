(* Or-pattern contributes to exhaustiveness checking *)

type abc = A | B | C

(* This should be exhaustive: A|B covers A and B, C covers C *)
let to_int x =
  match x with
  | A | B -> 1
  | C -> 2

let () = print_int (to_int A); print_newline ()
let () = print_int (to_int B); print_newline ()
let () = print_int (to_int C); print_newline ()
