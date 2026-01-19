(* Tuple: swap operation *)
(* Expected: ACCEPT - swap exchanges elements *)

let swap (a, b) = (b, a)
let pair = (1, 2)
let swapped = swap pair
let x = fst swapped
let () = print_int x
