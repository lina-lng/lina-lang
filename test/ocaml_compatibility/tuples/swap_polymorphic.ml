(* Tuple: swap with different types *)
(* Expected: ACCEPT - swap works on heterogeneous pairs *)

let swap (a, b) = (b, a)
let pair = (42, "hello")
let swapped = swap pair
let s = fst swapped
let () = print_string s
