(* Nested alias pattern *)

let swap_and_sum pair =
  match pair with
  | ((a, b) as original) ->
    let swapped = (b, a) in
    let (x, y) = original in
    x + y

let () = print_int (swap_and_sum (3, 7)); print_newline ()
