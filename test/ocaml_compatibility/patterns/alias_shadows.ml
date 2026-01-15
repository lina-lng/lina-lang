(* Alias pattern shadows outer binding *)

let x = 100

let shadow_test opt =
  match opt with
  | Some (y as x) -> x + y  (* x shadows the outer x=100 *)
  | None -> 0

let () = print_int (shadow_test (Some 5)); print_newline ()
let () = print_int (shadow_test None); print_newline ()
