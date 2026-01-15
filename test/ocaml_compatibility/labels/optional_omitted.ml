(* Optional argument can be omitted *)

let describe ?prefix value =
  match prefix with
  | Some p -> p ^ ": " ^ value
  | None -> value

let msg1 = describe "test"
let msg2 = describe ~prefix:"Label" "test"

let () = print_string msg1; print_newline ()
let () = print_string msg2; print_newline ()
