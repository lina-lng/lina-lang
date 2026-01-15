(* Mix of labeled and optional arguments *)

let format ~prefix ?suffix base =
  let s = prefix ^ base in
  match suffix with
  | Some suf -> s ^ suf
  | None -> s

let msg1 = format ~prefix:"[" "content"
let msg2 = format ~prefix:"(" ~suffix:")" "content"

let () = print_string msg1; print_newline ()
let () = print_string msg2; print_newline ()
