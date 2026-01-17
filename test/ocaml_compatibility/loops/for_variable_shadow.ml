(* For loop: variable shadowing *)
(* Expected: ACCEPT - loop var shadows outer binding *)

let i = 999
let sum = ref 0

let _ =
  for i = 1 to 3 do
    sum := !sum + i
  done

let _ = print_int !sum
let _ = print_newline ()
let _ = print_int i
