(* For loop: basic to usage *)
(* Expected: ACCEPT - for i = start to end *)

let sum = ref 0

let _ =
  for i = 1 to 5 do
    sum := !sum + i
  done

let _ = print_int !sum
