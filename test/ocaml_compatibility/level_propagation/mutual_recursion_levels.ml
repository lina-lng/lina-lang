(* Level propagation: mutual recursion with correct levels *)
(* Expected: ACCEPT - mutually recursive functions generalize together *)

let rec even n =
  if n = 0 then true
  else odd (n - 1)
and odd n =
  if n = 0 then false
  else even (n - 1)

let result1 = even 10
let result2 = odd 7

let () = print_int (if result1 then 1 else 0)
