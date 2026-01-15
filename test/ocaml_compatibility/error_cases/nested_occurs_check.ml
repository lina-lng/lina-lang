(* REJECT: Nested occurs check failure *)

let bad f =
  let x = f in
  let y = (x, x x) in
  y
