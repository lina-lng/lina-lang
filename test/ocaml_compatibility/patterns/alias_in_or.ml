(* Alias pattern combined with or-pattern *)

type wrapper = Left of int | Right of int

let extract_with_tag x =
  match x with
  | (Left n | Right n) as whole ->
    (n, whole)

let (val1, _) = extract_with_tag (Left 42)
let (val2, _) = extract_with_tag (Right 99)

let () = print_int val1; print_newline ()
let () = print_int val2; print_newline ()
