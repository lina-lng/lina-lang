(* List type annotations: explicit type constraints *)
(* Expected: ACCEPT - type annotations on lists should work *)

let explicit_int : int list = [1; 2; 3]
let explicit_string : string list = ["a"; "b"]

let f (x : int list) : int list = x
let g : int list -> int list = fun x -> x

let annotated_empty : bool list = []

let () = print_string "ok"
