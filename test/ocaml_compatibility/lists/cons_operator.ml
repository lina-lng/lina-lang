(* Cons operator: :: prepends element to list *)
(* Expected: ACCEPT - x :: xs should work *)

let lst1 = 1 :: []
let lst2 = 1 :: 2 :: 3 :: []
let lst3 = "a" :: "b" :: []

let prepend x xs = x :: xs
let lst4 = prepend 0 [1; 2; 3]

let () = print_string "ok"
