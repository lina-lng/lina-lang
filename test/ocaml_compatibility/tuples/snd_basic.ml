(* Tuple: snd accessor *)
(* Expected: ACCEPT - snd returns second element *)

let pair = (42, "hello")
let y = snd pair
let () = print_string y
