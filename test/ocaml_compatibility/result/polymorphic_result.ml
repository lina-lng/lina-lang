(* Result: polymorphic result functions *)
(* Expected: ACCEPT - result functions work with any types *)

let map f r = match r with
  | Ok x -> Ok (f x)
  | Error e -> Error e

let int_result = map (fun x -> x + 1) (Ok 10)
let str_result = map (fun s -> s ^ "!") (Ok "hello")

let get_or r default = match r with
  | Ok x -> x
  | Error _ -> default

let () = print_int (get_or int_result 0)
