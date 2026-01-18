(* Result: Ok is covariant - relaxed value restriction *)
(* Expected: ACCEPT - Ok applied to id should be polymorphic *)

let id x = x
let ok_id = Ok (id 42)

(* The 'a in Ok is covariant, so this should work *)
let get r = match r with
  | Ok x -> x
  | Error _ -> 0

let () = print_int (get ok_id)
