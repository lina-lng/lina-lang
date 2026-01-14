(* Value restriction: partial application is a syntactic value *)
(* Expected: ACCEPT - partial application produces a lambda (value) *)

let id x = x

(* Partial application: (List.map id) is a syntactic value (a lambda) *)
let map_id = List.map id

(* Should be polymorphic - usable at multiple types *)
let int_list = map_id [1; 2; 3]
let bool_list = map_id [true; false]

let () = print_int 0
