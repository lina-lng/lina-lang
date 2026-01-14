(* Value restriction: constructor applied to value is a syntactic value *)
(* Expected: ACCEPT - Some (fun x -> x) is a value, should be polymorphic *)

let id = fun x -> x

(* Constructor applied to a lambda (value) is itself a value *)
let some_id = Some id

(* Should be polymorphic option *)
let f = match some_id with None -> (fun x -> x) | Some f -> f
let int_result = f 42
let bool_result = f true

let () = print_int int_result
