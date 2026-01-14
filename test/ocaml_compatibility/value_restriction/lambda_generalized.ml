(* Value restriction: lambda is a syntactic value, should be generalized *)
(* Expected: ACCEPT - id should be polymorphic 'a -> 'a *)

let id = fun x -> x

(* Should work at multiple types - proves polymorphism *)
let int_result = id 42
let bool_result = id true
let string_result = id "hello"

let () = print_int int_result
