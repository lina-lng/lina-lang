(* Value restriction: tuple of values is a syntactic value *)
(* Expected: ACCEPT - tuple containing only values is itself a value *)

let id = fun x -> x
let const = fun x -> fun y -> x

(* A tuple of lambdas is a syntactic value *)
let pair = (id, const)

(* Both components should be polymorphic *)
let (f, g) = pair
let int_result = f 42
let bool_result = f true
let str_result = g "hello" 123

let () = print_int int_result
