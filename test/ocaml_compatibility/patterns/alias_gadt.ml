(* Alias pattern in GADT match preserves refined type *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | (Int n) as int_expr -> n
  | (Bool b) as bool_expr -> b
  | Add (left, right) -> eval left + eval right

let () = print_int (eval (Int 42)); print_newline ()
let () = print_string (string_of_bool (eval (Bool true))); print_newline ()
let () = print_int (eval (Add (Int 1, Int 2))); print_newline ()
