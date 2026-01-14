(* GADTs: basic syntax and construction *)
(* Expected: ACCEPT - basic GADT definition and usage *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Unit : unit expr

let int_expr = Int 42
let bool_expr = Bool true
let unit_expr = Unit

let () = print_int 0
