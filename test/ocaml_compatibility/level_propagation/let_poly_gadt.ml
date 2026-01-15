(* Let-polymorphism interacting with GADT *)
(* Uses user-defined list instead of built-in list *)

type 'a list = Nil | Cons of ('a * 'a list)

type _ ty =
  | TInt : int ty
  | TList : 'a ty -> 'a list ty

let rec default : type a. a ty -> a = function
  | TInt -> 0
  | TList t -> Nil

(* Let-polymorphism should allow default to be used at different types *)
let int_default = default TInt
let list_default = default (TList TInt)
let nested_default = default (TList (TList TInt))

let () = print_int int_default; print_newline ()
