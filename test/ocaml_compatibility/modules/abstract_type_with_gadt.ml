(* Abstract type used in GADT pattern matching *)
(* Simplified to avoid OCaml stdlib functions like compare and string_of_int *)

module type KEY = sig
  type t
  val default : t
end

module IntKey : KEY with type t = int = struct
  type t = int
  let default = 0
end

type _ key_type =
  | IntKey : int key_type
  | BoolKey : bool key_type

let key_to_int : type a. a key_type -> a -> int = fun kt k ->
  match kt with
  | IntKey -> k
  | BoolKey -> if k then 1 else 0

let () = print_int (key_to_int IntKey 42); print_newline ()
let () = print_int (key_to_int BoolKey true); print_newline ()
