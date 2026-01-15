(* First-class module with abstract type *)
(* Uses simple counter instead of list to avoid built-in list dependency *)

module type CONTAINER = sig
  type t
  val empty : t
  val add : int -> t -> t
  val sum : t -> int
end

module SumContainer : CONTAINER = struct
  type t = int
  let empty = 0
  let add x total = x + total
  let sum total = total
end

let container = (module SumContainer : CONTAINER)

let test () =
  let module C = (val container : CONTAINER) in
  let c = C.add 1 (C.add 2 (C.add 3 C.empty)) in
  C.sum c

let () = print_int (test ()); print_newline ()
