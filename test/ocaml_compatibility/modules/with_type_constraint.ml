(* Modules: with type constraint *)
(* Expected: ACCEPT - signature with type constraint *)

module type CONTAINER = sig
  type t
  type elem
  val empty : t
  val add : elem -> t -> t
end

module type INT_CONTAINER = CONTAINER with type elem = int

module IntList : INT_CONTAINER = struct
  type elem = int
  type t = elem list
  let empty = []
  let add x xs = x :: xs
end

let c = IntList.add 42 IntList.empty
let () = print_int 0
