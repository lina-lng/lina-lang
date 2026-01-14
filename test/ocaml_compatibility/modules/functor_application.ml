(* Modules: functor application *)
(* Expected: ACCEPT - applying functor to module *)

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeSet (Ord : ORDERED) = struct
  type elt = Ord.t
  type t = elt list
  let empty = []
  let add x s = x :: s
  let mem x s =
    let rec loop s = match s with
      | [] -> false
      | h :: t -> if Ord.compare x h = 0 then true else loop t
    in loop s
end

module IntOrd = struct
  type t = int
  let compare a b = a - b
end

module IntSet = MakeSet (IntOrd)

let s = IntSet.add 1 (IntSet.add 2 IntSet.empty)
let found = IntSet.mem 1 s

let () = print_int 0
