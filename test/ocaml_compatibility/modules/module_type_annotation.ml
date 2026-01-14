(* Modules: module with signature annotation *)
(* Expected: ACCEPT - module constrained by signature *)

module type COUNTER = sig
  type t
  val zero : t
  val succ : t -> t
end

module Counter : COUNTER = struct
  type t = int
  let zero = 0
  let succ n = n + 1
end

(* Counter.t is abstract - cannot use as int directly *)
let x = Counter.zero
let y = Counter.succ x

let () = print_int 0
