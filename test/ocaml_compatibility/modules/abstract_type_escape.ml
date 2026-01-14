(* Modules: abstract type cannot be used as concrete *)
(* Expected: REJECT - abstract type used incorrectly *)

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

(* Error: Counter.t is abstract, cannot use as int *)
let x : int = Counter.zero
