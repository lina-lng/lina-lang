(* REJECT: Existential type from first-class module escapes *)

module type S = sig
  type t
  val value : t
end

let bad () =
  let module M = (val (module struct type t = int let value = 42 end : S) : S) in
  M.value  (* M.t is existential and should not escape *)
