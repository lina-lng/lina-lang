(* Modules: signature mismatch error *)
(* Expected: REJECT - module doesn't satisfy signature *)

module type SHOW = sig
  type t
  val show : t -> string
  val parse : string -> t
end

(* Missing 'parse' function - should fail *)
module BadShow : SHOW = struct
  type t = int
  let show n = "int"
end
