(* Nested functor application - F(G(M)).t type equality *)

module type T = sig type t end

module Id (X : T) : T with type t = X.t = struct
  type t = X.t
end

module Wrap (X : T) : T with type t = X.t = struct
  type t = X.t
end

module Base = struct type t = int end

module M1 = Id (Wrap (Base))
module M2 = Wrap (Id (Base))

(* Both should have type t = int *)
let x : M1.t = 42
let y : M2.t = 42

let () = print_int x; print_newline ()
let () = print_int y; print_newline ()
