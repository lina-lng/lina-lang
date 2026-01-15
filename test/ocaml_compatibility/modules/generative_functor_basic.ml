(* Test: Basic generative functor syntax and behavior *)
(* Expected: ACCEPT - basic generative functor should work *)

(* Generative functor with () parameter using functor keyword *)
module Make = functor () -> struct
  type t = int
  let make n = n
  let get x = x
end

module M1 = Make ()
module M2 = Make ()

let v1 = M1.make 10
let _ = print_int (M1.get v1)
