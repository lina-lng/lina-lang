(* Modules: with type constraint makes type equality visible *)
(* Expected: ACCEPT - constrained type is usable as concrete type *)

module type S = sig
  type t
  val x : t
end

module M : S with type t = int = struct
  type t = int
  let x = 42
end

(* Test 1: Use M.x where int is expected *)
let f = fun n -> n + 1
let a = f M.x

(* Test 2: Use M.t as a type annotation *)
let b : M.t = 100
let c : int = b

(* Test 3: Pass M.x to polymorphic function, result is still int *)
let identity = fun x -> x
let d = identity M.x + 1

let _ = print_int 0
