(* Modules: nested module operator access *)
(* Expected: ACCEPT - access operator from nested module *)

module Outer = struct
  module Inner = struct
    let ( @@ ) f x = f x
  end
end

let succ n = n + 1

let result = Outer.Inner.( @@ ) succ 41

let _ = print_int result
