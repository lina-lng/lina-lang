(* Modules: basic operator access with Module.( op ) syntax *)
(* Expected: ACCEPT - access operator from module *)

module Op = struct
  let ( |> ) x f = f x
end

let double x = x * 2

let result = Op.( |> ) 5 double

let _ = print_int result
