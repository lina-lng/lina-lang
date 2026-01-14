(* Modules: basic module definition *)
(* Expected: ACCEPT - simple module with type and value *)

module Counter = struct
  type t = int
  let zero = 0
  let succ n = n + 1
end

let x = Counter.zero
let y = Counter.succ x

let () = print_int y
