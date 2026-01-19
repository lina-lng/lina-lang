(* Test: Polymorphic dict functions *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let singleton k v = [(k, v)]
  let size d = List.length d
end

(* A polymorphic function that works with any dict *)
let get_size d = Dict.size d

let d1 = Dict.singleton "a" 1
let d2 = Dict.singleton 1 "one"
let d3 = Dict.singleton true false

let () = print_int (get_size d1)
let () = print_int (get_size d2)
let () = print_int (get_size d3)
