(* Test: Nested dict types *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let singleton k v = [(k, v)]
  let of_list l = l
  let size d = List.length d
end

(* Dict of dicts *)
let inner1 = Dict.singleton "x" 1
let inner2 = Dict.singleton "y" 2
let outer = Dict.of_list [("a", inner1); ("b", inner2)]

let () = print_int (Dict.size outer)
