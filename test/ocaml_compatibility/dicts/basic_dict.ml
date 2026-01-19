(* Test: Basic dict operations type inference *)
(* Expected: ACCEPT *)

(* OCaml equivalent using a simple immutable dict abstraction *)
module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty () = []
  let singleton k v = [(k, v)]
  let set k v d = (k, v) :: d
  let size d = List.length d
end

let empty_dict = Dict.empty ()
let single = Dict.singleton "key" 42
let with_binding = Dict.set "a" 1 (Dict.empty ())

let () = print_int (Dict.size single)
