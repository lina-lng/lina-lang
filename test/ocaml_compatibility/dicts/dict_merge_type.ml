(* Test: Dict.merge type inference *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let of_list l = l
  let merge d1 d2 = d2 @ d1
  let size d = List.length d
end

let d1 = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.of_list [("c", 3); ("d", 4)]

(* merge combines two dicts of the same type *)
let merged = Dict.merge d1 d2

let () = print_int (Dict.size merged)
