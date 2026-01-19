(* Test: Dict.fold type inference *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let of_list l = l
  let fold f d init = List.fold_left (fun acc (k, v) -> f k v acc) init d
end

let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]

(* fold accumulates values *)
let sum = Dict.fold (fun k v acc -> acc + v) d 0

(* fold can change accumulator type *)
let keys = Dict.fold (fun k v acc -> k :: acc) d []

let () = print_int sum
