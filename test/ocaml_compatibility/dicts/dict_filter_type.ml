(* Test: Dict.filter type inference *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let of_list l = l
  let filter pred d = List.filter (fun (k, v) -> pred k v) d
  let size d = List.length d
end

let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]

(* filter preserves dict type *)
let filtered = Dict.filter (fun k v -> v > 1) d

let () = print_int (Dict.size filtered)
