(* Test: Dict.map type inference *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let of_list l = l
  let map f d = List.map (fun (k, v) -> (k, f v)) d
  let get_or k default d =
    match List.assoc_opt k d with
    | Some v -> v
    | None -> default
end

let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]
let doubled = Dict.map (fun v -> v * 2) d

let () = print_int (Dict.get_or "a" 0 doubled)
