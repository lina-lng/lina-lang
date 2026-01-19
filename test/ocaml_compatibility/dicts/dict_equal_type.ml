(* Test: Dict.equal type inference *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let of_list l = l
  let size d = List.length d
  let entries d = d
  let get k d = List.assoc_opt k d
  let equal eq d1 d2 =
    if size d1 <> size d2 then false
    else List.for_all (fun (k, v1) ->
      match get k d2 with
      | None -> false
      | Some v2 -> eq v1 v2
    ) (entries d1)
end

let d1 = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.of_list [("a", 1); ("b", 2)]

(* equal takes a value equality function *)
let eq = Dict.equal (fun a b -> a = b) d1 d2

let () = print_string (if eq then "true" else "false")
