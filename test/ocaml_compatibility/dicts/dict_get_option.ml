(* Test: Dict.get returns option type *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let singleton k v = [(k, v)]
  let get k d = List.assoc_opt k d
end

let d = Dict.singleton "a" 42

let result = Dict.get "a" d

let value = match result with
  | Some v -> v
  | None -> 0

let () = print_int value
