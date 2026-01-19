(* Test: Dict key type mismatch *)
(* Expected: REJECT - cannot use int key with string-keyed dict *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let singleton k v = [(k, v)]
  let get k d = List.assoc_opt k d
end

let d = Dict.singleton "a" 1
let v = Dict.get 42 d
