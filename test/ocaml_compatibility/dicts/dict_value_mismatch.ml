(* Test: Dict value type mismatch *)
(* Expected: REJECT - cannot add string value to int-valued dict *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let singleton k v = [(k, v)]
  let set k v d = (k, v) :: d
end

let d = Dict.singleton "a" 1
let d2 = Dict.set "b" "two" d
