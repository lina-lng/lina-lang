(* Test: Dict is covariant - relaxed value restriction *)
(* Expected: ACCEPT - Dict applied to id should be usable polymorphically *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty () = []
  let set k v d = (k, v) :: d
  let size d = List.length d
end

let id x = x
let empty = id (Dict.empty ())

(* The dict type is covariant, so this should work *)
let d1 = Dict.set "a" 1 empty
let d2 = Dict.set true "yes" empty

let () = print_int (Dict.size d1)
let () = print_int (Dict.size d2)
