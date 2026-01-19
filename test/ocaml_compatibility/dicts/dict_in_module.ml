(* Test: Dict in module signature *)
(* Expected: ACCEPT *)

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty () = []
  let get k d = List.assoc_opt k d
  let set k v d = (k, v) :: d
end

module type CACHE = sig
  val get : string -> int option
  val set : string -> int -> unit
end

module Cache : CACHE = struct
  let cache = ref (Dict.empty ())

  let get key = Dict.get key !cache

  let set key value =
    cache := Dict.set key value !cache
end

let () = Cache.set "x" 42
let () = match Cache.get "x" with
  | Some v -> print_int v
  | None -> print_int 0
