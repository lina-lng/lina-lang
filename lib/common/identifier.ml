type t = {
  name : string;
  stamp : int;
}
[@@deriving show, eq, ord]

let next_stamp = ref 0

let create name =
  let stamp = !next_stamp in
  incr next_stamp;
  { name; stamp }

let create_with_stamp name stamp = { name; stamp }

let name id = id.name
let stamp id = id.stamp

module Set = Set.Make (struct
  type nonrec t = t
  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t
  let compare = compare
end)
