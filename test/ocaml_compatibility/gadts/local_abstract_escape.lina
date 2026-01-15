(* REJECT: Locally abstract type escapes its scope *)

type box = Box : 'a -> box

let bad_escape () =
  let Box x = Box 42 in
  x  (* x has existential type that should not escape *)
