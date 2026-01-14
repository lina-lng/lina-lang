(* Error: missing record field *)
(* Expected: REJECT - y field is missing *)
(* Note: OCaml uses nominal records and infers the type from field names,
   so it rejects this even without annotation. Lina uses structural records,
   so a type annotation is needed to require all fields. *)

type point = { x : int; y : int }

let p : point = { x = 1 }
