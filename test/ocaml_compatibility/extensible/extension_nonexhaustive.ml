(* Extensible types are never exhaustive without wildcard *)
(* This should produce a warning about non-exhaustive match *)

type status = ..
type status += Active | Inactive

let check s =
  match s with
  | Active -> true
  | Inactive -> false
  (* Missing wildcard - should warn *)

let () = print_string (string_of_bool (check Active)); print_newline ()
