(* Extension constructors added in a module *)

type action = ..

module Actions = struct
  type action += Save | Load of string | Delete of int
end

let describe a =
  match a with
  | Actions.Save -> "save"
  | Actions.Load path -> "load"
  | Actions.Delete id -> "delete"
  | _ -> "unknown"

let () = print_string (describe Actions.Save); print_newline ()
let () = print_string (describe (Actions.Load "file.txt")); print_newline ()
let () = print_string (describe (Actions.Delete 42)); print_newline ()
