(* Result: map_error transforms error values *)
(* Expected: ACCEPT - map_error preserves Ok, transforms Error *)

let map_error f r = match r with
  | Ok x -> Ok x
  | Error e -> Error (f e)

let err = Error 404
let mapped = map_error (fun code -> "Error code: " ^ "404") err

let ok_val = Ok 42
let ok_mapped = map_error (fun _ -> "never called") ok_val

let get r = match r with
  | Ok x -> x
  | Error _ -> 0

let () = print_int (get ok_mapped)
