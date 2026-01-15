(* Basic optional arguments *)
let greet ?title name =
  match title with
  | Some t -> print_string t; print_endline name
  | None -> print_endline name

let () = greet ~title:"Mr." "Smith"
let () = greet "Jones"
