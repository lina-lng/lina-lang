(* Mutually recursive modules *)

module rec Even : sig
  val check : int -> bool
end = struct
  let check n =
    if n = 0 then true
    else Odd.check (n - 1)
end

and Odd : sig
  val check : int -> bool
end = struct
  let check n =
    if n = 0 then false
    else Even.check (n - 1)
end

let () = print_string (string_of_bool (Even.check 4)); print_newline ()
let () = print_string (string_of_bool (Odd.check 3)); print_newline ()
