(* Multiple with-type constraints *)

module type PAIR = sig
  type fst
  type snd
  val pair : fst * snd
end

module type INT_STRING = PAIR with type fst = int and type snd = string

module M : INT_STRING = struct
  type fst = int
  type snd = string
  let pair = (42, "hello")
end

let (n, s) = M.pair
let () = print_int n; print_newline ()
let () = print_string s; print_newline ()
