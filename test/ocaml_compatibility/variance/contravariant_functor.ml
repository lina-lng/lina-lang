(* Contravariant type in functor argument *)

module type CONSUMER = sig
  type -'a t
  val consume : 'a t -> 'a -> unit
end

module Printer : CONSUMER with type 'a t = 'a -> unit = struct
  type 'a t = 'a -> unit
  let consume f x = f x
end

let int_printer : int Printer.t = fun n -> print_int n; print_newline ()
let () = Printer.consume int_printer 42
