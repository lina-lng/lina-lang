(* First-class module containing GADT *)
(* Simplified to avoid string_of_int and return int instead of string *)

module type TYPED_VALUE = sig
  type _ t
  type packed = Pack : 'a t * 'a -> packed
  val int_t : int t
  val bool_t : bool t
  val to_int : 'a t -> 'a -> int
end

module TypedValue : TYPED_VALUE = struct
  type _ t =
    | TInt : int t
    | TBool : bool t

  type packed = Pack : 'a t * 'a -> packed

  let int_t = TInt
  let bool_t = TBool

  let to_int : type a. a t -> a -> int = fun t v ->
    match t with
    | TInt -> v
    | TBool -> if v then 1 else 0
end

let packed = (module TypedValue : TYPED_VALUE)

let test () =
  let module TV = (val packed : TYPED_VALUE) in
  TV.to_int TV.int_t 42

let () = print_int (test ()); print_newline ()
