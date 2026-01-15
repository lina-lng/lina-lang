(* Labeled arguments with GADT *)

type _ ty =
  | TInt : int ty
  | TString : string ty

let make_value : type a. ty:a ty -> value:a -> a = fun ~ty ~value ->
  match ty with
  | TInt -> value + 0
  | TString -> value ^ ""

let int_val = make_value ~ty:TInt ~value:42
let str_val = make_value ~ty:TString ~value:"hello"

let () = print_int int_val; print_newline ()
let () = print_string str_val; print_newline ()
