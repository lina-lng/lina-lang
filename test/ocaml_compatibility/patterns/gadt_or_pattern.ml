(* Or-pattern in GADT match with type refinement *)

type _ ty =
  | TInt : int ty
  | TBool : bool ty
  | TString : string ty

type _ value =
  | VInt : int -> int value
  | VBool : bool -> bool value
  | VString : string -> string value

let get_default : type a. a ty -> a = function
  | TInt | TBool ->
    (* Both branches refine to different types, so this should fail
       unless we use a common supertype. Let's use a valid version: *)
    0  (* This won't work - let's fix it *)
  | TString -> ""

(* Valid version - separate cases *)
let get_default2 : type a. a ty -> a = function
  | TInt -> 0
  | TBool -> false
  | TString -> ""

let () = print_int (get_default2 TInt); print_newline ()
let () = print_string (string_of_bool (get_default2 TBool)); print_newline ()
