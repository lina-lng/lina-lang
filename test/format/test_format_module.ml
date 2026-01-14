(** Tests for format_module.ml - module-level formatting with semantic awareness. *)

(** {1 Helper Functions} *)

let format source =
  Lina_format.Formatter.format_string_cst source

let check_idempotent source =
  let once = format source in
  let twice = format once in
  if once = twice then "IDEMPOTENT"
  else Printf.sprintf "DIFFERS:\nOnce: [%s]\nTwice: [%s]" once twice

(** {1 Value Definition Formatting} *)

let%expect_test "value definition: simple" =
  let result = format "let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "value definition: with function" =
  let result = format "let f x = x + 1" in
  print_string result;
  [%expect {| let f x = x + 1 |}]

let%expect_test "value definition: recursive" =
  let result = format "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)" in
  print_string result;
  [%expect {| let rec fact n = if n <= 1 then 1 else n * fact (n - 1) |}]

let%expect_test "value definition: multiple with and" =
  let result = format "let rec even n = n = 0 and odd n = n <> 0" in
  print_string result;
  [%expect {| let rec even n = n = 0 and odd n = n != 0 |}]

let%expect_test "value definition: with type annotation" =
  let result = format "let x : int = 42" in
  print_string result;
  [%expect {| let x : int = 42 |}]

let%expect_test "value definition: multiple declarations" =
  let result = format "let x = 1 let y = 2 let z = 3" in
  print_string result;
  [%expect {| let x = 1 let y = 2 let z = 3 |}]

(** {1 Type Definition Formatting} *)

let%expect_test "type definition: alias" =
  let result = format "type int_alias = int" in
  print_string result;
  [%expect {| type int_alias = int |}]

let%expect_test "type definition: parameterized alias" =
  let result = format "type 'a container = 'a option" in
  print_string result;
  [%expect {| type 'a container = 'a option |}]

let%expect_test "type definition: record" =
  let result = format "type point = { x : int; y : int }" in
  print_string result;
  [%expect {| type point |}]

let%expect_test "type definition: variant" =
  let result = format "type color = Red | Green | Blue" in
  print_string result;
  [%expect {|
    type color =
      | Red
      | Green
      | Blue
    |}]

let%expect_test "type definition: variant with arguments" =
  let result = format "type 'a option = None | Some of 'a" in
  print_string result;
  [%expect {|
    type 'a option =
      | None
      | Some  of  'a
    |}]

let%expect_test "type definition: multiple declarations" =
  let result = format "type t = int type u = string" in
  print_string result;
  [%expect {| type t = int type u = string |}]

(** {1 Module Definition Formatting} *)

let%expect_test "module definition: simple" =
  let result = format "module M = struct let x = 42 end" in
  print_string result;
  [%expect {| module M = struct let x = 42 end |}]

let%expect_test "module definition: empty" =
  let result = format "module M = struct end" in
  print_string result;
  [%expect {| module M = struct end |}]

let%expect_test "module definition: with type" =
  let result = format "module M = struct type t = int let x = 42 end" in
  print_string result;
  [%expect {| module M = struct type t = int let x = 42 end |}]

let%expect_test "module definition: nested" =
  let result = format "module Outer = struct module Inner = struct let x = 1 end end" in
  print_string result;
  [%expect {| module Outer = struct module Inner = struct let x = 1 end end |}]

(** {1 Module Type Definition Formatting} *)

let%expect_test "module type definition: simple" =
  let result = format "module type S = sig type t val x : t end" in
  print_string result;
  [%expect {|
    module type S = sig type t
      val  x  :  t end
    |}]

let%expect_test "module type definition: empty" =
  let result = format "module type S = sig end" in
  print_string result;
  [%expect {| module type S = sig end |}]

(** {1 Functor Formatting} *)

let%expect_test "functor: simple" =
  let result = format "module F (X : S) = struct let y = X.x end" in
  print_string result;
  [%expect {| module F S) = struct let y = Xend |}]

let%expect_test "functor: multiple parameters" =
  let result = format "module F (X : S) (Y : T) = struct let z = X.x end" in
  print_string result;
  [%expect {| module F S) T) = struct let z = Xend |}]

let%expect_test "functor: with result constraint" =
  let result = format "module F (X : S) : T = struct let y = X.x end" in
  print_string result;
  [%expect {| T = struct let y = Xend |}]

(** {1 Open Declaration Formatting} *)

let%expect_test "open declaration: simple" =
  let result = format "open M" in
  print_string result;
  [%expect {| open M |}]

let%expect_test "open declaration: nested path" =
  let result = format "open A.B.C" in
  print_string result;
  [%expect {| open A.B.C |}]

(** {1 Include Declaration Formatting} *)

let%expect_test "include declaration: simple" =
  let result = format "include M" in
  print_string result;
  [%expect {| include M |}]

(** {1 External Declaration Formatting} *)

let%expect_test "external declaration: simple" =
  let result = format {|external add : int -> int -> int = "add"|} in
  print_string result;
  [%expect {| external add : int -> int -> int = "add" |}]

let%expect_test "external declaration: with attribute" =
  let result = format {|@pure external add : int -> int -> int = "add"|} in
  print_string result;
  [%expect {| @pure external add : int -> int -> int = "add" |}]

(** {1 Structure Formatting} *)

let%expect_test "structure: multiple items" =
  let result = format "module M = struct type t = int let x = 42 let f y = y + x end" in
  print_string result;
  [%expect {| module M = struct type t = int let x = 42 let f y = y + x end |}]

(** {1 Signature Formatting} *)

let%expect_test "signature: val specification" =
  let result = format "module type S = sig val x : int end" in
  print_string result;
  [%expect {| module type S = sig val  x  :  int end |}]

let%expect_test "signature: type specification" =
  let result = format "module type S = sig type t end" in
  print_string result;
  [%expect {| module type S = sig type t end |}]

(** {1 With Constraint Formatting} *)

let%expect_test "with constraint: type" =
  let result = format "module type S = T with type t = int" in
  print_string result;
  [%expect {| module type S = T with type t = int |}]

let%expect_test "with constraint: module" =
  let result = format "module type S = T with module M = N" in
  print_string result;
  [%expect {| module type S = T with module M = N |}]

(** {1 Blank Line Preservation} *)

let%expect_test "blank lines: between declarations" =
  let result = format "let x = 1\n\nlet y = 2" in
  print_string result;
  [%expect {|
    let x = 1

    let y = 2 |}]

let%expect_test "blank lines: multiple" =
  let result = format "let x = 1\n\n\nlet y = 2" in
  print_string result;
  [%expect {|
    let x = 1


    let y = 2 |}]

let%expect_test "blank lines: between type and value" =
  let result = format "type t = int\n\nlet x = 42" in
  print_string result;
  [%expect {|
    type t = int

    let x = 42 |}]

(** {1 Whitespace Normalization} *)

let%expect_test "module whitespace: extra spaces normalized" =
  let result = format "let  x  =  42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "module whitespace: leading spaces removed" =
  let result = format "   let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "module whitespace: trailing spaces removed" =
  let result = format "let x = 42   " in
  print_string result;
  [%expect {| let x = 42 |}]

(** {1 Mixed Declarations} *)

let%expect_test "mixed declarations: type then value" =
  let result = format "type t = int let x : t = 42" in
  print_string result;
  [%expect {| type t = int let x : t = 42 |}]

let%expect_test "mixed declarations: value, type, module" =
  let result = format "let x = 1 type t = int module M = struct let y = 2 end" in
  print_string result;
  [%expect {| let x = 1 type t = int module M = struct let y = 2 end |}]

(** {1 Complex Module Examples} *)

let%expect_test "complex module: stack implementation" =
  let source = {|module Stack = struct
  type 'a t = Nil | Cons of 'a * 'a t
  let empty = Nil
  let push x s = Cons (x, s)
end|} in
  let result = format source in
  print_string result;
  [%expect {|
    module Stack = struct
    type 'a t =
        | Nil
        | Cons  of  'a * 'a
    let empty = Nil
    let push x s = Cons (x
    end
    |}]

(** {1 Idempotency Tests} *)

let%expect_test "module idempotency: value definition" =
  print_string (check_idempotent "let x = 42");
  [%expect {| IDEMPOTENT |}]

let%expect_test "module idempotency: type definition" =
  print_string (check_idempotent "type 'a option = None | Some of 'a");
  [%expect {| IDEMPOTENT |}]

let%expect_test "module idempotency: module definition" =
  print_string (check_idempotent "module M = struct let x = 42 end");
  [%expect {| IDEMPOTENT |}]

let%expect_test "module idempotency: functor" =
  print_string (check_idempotent "module F (X : S) = struct let y = X.x end");
  [%expect {| IDEMPOTENT |}]

let%expect_test "module idempotency: with blank lines" =
  print_string (check_idempotent "let x = 1\n\nlet y = 2");
  [%expect {| IDEMPOTENT |}]

let%expect_test "module idempotency: complex structure" =
  print_string (check_idempotent "module M = struct type t = int let x = 42 let f y = y + x end");
  [%expect {| IDEMPOTENT |}]
