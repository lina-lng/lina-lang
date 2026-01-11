(** Tests for format_type.ml - type expression formatting with semantic awareness. *)

(** {1 Helper Functions} *)

let format source =
  Lina_format.Formatter.format_string_cst source

let check_idempotent source =
  let once = format source in
  let twice = format once in
  if once = twice then "IDEMPOTENT"
  else Printf.sprintf "DIFFERS:\nOnce: [%s]\nTwice: [%s]" once twice

(** {1 Simple Type Formatting} *)

let%expect_test "type variable: simple" =
  let result = format "let x : 'a = value" in
  print_string result;
  [%expect {| let x : 'a = value |}]

let%expect_test "type constructor: builtin int" =
  let result = format "let x : int = 42" in
  print_string result;
  [%expect {| let x : int = 42 |}]

let%expect_test "type constructor: builtin string" =
  let result = format {|let s : string = "hello"|} in
  print_string result;
  [%expect {| let s : string = "hello" |}]

let%expect_test "type constructor: builtin bool" =
  let result = format "let b : bool = true" in
  print_string result;
  [%expect {| let b : bool = true |}]

let%expect_test "type constructor: unit" =
  let result = format "let u : unit = ()" in
  print_string result;
  [%expect {| let u : unit = () |}]

(** {1 Parameterized Type Formatting} *)

let%expect_test "parameterized type: option" =
  let result = format "let x : int option = Some 42" in
  print_string result;
  [%expect {| let x : int option = Some 42 |}]

let%expect_test "parameterized type: list" =
  let result = format "let xs : int list = Nil" in
  print_string result;
  [%expect {| let xs : int list = Nil |}]

let%expect_test "parameterized type: nested" =
  let result = format "let x : int option option = Some (Some 42)" in
  print_string result;
  [%expect {| let x : int option option = Some (Some 42) |}]

let%expect_test "parameterized type: two parameters" =
  let result = format "let x : (int, string) result = Ok 42" in
  print_string result;
  [%expect {| let x : (int, string) result = Ok 42 |}]

(** {1 Arrow Type Formatting} *)

let%expect_test "arrow type: simple" =
  let result = format "let f : int -> string = x" in
  print_string result;
  [%expect {| let f : int -> string = x |}]

let%expect_test "arrow type: multiple arguments" =
  let result = format "let f : int -> string -> bool = x" in
  print_string result;
  [%expect {| let f : int -> string -> bool = x |}]

let%expect_test "arrow type: with type variable" =
  let result = format "let id : 'a -> 'a = fun x -> x" in
  print_string result;
  [%expect {| let id : 'a -> 'a = fun x -> x |}]

let%expect_test "arrow type: with parentheses" =
  let result = format "let apply : ('a -> 'b) -> 'a -> 'b = f" in
  print_string result;
  [%expect {| let apply : ('a -> 'b) -> 'a -> 'b = f |}]

let%expect_test "arrow type: returning function" =
  let result = format "let curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = f" in
  print_string result;
  [%expect {| let curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = f |}]

(** {1 Tuple Type Formatting} *)

let%expect_test "tuple type: pair" =
  let result = format "let p : int * string = (42, x)" in
  print_string result;
  [%expect {| let p : int * string = (42, x) |}]

let%expect_test "tuple type: triple" =
  let result = format "let t : int * string * bool = (1, x, true)" in
  print_string result;
  [%expect {| let t : int * string * bool = (1, x, true) |}]

let%expect_test "tuple type: with parameterized type" =
  let result = format "let t : int option * string list = x" in
  print_string result;
  [%expect {| let t : int option * string list = x |}]

(** {1 Record Type Formatting} *)

let%expect_test "record type: simple declaration" =
  let result = format "type point = { x : int; y : int }" in
  print_string result;
  [%expect {| type point |}]

let%expect_test "record type: single field" =
  let result = format "type wrapper = { value : int }" in
  print_string result;
  [%expect {| type wrapper |}]

let%expect_test "record type: with arrow field" =
  let result = format "type handler = { handle : int -> string }" in
  print_string result;
  [%expect {| type handler |}]

(** {1 Parenthesized Type Formatting} *)

let%expect_test "paren type: simple" =
  let result = format "let f : (int) -> int = x" in
  print_string result;
  [%expect {| let f : (int) -> int = x |}]

let%expect_test "paren type: arrow in paren" =
  let result = format "let f : (int -> string) -> int = x" in
  print_string result;
  [%expect {| let f : (int -> string) -> int = x |}]

let%expect_test "paren type: for tuple parameter" =
  let result = format "let f : (int * string) option = x" in
  print_string result;
  [%expect {| let f : (int * string) option = x |}]

(** {1 Type Alias Formatting} *)

let%expect_test "type alias: simple" =
  let result = format "type int_alias = int" in
  print_string result;
  [%expect {| type int_alias = int |}]

let%expect_test "type alias: parameterized" =
  let result = format "type 'a container = 'a option" in
  print_string result;
  [%expect {| type 'a container = 'a option |}]

let%expect_test "type alias: two parameters" =
  let result = format "type ('a, 'b) pair = 'a * 'b" in
  print_string result;
  [%expect {| type ('a, 'b) pair = 'a * 'b |}]

(** {1 Variant Type Formatting} *)

let%expect_test "variant type: simple" =
  let result = format "type color = Red | Green | Blue" in
  print_string result;
  [%expect {|
    type color =
      | Red
      | Green
      | Blue
    |}]

let%expect_test "variant type: with argument" =
  let result = format "type 'a option = None | Some of 'a" in
  print_string result;
  [%expect {|
    type 'a option =
      | None
      | Some  of  'a
    |}]

let%expect_test "variant type: multiple with arguments" =
  let result = format "type expr = Int of int | Add of expr * expr | Neg of expr" in
  print_string result;
  [%expect {|
    type expr =
      | Int  of  int
      | Add  of  expr * expr
      | Neg  of  expr
    |}]

(** {1 Module Type Path Formatting} *)

let%expect_test "module type path: qualified" =
  let result = format "let x : M.t = value" in
  print_string result;
  [%expect {| let x : M.t = value |}]

let%expect_test "module type path: nested qualified" =
  let result = format "let x : A.B.C.t = value" in
  print_string result;
  [%expect {| let x : A.B.C.t = value |}]

(** {1 Complex Type Combinations} *)

let%expect_test "complex type: function returning option" =
  let result = format "let find : 'a list -> ('a -> bool) -> 'a option = f" in
  print_string result;
  [%expect {| let find : 'a list -> ('a -> bool) -> 'a option = f |}]

let%expect_test "complex type: tuple of functions" =
  let result = format "let pair : (int -> int) * (string -> string) = x" in
  print_string result;
  [%expect {| let pair : (int -> int) * (string -> string) = x |}]

let%expect_test "complex type: record with complex fields" =
  let result = format "type state = { get : unit -> int; set : int -> unit }" in
  print_string result;
  [%expect {| type state |}]

(** {1 Whitespace Normalization} *)

let%expect_test "type whitespace: arrow with extra spaces" =
  let result = format "let f : int  ->  string = x" in
  print_string result;
  [%expect {| let f : int -> string = x |}]

let%expect_test "type whitespace: tuple with extra spaces" =
  let result = format "let p : int  *  string = x" in
  print_string result;
  [%expect {| let p : int * string = x |}]

(** {1 Idempotency Tests} *)

let%expect_test "type idempotency: arrow type" =
  print_string (check_idempotent "let f : int -> string -> bool = x");
  [%expect {| IDEMPOTENT |}]

let%expect_test "type idempotency: tuple type" =
  print_string (check_idempotent "let t : int * string * bool = x");
  [%expect {| IDEMPOTENT |}]

let%expect_test "type idempotency: parameterized type" =
  print_string (check_idempotent "let x : (int, string) result = Ok 42");
  [%expect {| IDEMPOTENT |}]

let%expect_test "type idempotency: record type" =
  print_string (check_idempotent "type point = { x : int; y : int }");
  [%expect {| IDEMPOTENT |}]

let%expect_test "type idempotency: variant type" =
  print_string (check_idempotent "type 'a option = None | Some of 'a");
  [%expect {| IDEMPOTENT |}]

let%expect_test "type idempotency: complex type" =
  print_string (check_idempotent "let f : ('a -> 'b) -> 'a list -> 'b list = x");
  [%expect {| IDEMPOTENT |}]
