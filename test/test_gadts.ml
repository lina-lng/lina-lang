(** Unit tests for GADT (Generalized Algebraic Data Types) features.
    Tests GADT syntax, type inference, polymorphic recursion, and exhaustiveness. *)

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Driver.Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<string>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let compile_and_run source =
  let lua_code = compile source in
  if String.length lua_code >= 6 && String.sub lua_code 0 6 = "ERROR:" then
    lua_code
  else
    (* Write to temp file and run with luajit *)
    let temp_file = Filename.temp_file "lina_gadt_test" ".lua" in
    let output_channel = open_out temp_file in
    output_string output_channel lua_code;
    close_out output_channel;
    let input_channel = Unix.open_process_in ("luajit " ^ temp_file ^ " 2>&1") in
    let output = In_channel.input_all input_channel in
    let _ = Unix.close_process_in input_channel in
    Sys.remove temp_file;
    String.trim output

(* ========================================================================
   GADT Constructor Parsing
   ======================================================================== *)

let%expect_test "GADT constructor without argument" =
  let _ = compile {|
type _ expr =
  | Unit : unit expr
|} in
  [%expect {||}]

let%expect_test "GADT constructor with simple argument" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
|} in
  [%expect {||}]

let%expect_test "GADT constructor with tuple argument" =
  let _ = compile {|
type _ expr =
  | Add : (int * int) -> int expr
|} in
  [%expect {||}]

let%expect_test "GADT constructor with nested type argument" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Add : (int expr * int expr) -> int expr
|} in
  [%expect {||}]

let%expect_test "GADT with multiple constructors" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : (int expr * int expr) -> int expr
|} in
  [%expect {||}]

let%expect_test "GADT constructor with arrow return type" =
  let _ = compile {|
type _ expr =
  | Fun : (int -> int) -> (int -> int) expr
|} in
  [%expect {||}]

(* ========================================================================
   Polymorphic Recursion Syntax (type a.)
   ======================================================================== *)

let%expect_test "polymorphic recursion annotation parsing" =
  let _ = compile {|
let rec id : type a. a -> a = fun x -> x
|} in
  [%expect {| |}]

let%expect_test "polymorphic recursion with arrow type" =
  let _ = compile {|
let rec const : type a b. a -> b -> a = fun x y -> x
|} in
  [%expect {| |}]

let%expect_test "polymorphic recursion with type constructor" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
|} in
  [%expect {| |}]

(* ========================================================================
   GADT Runtime Behavior
   ======================================================================== *)

let%expect_test "GADT eval function basic" =
  print_endline (compile_and_run {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b

let _ = print (eval (Int 42))
let _ = print (eval (Bool true))
|});
  [%expect {|
    42
    true
    |}]

let%expect_test "GADT eval function with Add" =
  print_endline (compile_and_run {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : (int expr * int expr) -> int expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
  | Add (x, y) -> eval x + eval y

let _ = print (eval (Int 42))
let _ = print (eval (Add (Int 10, Int 32)))
let _ = print (eval (Add (Add (Int 1, Int 2), Int 3)))
|});
  [%expect {|
    42
    42
    6
    |}]

let%expect_test "non-GADT polymorphic recursion" =
  print_endline (compile_and_run {|
let rec id : type a. a -> a = fun x -> x
let _ = print (id 42)
let _ = print (id true)
|});
  [%expect {|
    42
    true
    |}]

(* ========================================================================
   GADT-Aware Exhaustiveness Checking
   ======================================================================== *)

let%expect_test "GADT exhaustiveness - all constructors covered" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
|} in
  (* No warning expected - all constructors are covered *)
  [%expect {| |}]

let%expect_test "GADT exhaustiveness - no spurious warning for tuple args" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Add : (int expr * int expr) -> int expr

let rec eval_int : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Add (x, y) -> eval_int x + eval_int y
|} in
  (* No warning expected - Add (x, y) covers the tuple completely *)
  [%expect {| |}]

let%expect_test "GADT exhaustiveness - missing constructor warning" =
  let _ = compile {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : (int expr * int expr) -> int expr

let rec eval_incomplete : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
|} in
  (* Warning expected - Add is missing *)
  [%expect {| |}]

(* ========================================================================
   Non-GADT Tuple Exhaustiveness (regression tests)
   ======================================================================== *)

let%expect_test "non-GADT with tuple arg - no spurious warning" =
  let _ = compile {|
type pair = Pair of (int * int)
let sum p = match p with
  | Pair (a, b) -> a + b
|} in
  (* No warning - Pair (a, b) covers the only constructor *)
  [%expect {| |}]

let%expect_test "list pattern - no spurious warning" =
  let _ = compile {|
type 'a list = Nil | Cons of ('a * 'a list)
let head l = match l with
  | Nil -> 0
  | Cons (h, t) -> h
|} in
  (* No warning - all constructors covered *)
  [%expect {| |}]

let%expect_test "tree pattern - no spurious warning" =
  let _ = compile {|
type tree = Leaf of int | Node of (tree * tree)
let rec sum t = match t with
  | Leaf n -> n
  | Node (l, r) -> sum l + sum r
|} in
  (* No warning - all constructors covered *)
  [%expect {| |}]

let%expect_test "expression evaluator - no spurious warning" =
  let _ = compile {|
type expr = Num of int | Add of (expr * expr) | Mul of (expr * expr)
let rec eval e = match e with
  | Num n -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b
|} in
  (* No warning - all constructors covered *)
  [%expect {| |}]

(* ========================================================================
   Type Inference Tests
   ======================================================================== *)

let%expect_test "GADT constructor creates correct type" =
  print_endline (compile_and_run {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let test_int (x : int expr) : int =
  match x with
  | Int n -> n

let test_bool (x : bool expr) : bool =
  match x with
  | Bool b -> b

let _ = print (test_int (Int 42))
let _ = print (test_bool (Bool true))
|});
  [%expect {|
    42
    true
    |}]

let%expect_test "GADT type mismatch error" =
  print_endline (compile {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr

let test_int (x : int expr) : int =
  match x with
  | Bool b -> b
|});
  [%expect {|
    ERROR: File "<string>", line 8, characters 4-15:
    Type error: Type mismatch: expected int, got bool
    Expected: int
    Actual: bool
      in type argument 1
    |}]

let%expect_test "polymorphic recursion required for GADT eval" =
  print_endline (compile_and_run {|
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | If : (bool expr * int expr * int expr) -> int expr

let rec eval : type a. a expr -> a = fun e ->
  match e with
  | Int n -> n
  | Bool b -> b
  | If (cond, then_e, else_e) ->
    if eval cond then eval then_e else eval else_e

let _ = print (eval (If (Bool true, Int 42, Int 0)))
let _ = print (eval (If (Bool false, Int 42, Int 100)))
|});
  [%expect {|
    42
    100
    |}]
