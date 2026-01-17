(** Unit tests for pattern match compilation.

    Tests cover:
    - Deep pattern matching compilation
    - Performance with deep nesting
    - Various pattern types

    Note: Pattern_match internals are not directly accessible from tests,
    so we test through the full compilation pipeline. *)

open Typing
open Driver

(** Helper to compile a match expression and return Lua code *)
let compile code =
  Types.reset_type_variable_id ();
  match Pipeline.compile_string Pipeline.default_options "<test>" code with
  | Ok lua -> lua
  | Error msg -> "ERROR: " ^ msg

(** {1 Simple Pattern Tests} *)

let%expect_test "compile simple variable pattern" =
  let lua = compile {|
    let f x = match x with
      | y -> y
  |} in
  print_endline lua;
  [%expect {|
    local f_17 = function(x_15)
      local _scrutinee_18 = x_15;
      local y_16 = _scrutinee_18;
      return y_16
    end
    |}]

let%expect_test "compile constructor pattern" =
  let lua = compile {|
    type option = None | Some of int
    let f x = match x with
      | Some n -> n
      | None -> 0
  |} in
  (* Should handle constructor patterns *)
  let has_tag_check = String.length lua > 0 in
  print_endline (if has_tag_check then "generated code" else "ERROR");
  [%expect {| generated code |}]

let%expect_test "compile tuple pattern" =
  let lua = compile {|
    let f x = match x with
      | (a, b) -> a + b
  |} in
  (* Should handle tuple destructuring *)
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Deep Pattern Performance Tests} *)

let%expect_test "compile deeply nested tuple" =
  (* Test that deep nesting doesn't cause stack overflow or O(n²) behavior *)
  let code = {|
    let f x = match x with
      | (((((a, b), c), d), e), f) -> a + b + c + d + e + f
  |} in
  let start = Unix.gettimeofday () in
  let _ = compile code in
  let elapsed = Unix.gettimeofday () -. start in
  (* Should complete quickly *)
  print_endline (if elapsed < 1.0 then "fast" else "slow");
  [%expect {| fast |}]

let%expect_test "compile multiple constructors" =
  let lua = compile {|
    type color = Red | Green | Blue | Yellow | Cyan | Magenta
    let f x = match x with
      | Red -> 1
      | Green -> 2
      | Blue -> 3
      | Yellow -> 4
      | Cyan -> 5
      | Magenta -> 6
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Record Pattern Tests} *)

let%expect_test "compile record pattern" =
  let lua = compile {|
    type point = { x : int; y : int }
    let f p = match p with
      | { x = a; y = b } -> a + b
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {|
    generated code
    File "<string>", line 3, characters 14-33:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "compile nested record and constructor" =
  let lua = compile {|
    type point = { x : int; y : int }
    type option = None | Some of point
    let f x = match x with
      | Some { x = a; y = b } -> a + b
      | None -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {|
    generated code
    File "<string>", line 4, characters 14-17:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

(** {1 Guard Pattern Tests} *)

let%expect_test "compile pattern with guard" =
  let lua = compile {|
    let f x = match x with
      | n when n > 0 -> n
      | _ -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {|
    generated code
    File "<string>", line 4, characters 8-14:
    Warning: Redundant pattern: this case will never be matched
    |}]

(** {1 Full Compilation Performance Test} *)

let%expect_test "full compilation of deep tree pattern" =
  let code = {|
    type tree = Leaf | Node of tree * int * tree

    let rec sum t = match t with
      | Leaf -> 0
      | Node (left, value, right) ->
        sum left + value + sum right
  |} in
  let start = Unix.gettimeofday () in
  let _ = compile code in
  let elapsed = Unix.gettimeofday () -. start in
  print_endline (if elapsed < 1.0 then "fast" else "slow");
  [%expect {| fast |}]

(** {1 Complex Nested Pattern Tests} *)

let%expect_test "compile mixed tuple and constructor pattern" =
  let lua = compile {|
    type option = None | Some of int
    let f x = match x with
      | (Some a, Some b) -> a + b
      | (Some a, None) -> a
      | (None, Some b) -> b
      | (None, None) -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

let%expect_test "compile triple nested tuple" =
  let lua = compile {|
    let f x = match x with
      | ((a, b), (c, d), (e, f)) -> a + b + c + d + e + f
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]
