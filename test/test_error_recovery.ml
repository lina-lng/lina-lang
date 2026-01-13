(** Tests for error-tolerant parsing.

    These tests verify that the parser can recover from syntax errors and
    produce partial ASTs with error nodes, enabling IDE features on invalid code. *)

open Parsing

(** Helper to check if a structure item is an error node. *)
let is_error_item (item : Syntax_tree.structure_item) : bool =
  match item.Common.Location.value with
  | Syntax_tree.StructureError _ -> true
  | _ -> false

(** Helper to count error nodes in a structure. *)
let count_errors (ast : Syntax_tree.structure) : int =
  List.length (List.filter is_error_item ast)

(** Helper to count non-error nodes in a structure. *)
let count_valid (ast : Syntax_tree.structure) : int =
  List.length (List.filter (fun item -> not (is_error_item item)) ast)

let%expect_test "valid code parses without errors" =
  let ast, errors = Parse.structure_from_string_tolerant "let x = 1" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  [%expect {| Items: 1, Errors: 0, Valid: 1 |}]

let%expect_test "multiple valid bindings parse correctly" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = 1\nlet y = 2\nlet z = 3" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  [%expect {| Items: 3, Errors: 0, Valid: 3 |}]

let%expect_test "incomplete let binding produces error node" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = \nlet y = 2" in
  Printf.printf "Items: %d, Errors: %d, Error nodes: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_errors ast) (count_valid ast);
  (* Error recovery creates an error node; the second binding is consumed *)
  if List.length errors > 0 then
    Printf.printf "Has error messages: yes\n"
  else
    Printf.printf "Has error messages: no\n";
  [%expect {|
    Items: 1, Errors: 1, Error nodes: 1, Valid: 0
    Has error messages: yes
    |}]

let%expect_test "missing expression in if produces error" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = if then 1 else 2" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  if List.length errors > 0 then
    Printf.printf "Recovery occurred: yes\n"
  else
    Printf.printf "Recovery occurred: no\n";
  [%expect {|
    Items: 1, Errors: 1
    Recovery occurred: yes |}]

let%expect_test "valid code after error - current limitation" =
  (* NOTE: Current error recovery creates error nodes for all items after
     a parse error. This is a known limitation - ideally y=42 and z=100
     would be parsed as valid bindings. *)
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = if then 1\nlet y = 42\nlet z = 100" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  if count_valid ast >= 1 then
    Printf.printf "Recovered valid code: yes\n"
  else
    Printf.printf "Recovered valid code: no\n";
  [%expect {|
    Items: 3, Errors: 3, Valid: 0
    Recovered valid code: no
    |}]

let%expect_test "type declaration parses correctly" =
  let ast, errors = Parse.structure_from_string_tolerant
    "type point = { x : int; y : int }" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  [%expect {| Items: 1, Errors: 0, Valid: 1 |}]

let%expect_test "module declaration parses correctly" =
  let ast, errors = Parse.structure_from_string_tolerant
    "module M = struct let x = 1 end" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  [%expect {| Items: 1, Errors: 0, Valid: 1 |}]

let%expect_test "empty input produces empty AST" =
  let ast, errors = Parse.structure_from_string_tolerant "" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  [%expect {| Items: 0, Errors: 0 |}]

let%expect_test "error recovery with function definitions" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let f x = \nlet g y = y + 1" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  if List.length errors > 0 then
    Printf.printf "Detected incomplete function: yes\n"
  else
    Printf.printf "Detected incomplete function: no\n";
  [%expect {|
    Items: 1, Errors: 1
    Detected incomplete function: yes |}]

let%expect_test "match expression with missing arm" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = match y with | Some z ->" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  if List.length errors > 0 then
    Printf.printf "Detected incomplete match: yes\n"
  else
    Printf.printf "Detected incomplete match: no\n";
  [%expect {|
    Items: 1, Errors: 1
    Detected incomplete match: yes |}]

(* ============================================================ *)
(* Cascading Error Tests *)
(* ============================================================ *)

let%expect_test "cascading errors: multiple consecutive incomplete bindings" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let a = \nlet b = \nlet c = " in
  Printf.printf "Items: %d, Errors: %d, Error nodes: %d\n"
    (List.length ast) (List.length errors) (count_errors ast);
  [%expect {| Items: 1, Errors: 1, Error nodes: 1 |}]

let%expect_test "cascading errors: mixed valid and invalid bindings" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let a = 1\nlet b = \nlet c = 3\nlet d = \nlet e = 5" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d, Error nodes: %d\n"
    (List.length ast) (List.length errors) (count_valid ast) (count_errors ast);
  [%expect {| Items: 3, Errors: 3, Valid: 0, Error nodes: 3 |}]

let%expect_test "cascading errors: error in nested expression" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let x = if then 1 else 2\nlet y = 42" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  [%expect {| Items: 2, Errors: 2 |}]

let%expect_test "cascading errors: recovery after type declaration" =
  let ast, errors = Parse.structure_from_string_tolerant
    "type t = \nlet x = 42" in
  Printf.printf "Items: %d, Errors: %d, Valid: %d\n"
    (List.length ast) (List.length errors) (count_valid ast);
  [%expect {| Items: 2, Errors: 2, Valid: 0 |}]

let%expect_test "cascading errors: multiple syntax errors in one item" =
  let ast, errors = Parse.structure_from_string_tolerant
    "let f x = if then else" in
  Printf.printf "Items: %d, Errors: %d\n"
    (List.length ast) (List.length errors);
  if List.length errors > 0 then
    Printf.printf "Syntax errors detected: yes\n"
  else
    Printf.printf "Syntax errors detected: no\n";
  [%expect {|
    Items: 1, Errors: 1
    Syntax errors detected: yes |}]
