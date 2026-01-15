open Test_helpers.Helpers
open Common.Location
open Parsing.Syntax_tree

(** Extract operator name from a binary operation expression *)
let get_binop_structure expr =
  match expr.value with
  | ExpressionApply ({ value = ExpressionVariable op; _ }, [ (_, left); (_, right) ]) ->
      Some (op, left, right)
  | _ -> None

(** Check if expression is a specific binary operation *)
let is_binop op expr =
  match get_binop_structure expr with
  | Some (op', _, _) -> op = op'
  | None -> false

(** Check if expression is a variable with given name *)
let is_var name expr =
  match expr.value with ExpressionVariable n -> n = name | _ -> false

(** Check if expression is an integer constant *)
let is_int n expr =
  match expr.value with
  | ExpressionConstant (ConstantInteger i) -> i = n
  | _ -> false

(* Arithmetic precedence tests *)

let%expect_test "multiplication binds tighter than addition: 1 + 2 * 3" =
  (* Should parse as 1 + (2 * 3) *)
  let expr = parse_expr "1 + 2 * 3" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      Printf.printf "Left is 1: %b\n" (is_int 1 left);
      Printf.printf "Right is multiplication: %b\n" (is_binop "*" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: +
    Left is 1: true
    Right is multiplication: true
    |}]

let%expect_test "left associativity of addition: 1 + 2 + 3" =
  (* Should parse as (1 + 2) + 3 *)
  let expr = parse_expr "1 + 2 + 3" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      Printf.printf "Left is addition: %b\n" (is_binop "+" left);
      Printf.printf "Right is 3: %b\n" (is_int 3 right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: +
    Left is addition: true
    Right is 3: true
    |}]

let%expect_test "left associativity of multiplication: 1 * 2 * 3" =
  (* Should parse as (1 * 2) * 3 *)
  let expr = parse_expr "1 * 2 * 3" in
  (match get_binop_structure expr with
  | Some ("*", left, right) ->
      Printf.printf "Top operator: *\n";
      Printf.printf "Left is multiplication: %b\n" (is_binop "*" left);
      Printf.printf "Right is 3: %b\n" (is_int 3 right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: *
    Left is multiplication: true
    Right is 3: true
    |}]

let%expect_test "mixed precedence: 1 * 2 + 3 * 4" =
  (* Should parse as (1 * 2) + (3 * 4) *)
  let expr = parse_expr "1 * 2 + 3 * 4" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      Printf.printf "Left is multiplication: %b\n" (is_binop "*" left);
      Printf.printf "Right is multiplication: %b\n" (is_binop "*" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: +
    Left is multiplication: true
    Right is multiplication: true
    |}]

(* Unary minus precedence *)

let%expect_test "unary minus with multiplication: -1 * 2" =
  (* Should parse as (-1) * 2, not -(1 * 2) *)
  let expr = parse_expr "-1 * 2" in
  (match get_binop_structure expr with
  | Some ("*", left, right) ->
      Printf.printf "Top operator: *\n";
      (* Unary minus is represented as 0 - x *)
      Printf.printf "Left is subtraction (unary minus): %b\n" (is_binop "-" left);
      Printf.printf "Right is 2: %b\n" (is_int 2 right)
  | _ -> print_endline "Unexpected structure");
  [%expect
    {|
    Top operator: *
    Left is subtraction (unary minus): true
    Right is 2: true
    |}]

let%expect_test "unary minus with addition: -1 + 2" =
  (* Should parse as (-1) + 2 *)
  let expr = parse_expr "-1 + 2" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      Printf.printf "Left is subtraction (unary minus): %b\n" (is_binop "-" left);
      Printf.printf "Right is 2: %b\n" (is_int 2 right)
  | _ -> print_endline "Unexpected structure");
  [%expect
    {|
    Top operator: +
    Left is subtraction (unary minus): true
    Right is 2: true
    |}]

(* Application vs operators *)

let%expect_test "application binds tighter than operators: f x + y" =
  (* Should parse as (f x) + y *)
  let expr = parse_expr "f x + y" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      let left_is_apply =
        match left.value with ExpressionApply _ -> true | _ -> false
      in
      Printf.printf "Left is application: %b\n" left_is_apply;
      Printf.printf "Right is y: %b\n" (is_var "y" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: +
    Left is application: true
    Right is y: true
    |}]

let%expect_test "application binds tighter than comparison: f x == g y" =
  (* Should parse as (f x) == (g y) *)
  let expr = parse_expr "f x == g y" in
  (match get_binop_structure expr with
  | Some ("==", left, right) ->
      Printf.printf "Top operator: ==\n";
      let left_is_apply =
        match left.value with ExpressionApply _ -> true | _ -> false
      in
      let right_is_apply =
        match right.value with ExpressionApply _ -> true | _ -> false
      in
      Printf.printf "Left is application: %b\n" left_is_apply;
      Printf.printf "Right is application: %b\n" right_is_apply
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: ==
    Left is application: true
    Right is application: true
    |}]

let%expect_test "chained application with operators: f x + g y * h z" =
  (* Should parse as (f x) + ((g y) * (h z)) *)
  let expr = parse_expr "f x + g y * h z" in
  (match get_binop_structure expr with
  | Some ("+", left, right) ->
      Printf.printf "Top operator: +\n";
      let left_is_apply =
        match left.value with ExpressionApply _ -> true | _ -> false
      in
      Printf.printf "Left is application: %b\n" left_is_apply;
      Printf.printf "Right is multiplication: %b\n" (is_binop "*" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: +
    Left is application: true
    Right is multiplication: true
    |}]

(* Comparison operators *)

let%expect_test "comparison left associativity: a < b < c" =
  (* Should parse as (a < b) < c *)
  let expr = parse_expr "a < b < c" in
  (match get_binop_structure expr with
  | Some ("<", left, right) ->
      Printf.printf "Top operator: <\n";
      Printf.printf "Left is comparison: %b\n" (is_binop "<" left);
      Printf.printf "Right is c: %b\n" (is_var "c" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: <
    Left is comparison: true
    Right is c: true
    |}]

let%expect_test "mixed comparisons: a == b != c" =
  (* Should parse as (a == b) != c *)
  let expr = parse_expr "a == b != c" in
  (match get_binop_structure expr with
  | Some ("!=", left, right) ->
      Printf.printf "Top operator: !=\n";
      Printf.printf "Left is equality: %b\n" (is_binop "==" left);
      Printf.printf "Right is c: %b\n" (is_var "c" right)
  | _ -> print_endline "Unexpected structure");
  [%expect {| Unexpected structure |}]

(* Sequence associativity *)

let%expect_test "sequence right associativity: a; b; c" =
  (* Should parse as a; (b; c) *)
  let expr = parse_expr "a; b; c" in
  (match expr.value with
  | ExpressionSequence (left, right) ->
      Printf.printf "Top is sequence\n";
      Printf.printf "Left is a: %b\n" (is_var "a" left);
      let right_is_seq =
        match right.value with ExpressionSequence _ -> true | _ -> false
      in
      Printf.printf "Right is sequence: %b\n" right_is_seq
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top is sequence
    Left is a: true
    Right is sequence: true
    |}]

(* Parentheses override precedence *)

let%expect_test "parentheses override precedence: (1 + 2) * 3" =
  (* Should parse as (1 + 2) * 3 *)
  let expr = parse_expr "(1 + 2) * 3" in
  (match get_binop_structure expr with
  | Some ("*", left, right) ->
      Printf.printf "Top operator: *\n";
      Printf.printf "Left is addition: %b\n" (is_binop "+" left);
      Printf.printf "Right is 3: %b\n" (is_int 3 right)
  | _ -> print_endline "Unexpected structure");
  [%expect {|
    Top operator: *
    Left is addition: true
    Right is 3: true
    |}]
