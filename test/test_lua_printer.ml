(** Tests for Lua AST printer functions.

    These tests verify the Lua printer correctly handles edge cases
    like string escaping, number formatting, and operator precedence. *)

open Lua.Lua_ast
open Lua.Printer

(* === String Escaping Tests === *)

let%expect_test "escape quotes in string" =
  print_endline (print_expression (ExpressionString {|hello "world"|}));
  [%expect{| "hello \"world\"" |}]

let%expect_test "escape backslash in string" =
  print_endline (print_expression (ExpressionString {|path\to\file|}));
  [%expect{| "path\\to\\file" |}]

let%expect_test "escape newlines in string" =
  print_endline (print_expression (ExpressionString "line1\nline2"));
  [%expect{| "line1\nline2" |}]

let%expect_test "escape tabs in string" =
  print_endline (print_expression (ExpressionString "col1\tcol2"));
  [%expect{| "col1\tcol2" |}]

let%expect_test "escape carriage return in string" =
  print_endline (print_expression (ExpressionString "text\rmore"));
  [%expect{| "text\rmore" |}]

let%expect_test "mixed escape sequences" =
  print_endline (print_expression (ExpressionString "a\tb\nc\\d\"e"));
  [%expect{| "a\tb\nc\\d\"e" |}]

let%expect_test "empty string" =
  print_endline (print_expression (ExpressionString ""));
  [%expect{| "" |}]

(* === Number Formatting Tests === *)

let%expect_test "integer without decimal" =
  print_endline (print_expression (ExpressionNumber 42.0));
  [%expect{| 42 |}]

let%expect_test "float with decimal" =
  print_endline (print_expression (ExpressionNumber 3.14));
  [%expect{| 3.14 |}]

let%expect_test "negative integer" =
  print_endline (print_expression (ExpressionNumber (-5.0)));
  [%expect{| -5 |}]

let%expect_test "negative float" =
  print_endline (print_expression (ExpressionNumber (-2.5)));
  [%expect{| -2.5 |}]

let%expect_test "zero" =
  print_endline (print_expression (ExpressionNumber 0.0));
  [%expect{| 0 |}]

let%expect_test "large integer" =
  print_endline (print_expression (ExpressionNumber 1000000.0));
  [%expect{| 1000000 |}]

let%expect_test "small decimal" =
  print_endline (print_expression (ExpressionNumber 0.001));
  [%expect{| 0.001 |}]

(* === Operator Precedence Tests === *)

let%expect_test "lower precedence gets parens" =
  (* a + b * c should NOT need parens around b * c *)
  let expr = ExpressionBinaryOp (OpAdd,
    ExpressionVariable "a",
    ExpressionBinaryOp (OpMul, ExpressionVariable "b", ExpressionVariable "c"))
  in
  print_endline (print_expression expr);
  [%expect{| a + b * c |}]

let%expect_test "higher precedence no extra parens" =
  (* (a + b) * c needs parens around a + b *)
  let expr = ExpressionBinaryOp (OpMul,
    ExpressionBinaryOp (OpAdd, ExpressionVariable "a", ExpressionVariable "b"),
    ExpressionVariable "c")
  in
  print_endline (print_expression expr);
  [%expect{| (a + b) * c |}]

let%expect_test "same precedence left associative" =
  (* a - b - c should be (a - b) - c *)
  let expr = ExpressionBinaryOp (OpSub,
    ExpressionBinaryOp (OpSub, ExpressionVariable "a", ExpressionVariable "b"),
    ExpressionVariable "c")
  in
  print_endline (print_expression expr);
  [%expect{| a - b - c |}]

let%expect_test "logical and lower than comparison" =
  (* a < b and c > d *)
  let expr = ExpressionBinaryOp (OpAnd,
    ExpressionBinaryOp (OpLess, ExpressionVariable "a", ExpressionVariable "b"),
    ExpressionBinaryOp (OpGreater, ExpressionVariable "c", ExpressionVariable "d"))
  in
  print_endline (print_expression expr);
  [%expect{| a < b and c > d |}]

let%expect_test "logical or lowest precedence" =
  (* a and b or c and d *)
  let expr = ExpressionBinaryOp (OpOr,
    ExpressionBinaryOp (OpAnd, ExpressionVariable "a", ExpressionVariable "b"),
    ExpressionBinaryOp (OpAnd, ExpressionVariable "c", ExpressionVariable "d"))
  in
  print_endline (print_expression expr);
  [%expect{| a and b or c and d |}]

let%expect_test "unary operator precedence" =
  let expr = ExpressionUnaryOp (OpNegate, ExpressionVariable "x") in
  print_endline (print_expression expr);
  [%expect{| -x |}]

let%expect_test "unary not with parens" =
  let expr = ExpressionUnaryOp (OpNot,
    ExpressionBinaryOp (OpAnd, ExpressionVariable "a", ExpressionVariable "b"))
  in
  print_endline (print_expression expr);
  [%expect{| not (a and b) |}]

(* === Deep Nesting Tests === *)

let%expect_test "deeply nested calls" =
  let expr = ExpressionCall (
    ExpressionCall (
      ExpressionCall (ExpressionVariable "f", [ExpressionVariable "a"]),
      [ExpressionVariable "b"]),
    [ExpressionVariable "c"])
  in
  print_endline (print_expression expr);
  [%expect{| f(a)(b)(c) |}]

let%expect_test "deeply nested field access" =
  let expr = ExpressionField (
    ExpressionField (
      ExpressionField (ExpressionVariable "a", "b"),
      "c"),
    "d")
  in
  print_endline (print_expression expr);
  [%expect{| a.b.c.d |}]

let%expect_test "mixed deep nesting - field and call" =
  let expr = ExpressionCall (
    ExpressionField (
      ExpressionCall (ExpressionVariable "obj", [ExpressionNumber 1.0]),
      "method"),
    [ExpressionNumber 2.0])
  in
  print_endline (print_expression expr);
  [%expect{| obj(1).method(2) |}]

let%expect_test "deeply nested index access" =
  let expr = ExpressionIndex (
    ExpressionIndex (
      ExpressionVariable "t",
      ExpressionNumber 1.0),
    ExpressionNumber 2.0)
  in
  print_endline (print_expression expr);
  [%expect{| t[1][2] |}]

(* === Table Literal Tests === *)

let%expect_test "empty table" =
  print_endline (print_expression (ExpressionTable []));
  [%expect{| {} |}]

let%expect_test "array table" =
  let expr = ExpressionTable [
    FieldArray (ExpressionNumber 1.0);
    FieldArray (ExpressionNumber 2.0);
    FieldArray (ExpressionNumber 3.0)
  ] in
  print_endline (print_expression expr);
  [%expect{| {1, 2, 3} |}]

let%expect_test "named fields table" =
  let expr = ExpressionTable [
    FieldNamed ("x", ExpressionNumber 10.0);
    FieldNamed ("y", ExpressionNumber 20.0)
  ] in
  print_endline (print_expression expr);
  [%expect{| {x = 10, y = 20} |}]

let%expect_test "indexed fields table" =
  let expr = ExpressionTable [
    FieldIndexed (ExpressionNumber 0.0, ExpressionString "zero");
    FieldIndexed (ExpressionNumber 1.0, ExpressionString "one")
  ] in
  print_endline (print_expression expr);
  [%expect{| {[0] = "zero", [1] = "one"} |}]

let%expect_test "mixed table fields" =
  let expr = ExpressionTable [
    FieldNamed ("tag", ExpressionNumber 1.0);
    FieldNamed ("value", ExpressionString "data")
  ] in
  print_endline (print_expression expr);
  [%expect{| {tag = 1, value = "data"} |}]

(* === Function Expression Tests === *)

let%expect_test "function with no params" =
  let expr = ExpressionFunction ([], [StatementReturn [ExpressionNumber 42.0]]) in
  print_endline (print_expression expr);
  [%expect{|
    function()
      return 42
    end
    |}]

let%expect_test "function with one param" =
  let expr = ExpressionFunction (["x"], [StatementReturn [ExpressionVariable "x"]]) in
  print_endline (print_expression expr);
  [%expect{|
    function(x)
      return x
    end
    |}]

let%expect_test "function with multiple params" =
  let expr = ExpressionFunction (["a"; "b"; "c"],
    [StatementReturn [ExpressionBinaryOp (OpAdd,
      ExpressionBinaryOp (OpAdd, ExpressionVariable "a", ExpressionVariable "b"),
      ExpressionVariable "c")]])
  in
  print_endline (print_expression expr);
  [%expect{|
    function(a, b, c)
      return a + b + c
    end
    |}]

(* === Statement Tests === *)

let%expect_test "local declaration without value" =
  print_endline (print_statement (StatementLocal (["x"], [])));
  [%expect{| local x |}]

let%expect_test "local declaration with value" =
  print_endline (print_statement (StatementLocal (["x"], [ExpressionNumber 42.0])));
  [%expect{| local x = 42 |}]

let%expect_test "multiple local declarations" =
  print_endline (print_statement (StatementLocal (["a"; "b"; "c"],
    [ExpressionNumber 1.0; ExpressionNumber 2.0; ExpressionNumber 3.0])));
  [%expect{| local a, b, c = 1, 2, 3 |}]

let%expect_test "assignment statement" =
  print_endline (print_statement (StatementAssign (
    [LvalueVariable "x"],
    [ExpressionNumber 10.0])));
  [%expect{| x = 10 |}]

let%expect_test "field assignment" =
  print_endline (print_statement (StatementAssign (
    [LvalueField (ExpressionVariable "obj", "field")],
    [ExpressionNumber 5.0])));
  [%expect{| obj.field = 5 |}]

let%expect_test "index assignment" =
  print_endline (print_statement (StatementAssign (
    [LvalueIndex (ExpressionVariable "arr", ExpressionNumber 1.0)],
    [ExpressionString "value"])));
  [%expect{| arr[1] = "value" |}]

let%expect_test "if statement" =
  let stmt = StatementIf (
    [(ExpressionVariable "cond", [StatementReturn [ExpressionNumber 1.0]])],
    Some [StatementReturn [ExpressionNumber 0.0]])
  in
  print_endline (print_statement stmt);
  [%expect{|
    if cond then
      return 1
    else
      return 0
    end
    |}]

let%expect_test "while statement" =
  let stmt = StatementWhile (
    ExpressionBinaryOp (OpGreater, ExpressionVariable "n", ExpressionNumber 0.0),
    [StatementAssign ([LvalueVariable "n"],
      [ExpressionBinaryOp (OpSub, ExpressionVariable "n", ExpressionNumber 1.0)])])
  in
  print_endline (print_statement stmt);
  [%expect{|
    while n > 0 do
      n = n - 1
    end
    |}]

let%expect_test "numeric for statement" =
  let stmt = StatementForNum ("i",
    ExpressionNumber 1.0, ExpressionNumber 10.0, None,
    [StatementCall (ExpressionVariable "print", [ExpressionVariable "i"])])
  in
  print_endline (print_statement stmt);
  [%expect{|
    for i = 1, 10 do
      print(i)
    end
    |}]

let%expect_test "numeric for with step" =
  let stmt = StatementForNum ("i",
    ExpressionNumber 10.0, ExpressionNumber 1.0, Some (ExpressionNumber (-1.0)),
    [StatementCall (ExpressionVariable "print", [ExpressionVariable "i"])])
  in
  print_endline (print_statement stmt);
  [%expect{|
    for i = 10, 1, -1 do
      print(i)
    end
    |}]

let%expect_test "for-in statement" =
  let stmt = StatementForIn (["k"; "v"],
    [ExpressionCall (ExpressionVariable "pairs", [ExpressionVariable "t"])],
    [StatementCall (ExpressionVariable "print", [ExpressionVariable "k"; ExpressionVariable "v"])])
  in
  print_endline (print_statement stmt);
  [%expect{|
    for k, v in pairs(t) do
      print(k, v)
    end
    |}]

let%expect_test "return statement" =
  print_endline (print_statement (StatementReturn [ExpressionNumber 42.0]));
  [%expect{| return 42 |}]

let%expect_test "return multiple values" =
  print_endline (print_statement (StatementReturn [
    ExpressionNumber 1.0;
    ExpressionNumber 2.0;
    ExpressionNumber 3.0]));
  [%expect{| return 1, 2, 3 |}]

let%expect_test "break statement" =
  print_endline (print_statement StatementBreak);
  [%expect{| break |}]

(* === Method Call Tests === *)

let%expect_test "method call" =
  let expr = ExpressionMethodCall (ExpressionVariable "obj", "method",
    [ExpressionNumber 1.0; ExpressionNumber 2.0])
  in
  print_endline (print_expression expr);
  [%expect{| obj:method(1, 2) |}]

let%expect_test "chained method calls" =
  let expr = ExpressionMethodCall (
    ExpressionMethodCall (ExpressionVariable "obj", "first", []),
    "second", [ExpressionNumber 42.0])
  in
  print_endline (print_expression expr);
  [%expect{| obj:first():second(42) |}]

(* === Boolean and Nil Literals === *)

let%expect_test "true literal" =
  print_endline (print_expression (ExpressionBool true));
  [%expect{| true |}]

let%expect_test "false literal" =
  print_endline (print_expression (ExpressionBool false));
  [%expect{| false |}]

let%expect_test "nil literal" =
  print_endline (print_expression ExpressionNil);
  [%expect{| nil |}]

(* === Not Equal Operator === *)

let%expect_test "not equal operator" =
  let expr = ExpressionBinaryOp (OpNotEqual, ExpressionVariable "a", ExpressionVariable "b") in
  print_endline (print_expression expr);
  [%expect{| a ~= b |}]

(* === Concat Operator === *)

let%expect_test "string concat operator" =
  let expr = ExpressionBinaryOp (OpConcat,
    ExpressionString "hello",
    ExpressionString " world")
  in
  print_endline (print_expression expr);
  [%expect{| "hello" .. " world" |}]

(* === Length Operator === *)

let%expect_test "length operator" =
  let expr = ExpressionUnaryOp (OpLength, ExpressionVariable "t") in
  print_endline (print_expression expr);
  [%expect{| #t |}]
