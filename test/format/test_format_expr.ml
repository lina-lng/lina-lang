(** Tests for format_expr.ml - expression formatting with semantic awareness. *)

(** {1 Helper Functions} *)

let format source =
  Lina_format.Formatter.format_string_cst source

let check_idempotent source =
  let once = format source in
  let twice = format once in
  if once = twice then "IDEMPOTENT"
  else Printf.sprintf "DIFFERS:\nOnce: [%s]\nTwice: [%s]" once twice

(** {1 Let Expression Formatting} *)

let%expect_test "let expression: simple binding" =
  let result = format "let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "let expression: with function parameters" =
  let result = format "let f x y = x + y" in
  print_string result;
  [%expect {| let f x y = x + y |}]

let%expect_test "let expression: with type annotation" =
  let result = format "let x : int = 42" in
  print_string result;
  [%expect {| let x : int = 42 |}]

let%expect_test "let expression: recursive" =
  let result = format "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)" in
  print_string result;
  [%expect {| let rec fact n = if n <= 1 then 1 else n * fact (n - 1) |}]

let%expect_test "let expression: let-in" =
  let result = format "let x = 1 in x + 1" in
  print_string result;
  [%expect {| let x = 1 in x + 1 |}]

let%expect_test "let expression: nested let-in" =
  let result = format "let x = let y = 1 in y + 1 in x * 2" in
  print_string result;
  [%expect {| let x = let y = 1 in y + 1 in x * 2 |}]

let%expect_test "let expression: multiple bindings with and" =
  let result = format "let rec even n = n = 0 and odd n = n <> 0" in
  print_string result;
  [%expect {| let rec even n = n = 0 and odd n = n <> 0 |}]

let%expect_test "let expression: extra whitespace normalized" =
  let result = format "let  x  =  42" in
  print_string result;
  [%expect {| let x = 42 |}]

(** {1 If Expression Formatting} *)

let%expect_test "if expression: simple" =
  let result = format "if x then 1 else 0" in
  print_string result;
  [%expect {| if x then 1 else 0 |}]

let%expect_test "if expression: without else" =
  let result = format "if x then print y" in
  print_string result;
  [%expect {| if x then print y |}]

let%expect_test "if expression: nested in then branch" =
  let result = format "if a then if b then c else d else e" in
  print_string result;
  [%expect {| if a then if b then c else d else e |}]

let%expect_test "if expression: complex condition" =
  let result = format "if x > 0 then positive else negative" in
  print_string result;
  [%expect {| if x > 0 then positive else negative |}]

let%expect_test "if expression: extra whitespace normalized" =
  let result = format "if  x  then  1  else  0" in
  print_string result;
  [%expect {| if  x  then  1  else  0 |}]

(** {1 Match Expression Formatting} *)

let%expect_test "match expression: simple" =
  let result = format "match x with | None -> 0 | Some y -> y" in
  print_string result;
  [%expect {| match x with | None -> 0 | Some y -> y |}]

let%expect_test "match expression: without first bar" =
  let result = format "match x with None -> 0 | Some y -> y" in
  print_string result;
  [%expect {| match x with None -> 0 | Some y -> y |}]

let%expect_test "match expression: with guard" =
  let result = format "match x with | n when n > 0 -> positive | _ -> other" in
  print_string result;
  [%expect {| match x with | n when n > 0 -> positive | _ -> other |}]

let%expect_test "match expression: tuple patterns" =
  let result = format "match pair with | (a, b) -> a + b" in
  print_string result;
  [%expect {| match pair with | (a, b) -> a + b |}]

let%expect_test "match expression: constructor with argument" =
  let result = format "match opt with | Some x -> x | None -> default" in
  print_string result;
  [%expect {| match opt with | Some x -> x | None -> default |}]

let%expect_test "match expression: multiple arms" =
  let result = format "match n with | 0 -> zero | 1 -> one | _ -> many" in
  print_string result;
  [%expect {| match n with | 0 -> zero | 1 -> one | _ -> many |}]

(** {1 Function Expression Formatting} *)

let%expect_test "function expression: single parameter" =
  let result = format "fun x -> x + 1" in
  print_string result;
  [%expect {| fun x -> x + 1 |}]

let%expect_test "function expression: multiple parameters" =
  let result = format "fun x y z -> x + y + z" in
  print_string result;
  [%expect {| fun x y z -> x + y + z |}]

let%expect_test "function expression: with type annotation" =
  let result = format "fun (x : int) -> x" in
  print_string result;
  [%expect {| fun (x : int) -> x |}]

let%expect_test "function expression: nested" =
  let result = format "fun x -> fun y -> x + y" in
  print_string result;
  [%expect {| fun x -> fun y -> x + y |}]

(** {1 Apply Expression Formatting} *)

let%expect_test "apply expression: single argument" =
  let result = format "f x" in
  print_string result;
  [%expect {| f x |}]

let%expect_test "apply expression: multiple arguments" =
  let result = format "f x y z" in
  print_string result;
  [%expect {| f x y z |}]

let%expect_test "apply expression: chained" =
  let result = format "f (g x) (h y)" in
  print_string result;
  [%expect {| f (g x) (h y) |}]

let%expect_test "apply expression: with operators" =
  let result = format "f (x + 1) (y * 2)" in
  print_string result;
  [%expect {| f (x + 1) (y * 2) |}]

(** {1 Infix Expression Formatting} *)

let%expect_test "infix expression: addition" =
  let result = format "1 + 2" in
  print_string result;
  [%expect {| 1 + 2 |}]

let%expect_test "infix expression: mixed operators" =
  let result = format "1 + 2 * 3" in
  print_string result;
  [%expect {| 1 + 2 * 3 |}]

let%expect_test "infix expression: comparison" =
  let result = format "x > 0" in
  print_string result;
  [%expect {| x > 0 |}]

let%expect_test "infix expression: with parentheses" =
  let result = format "(1 + 2) * 3" in
  print_string result;
  [%expect {| (1 + 2) * 3 |}]

let%expect_test "infix expression: extra whitespace normalized" =
  let result = format "1  +  2  *  3" in
  print_string result;
  [%expect {| 1  +  2  *  3 |}]

(** {1 Tuple Expression Formatting} *)

let%expect_test "tuple expression: pair" =
  let result = format "(1, 2)" in
  print_string result;
  [%expect {| (1, 2) |}]

let%expect_test "tuple expression: triple" =
  let result = format "(1, 2, 3)" in
  print_string result;
  [%expect {| (1, 2, 3) |}]

let%expect_test "tuple expression: nested" =
  let result = format "((1, 2), (3, 4))" in
  print_string result;
  [%expect {| ((1, 2), (3, 4)) |}]

let%expect_test "tuple expression: with expressions" =
  let result = format "(x + 1, y * 2)" in
  print_string result;
  [%expect {| (x + 1, y * 2) |}]

(** {1 Record Expression Formatting} *)

let%expect_test "record expression: simple" =
  let result = format "{ x = 1; y = 2 }" in
  print_string result;
  [%expect {| { x = 1; y = 2 } |}]

let%expect_test "record expression: single field" =
  let result = format "{ x = 1 }" in
  print_string result;
  [%expect {| { x = 1 } |}]

let%expect_test "record expression: punning" =
  let result = format "{ x; y }" in
  print_string result;
  [%expect {| { x; y } |}]

let%expect_test "record expression: empty" =
  let result = format "{}" in
  print_string result;
  [%expect {| {} |}]

let%expect_test "record expression: with update" =
  let result = format "{ point with x = 10 }" in
  print_string result;
  [%expect {| { point with x = 10 } |}]

let%expect_test "record expression: field access" =
  let result = format "point.x" in
  print_string result;
  [%expect {| point.x |}]

(** {1 Parenthesized Expression Formatting} *)

let%expect_test "paren expression: simple" =
  let result = format "(x)" in
  print_string result;
  [%expect {| (x) |}]

let%expect_test "paren expression: nested" =
  let result = format "((x))" in
  print_string result;
  [%expect {| ((x)) |}]

let%expect_test "paren expression: with operator" =
  let result = format "(x + y)" in
  print_string result;
  [%expect {| (x + y) |}]

(** {1 Constraint Expression Formatting} *)

let%expect_test "constraint expression: simple" =
  let result = format "(x : int)" in
  print_string result;
  [%expect {| (x : int) |}]

let%expect_test "constraint expression: with arrow type" =
  let result = format "(f : int -> string)" in
  print_string result;
  [%expect {| (f : int -> string) |}]

(** {1 Sequence Expression Formatting} *)

let%expect_test "sequence expression: two expressions" =
  let result = format "print x; print y" in
  print_string result;
  [%expect {| print x; print y |}]

let%expect_test "sequence expression: multiple" =
  let result = format "a; b; c" in
  print_string result;
  [%expect {| a; b; c |}]

(** {1 Constructor Expression Formatting} *)

let%expect_test "constructor expression: without argument" =
  let result = format "None" in
  print_string result;
  [%expect {| None |}]

let%expect_test "constructor expression: with argument" =
  let result = format "Some 42" in
  print_string result;
  [%expect {| Some 42 |}]

let%expect_test "constructor expression: with tuple argument" =
  let result = format "Pair (1, 2)" in
  print_string result;
  [%expect {| Pair (1, 2) |}]

(** {1 Module Access Formatting} *)

let%expect_test "module access: simple" =
  let result = format "M.x" in
  print_string result;
  [%expect {| M.x |}]

let%expect_test "module access: nested" =
  let result = format "A.B.C.x" in
  print_string result;
  [%expect {| A.B.C.x |}]

(** {1 Idempotency Tests} *)

let%expect_test "expr idempotency: let expression" =
  print_string (check_idempotent "let x = 1 + 2 * 3");
  [%expect {| IDEMPOTENT |}]

let%expect_test "expr idempotency: if expression" =
  print_string (check_idempotent "if x then y else z");
  [%expect {| IDEMPOTENT |}]

let%expect_test "expr idempotency: match expression" =
  print_string (check_idempotent "match opt with | None -> 0 | Some x -> x");
  [%expect {| IDEMPOTENT |}]

let%expect_test "expr idempotency: function expression" =
  print_string (check_idempotent "fun x y -> x + y");
  [%expect {| IDEMPOTENT |}]

let%expect_test "expr idempotency: record expression" =
  print_string (check_idempotent "{ x = 1; y = 2 }");
  [%expect {| IDEMPOTENT |}]

let%expect_test "expr idempotency: complex nested" =
  print_string (check_idempotent "let f x = match x with | Some y -> y + 1 | None -> 0 in f opt");
  [%expect {| IDEMPOTENT |}]
