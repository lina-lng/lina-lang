(** Tests for format_pattern.ml - pattern formatting with semantic awareness. *)

(** {1 Helper Functions} *)

let format source =
  Lina_format.Formatter.format_string_cst source

let check_idempotent source =
  let once = format source in
  let twice = format once in
  if once = twice then "IDEMPOTENT"
  else Printf.sprintf "DIFFERS:\nOnce: [%s]\nTwice: [%s]" once twice

(** {1 Variable Pattern Formatting} *)

let%expect_test "variable pattern: simple" =
  let result = format "let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "variable pattern: in function" =
  let result = format "let f x = x" in
  print_string result;
  [%expect {| let f x = x |}]

let%expect_test "variable pattern: in match" =
  let result = format "match x with | y -> y" in
  print_string result;
  [%expect {| match x with | y -> y |}]

(** {1 Wildcard Pattern Formatting} *)

let%expect_test "wildcard pattern: simple" =
  let result = format "let _ = 42" in
  print_string result;
  [%expect {| let _ = 42 |}]

let%expect_test "wildcard pattern: in match" =
  let result = format "match x with | _ -> default" in
  print_string result;
  [%expect {| match x with | _ -> default |}]

(** {1 Constant Pattern Formatting} *)

let%expect_test "constant pattern: integer" =
  let result = format "match x with | 0 -> zero | 1 -> one" in
  print_string result;
  [%expect {| match x with | 0 -> zero | 1 -> one |}]

let%expect_test "constant pattern: string" =
  let result = format {|match s with | "hello" -> greeting|} in
  print_string result;
  [%expect {| match s with | "hello" -> greeting |}]

let%expect_test "constant pattern: boolean" =
  let result = format "match b with | true -> yes | false -> no" in
  print_string result;
  [%expect {| match b with | true -> yes | false -> no |}]

(** {1 Tuple Pattern Formatting} *)

let%expect_test "tuple pattern: pair" =
  let result = format "let (a, b) = pair" in
  print_string result;
  [%expect {| let (a, b) = pair |}]

let%expect_test "tuple pattern: triple" =
  let result = format "let (a, b, c) = triple" in
  print_string result;
  [%expect {| let (a, b, c) = triple |}]

let%expect_test "tuple pattern: nested" =
  let result = format "let ((a, b), (c, d)) = nested" in
  print_string result;
  [%expect {| let ((a, b), (c, d)) = nested |}]

let%expect_test "tuple pattern: in match" =
  let result = format "match pair with | (x, y) -> x + y" in
  print_string result;
  [%expect {| match pair with | (x, y) -> x + y |}]

let%expect_test "tuple pattern: with wildcards" =
  let result = format "let (_, y, _) = triple" in
  print_string result;
  [%expect {| let (_, y, _) = triple |}]

(** {1 Constructor Pattern Formatting} *)

let%expect_test "constructor pattern: without argument" =
  let result = format "match opt with | None -> default" in
  print_string result;
  [%expect {| match opt with | None -> default |}]

let%expect_test "constructor pattern: with argument" =
  let result = format "match opt with | Some x -> x" in
  print_string result;
  [%expect {| match opt with | Some x -> x |}]

let%expect_test "constructor pattern: with tuple argument" =
  let result = format "match pair with | Pair (a, b) -> a + b" in
  print_string result;
  [%expect {| match pair with | Pair (a, b) -> a + b |}]

let%expect_test "constructor pattern: multiple constructors" =
  let result = format "match result with | Ok x -> x | Error e -> default" in
  print_string result;
  [%expect {| match result with | Ok x -> x | Error e -> default |}]

let%expect_test "constructor pattern: nested" =
  let result = format "match x with | Some (Some y) -> y | _ -> default" in
  print_string result;
  [%expect {| match x with | Some (Some y) -> y | _ -> default |}]

(** {1 Record Pattern Formatting} *)

let%expect_test "record pattern: simple" =
  let result = format "let { x; y } = point" in
  print_string result;
  [%expect {| let { x; y } = point |}]

let%expect_test "record pattern: with renaming" =
  let result = format "let { x = a; y = b } = point" in
  print_string result;
  [%expect {| let { x = a; y = b } = point |}]

let%expect_test "record pattern: in match" =
  let result = format "match p with | { x; y } -> x + y" in
  print_string result;
  [%expect {| match p with | { x; y } -> x + y |}]

let%expect_test "record pattern: partial" =
  let result = format "let { x } = point" in
  print_string result;
  [%expect {| let { x } = point |}]

(** {1 Alias Pattern Formatting} *)

let%expect_test "alias pattern: simple" =
  let result = format "match opt with | Some _ as x -> use x" in
  print_string result;
  [%expect {| match opt with | Some _ as x -> use x |}]

let%expect_test "alias pattern: with constructor" =
  let result = format "match list with | Cons (h, t) as whole -> whole" in
  print_string result;
  [%expect {| match list with | Cons (h, t) as whole -> whole |}]

(** {1 Constraint Pattern Formatting} *)

let%expect_test "constraint pattern: simple" =
  let result = format "let (x : int) = 42" in
  print_string result;
  [%expect {| let (x : int) = 42 |}]

let%expect_test "constraint pattern: in function" =
  let result = format "fun (x : int) -> x + 1" in
  print_string result;
  [%expect {| fun (x : int) -> x + 1 |}]

(** {1 Parenthesized Pattern Formatting} *)

let%expect_test "paren pattern: simple" =
  let result = format "let (x) = 42" in
  print_string result;
  [%expect {| let (x) = 42 |}]

let%expect_test "paren pattern: with constraint" =
  let result = format "fun ((x : int)) -> x" in
  print_string result;
  [%expect {| fun ((x : int)) -> x |}]

(** {1 Complex Pattern Combinations} *)

let%expect_test "complex pattern: tuple of constructors" =
  let result = format "match pair with | (Some a, Some b) -> a + b | _ -> 0" in
  print_string result;
  [%expect {| match pair with | (Some a, Some b) -> a + b | _ -> 0 |}]

let%expect_test "complex pattern: record with nested tuple" =
  let result = format "let { pos = (x, y) } = entity" in
  print_string result;
  [%expect {| let { pos = (x, y) } = entity |}]

let%expect_test "complex pattern: constructor with record" =
  let result = format "match x with | Node { left; right } -> combine left right" in
  print_string result;
  [%expect {| match x with | Node { left; right } -> combine left right |}]

(** {1 Whitespace Normalization} *)

let%expect_test "pattern whitespace: extra spaces normalized" =
  let result = format "let  (  x  ,  y  )  = pair" in
  print_string result;
  [%expect {| let ( x , y ) = pair |}]

let%expect_test "pattern whitespace: constructor with spaces" =
  let result = format "match x with |  Some  y  ->  y" in
  print_string result;
  [%expect {| match x with |  Some  y  ->  y |}]

(** {1 Idempotency Tests} *)

let%expect_test "pattern idempotency: tuple pattern" =
  print_string (check_idempotent "let (a, b, c) = triple");
  [%expect {| IDEMPOTENT |}]

let%expect_test "pattern idempotency: constructor pattern" =
  print_string (check_idempotent "match opt with | Some x -> x | None -> 0");
  [%expect {| IDEMPOTENT |}]

let%expect_test "pattern idempotency: record pattern" =
  print_string (check_idempotent "let { x; y } = point");
  [%expect {| IDEMPOTENT |}]

let%expect_test "pattern idempotency: alias pattern" =
  print_string (check_idempotent "match x with | Some y as opt -> opt");
  [%expect {| IDEMPOTENT |}]

let%expect_test "pattern idempotency: complex pattern" =
  print_string (check_idempotent "match x with | (Some a, { x; y }) -> a + x + y | _ -> 0");
  [%expect {| IDEMPOTENT |}]
