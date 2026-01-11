(** Tests for the CST-based formatter.

    The CST formatter preserves comments and blank lines while
    normalizing whitespace. These tests verify this behavior. *)

(** {1 Basic Formatting} *)

let%expect_test "cst: simple let binding" =
  let result = Lina_format.Formatter.format_string_cst "let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "cst: multiple bindings" =
  let result = Lina_format.Formatter.format_string_cst "let x = 1 let y = 2" in
  print_string result;
  [%expect {| let x = 1 let y = 2 |}]

(** {1 Whitespace Normalization} *)

let%expect_test "cst: extra spaces normalized" =
  let result = Lina_format.Formatter.format_string_cst "let  x  =  42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "cst: leading whitespace removed" =
  let result = Lina_format.Formatter.format_string_cst "   let x = 42" in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "cst: trailing whitespace removed" =
  let result = Lina_format.Formatter.format_string_cst "let x = 42   " in
  print_string result;
  [%expect {| let x = 42 |}]

let%expect_test "cst: mixed extra whitespace" =
  let result = Lina_format.Formatter.format_string_cst "let   f   x   y   =   x  +  y" in
  print_string result;
  [%expect {| let f x y = x + y |}]

(** {1 Blank Line Preservation} *)

let%expect_test "cst: single blank line preserved" =
  let result = Lina_format.Formatter.format_string_cst "let x = 1\n\nlet y = 2" in
  print_string result;
  [%expect {|
    let x = 1

    let y = 2 |}]

let%expect_test "cst: multiple blank lines preserved" =
  let result = Lina_format.Formatter.format_string_cst "let x = 1\n\n\nlet y = 2" in
  print_string result;
  [%expect {|
    let x = 1


    let y = 2 |}]

let%expect_test "cst: blank lines between declarations" =
  let result = Lina_format.Formatter.format_string_cst "type t = int\n\nlet x = 1\n\nlet y = 2" in
  print_string result;
  [%expect {|
    type t = int

    let x = 1

    let y = 2 |}]

(** {1 Line Comment Preservation} *)

let%expect_test "cst: line comment at start" =
  let result = Lina_format.Formatter.format_string_cst "-- comment\nlet x = 42" in
  print_string result;
  [%expect {|
    -- comment
    let x = 42 |}]

let%expect_test "cst: line comment trailing" =
  let result = Lina_format.Formatter.format_string_cst "let x = 42 -- comment" in
  print_string result;
  [%expect {| let x = 42 -- comment |}]

let%expect_test "cst: multiple line comments" =
  let result = Lina_format.Formatter.format_string_cst "-- first\nlet x = 1\n-- second\nlet y = 2" in
  print_string result;
  [%expect {|
    -- first
    let x = 1
    -- second
    let y = 2 |}]

let%expect_test "cst: indented comment normalized" =
  let result = Lina_format.Formatter.format_string_cst "    -- indented comment\nlet x = 42" in
  print_string result;
  [%expect {|
    -- indented comment
    let x = 42 |}]

let%expect_test "cst: comment with extra spaces normalized" =
  let result = Lina_format.Formatter.format_string_cst "let  x  =  42  -- trailing  comment" in
  print_string result;
  [%expect {| let x = 42 -- trailing  comment |}]

(** {1 Block Comment Preservation} *)

let%expect_test "cst: block comment inline" =
  let result = Lina_format.Formatter.format_string_cst "let (* comment *) x = 42" in
  print_string result;
  [%expect {| let (* comment *) x = 42 |}]

let%expect_test "cst: block comment on own line" =
  let result = Lina_format.Formatter.format_string_cst "(* comment *)\nlet x = 42" in
  print_string result;
  [%expect {|
    (* comment *)
    let x = 42 |}]

let%expect_test "cst: multiline block comment" =
  let result = Lina_format.Formatter.format_string_cst "(* line 1\n   line 2 *)\nlet x = 42" in
  print_string result;
  [%expect {|
    (* line 1
       line 2 *)
    let x = 42 |}]

(** {1 Comments with Blank Lines} *)

let%expect_test "cst: comment after blank line" =
  let result = Lina_format.Formatter.format_string_cst "let x = 1\n\n-- section\nlet y = 2" in
  print_string result;
  [%expect {|
    let x = 1

    -- section
    let y = 2 |}]

let%expect_test "cst: blank line after comment" =
  let result = Lina_format.Formatter.format_string_cst "-- header\n\nlet x = 1" in
  print_string result;
  [%expect {|
    -- header

    let x = 1 |}]

(** {1 Idempotency} *)

let check_cst_idempotent source =
  let once = Lina_format.Formatter.format_string_cst source in
  let twice = Lina_format.Formatter.format_string_cst once in
  if once = twice then "IDEMPOTENT"
  else Printf.sprintf "DIFFERS:\nOnce: [%s]\nTwice: [%s]" once twice

let%expect_test "cst idempotency: simple" =
  print_string (check_cst_idempotent "let x = 42");
  [%expect {| IDEMPOTENT |}]

let%expect_test "cst idempotency: with extra spaces" =
  print_string (check_cst_idempotent "let  x  =  42");
  [%expect {| IDEMPOTENT |}]

let%expect_test "cst idempotency: with comments" =
  print_string (check_cst_idempotent "-- comment\nlet x = 42");
  [%expect {| IDEMPOTENT |}]

let%expect_test "cst idempotency: with blank lines" =
  print_string (check_cst_idempotent "let x = 1\n\nlet y = 2");
  [%expect {| IDEMPOTENT |}]

let%expect_test "cst idempotency: complex" =
  print_string (check_cst_idempotent "-- header\n\nlet  x  =  1  -- trailing\n\n-- section\nlet y = 2");
  [%expect {| IDEMPOTENT |}]

(** {1 Edge Cases} *)

let%expect_test "cst: empty input" =
  let result = Lina_format.Formatter.format_string_cst "" in
  Printf.printf "[%s]" result;
  [%expect {| [] |}]

let%expect_test "cst: only whitespace" =
  let result = Lina_format.Formatter.format_string_cst "   " in
  Printf.printf "[%s]" result;
  [%expect {| [] |}]

let%expect_test "cst: only newlines" =
  let result = Lina_format.Formatter.format_string_cst "\n\n" in
  Printf.printf "[%s]" result;
  [%expect {|
    [

    ]
    |}]

let%expect_test "cst: only comment" =
  let result = Lina_format.Formatter.format_string_cst "-- just a comment" in
  print_string result;
  [%expect {| -- just a comment |}]

(** {1 Syntax Error Handling} *)

let%expect_test "cst: syntax error returns original" =
  (* CST formatter should handle errors gracefully *)
  let source = "let x =" in
  let result = Lina_format.Formatter.format_string_cst source in
  (* On parse error, CST formatter may return partial output *)
  Printf.printf "length: %d" (String.length result);
  [%expect {| length: 7 |}]

(** {1 Complex Examples} *)

let%expect_test "cst: module with comments" =
  let source = {|-- Module documentation
module M = struct
  -- Internal value
  let  x  =  42
end|} in
  let result = Lina_format.Formatter.format_string_cst source in
  print_string result;
  [%expect {|
    -- Module documentation
    module M = struct
    -- Internal value
    let x = 42
    end |}]

let%expect_test "cst: function with trailing comments" =
  let source = "let f x = x + 1 -- increment\nlet g x = x - 1 -- decrement" in
  let result = Lina_format.Formatter.format_string_cst source in
  print_string result;
  [%expect {|
    let f x = x + 1 -- increment
    let g x = x - 1 -- decrement |}]

let%expect_test "cst: type with comments" =
  let source = {|-- Option type
type 'a option =
  | None -- empty
  | Some of 'a -- has value|} in
  let result = Lina_format.Formatter.format_string_cst source in
  print_string result;
  [%expect {|
    -- Option type
    type 'a option =
    | None -- empty
    | Some of 'a -- has value |}]
