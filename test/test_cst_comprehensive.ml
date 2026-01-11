open Cst

let test_lossless source =
  let root = Cst_parser.parse "test" source in
  let reconstructed = Red_tree.text root in
  if source = reconstructed then
    Printf.printf "PASS: lossless\n"
  else begin
    Printf.printf "FAIL: lossless\n";
    Printf.printf "  original:      |%s|\n" source;
    Printf.printf "  reconstructed: |%s|\n" reconstructed
  end

let test_idempotent source =
  let first = Lina_format.Format_cst.format_string source in
  let second = Lina_format.Format_cst.format_string first in
  if first = second then
    Printf.printf "PASS: idempotent\n"
  else begin
    Printf.printf "FAIL: idempotent\n";
    Printf.printf "  first:  |%s|\n" first;
    Printf.printf "  second: |%s|\n" second
  end

let count_substring haystack needle =
  let rec aux count start =
    match String.index_from_opt haystack start needle.[0] with
    | None -> count
    | Some pos ->
        if String.length haystack - pos >= String.length needle &&
           String.sub haystack pos (String.length needle) = needle then
          aux (count + 1) (pos + 1)
        else
          aux count (pos + 1)
  in
  if needle = "" then 0 else aux 0 0

let test_comments_preserved source =
  let formatted = Lina_format.Format_cst.format_string source in
  let line_count_orig = count_substring source "--" in
  let line_count_fmt = count_substring formatted "--" in
  let block_count_orig = count_substring source "(*" in
  let block_count_fmt = count_substring formatted "(*" in
  if line_count_orig = line_count_fmt && block_count_orig = block_count_fmt then
    Printf.printf "PASS: comments preserved (%d line, %d block)\n"
      line_count_orig block_count_orig
  else begin
    Printf.printf "FAIL: comments not preserved\n";
    Printf.printf "  original: %d line, %d block\n" line_count_orig block_count_orig;
    Printf.printf "  formatted: %d line, %d block\n" line_count_fmt block_count_fmt
  end

let%expect_test "expr - simple let" =
  test_lossless "let x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - integer literal" =
  test_lossless "let x = 42";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - string literal" =
  test_lossless "let s = \"hello\"";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - boolean true" =
  test_lossless "let b = true";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - boolean false" =
  test_lossless "let b = false";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - tuple" =
  test_lossless "let t = (1, 2)";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - constructor None" =
  test_lossless "let x = None";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - constructor Some" =
  test_lossless "let x = Some 42";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - record simple" =
  test_lossless "let r = { x = 1 }";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - record two fields" =
  test_lossless "let r = { x = 1; y = 2 }";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - function application" =
  test_lossless "let y = f x";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - nested application" =
  test_lossless "let z = f x y";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - infix plus" =
  test_lossless "let sum = a + b";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - infix minus" =
  test_lossless "let diff = a - b";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - infix times" =
  test_lossless "let prod = a * b";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - parenthesized" =
  test_lossless "let x = (42)";
  [%expect {| PASS: lossless |}]

let%expect_test "expr - nested parens" =
  test_lossless "let x = ((42))";
  [%expect {| PASS: lossless |}]

let%expect_test "pattern - variable" =
  test_lossless "let x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "pattern - wildcard" =
  test_lossless "let _ = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "pattern - tuple" =
  test_lossless "let (a, b) = pair";
  [%expect {| PASS: lossless |}]

let%expect_test "func - one param" =
  test_lossless "let f x = x";
  [%expect {| PASS: lossless |}]

let%expect_test "func - two params" =
  test_lossless "let f x y = x";
  [%expect {| PASS: lossless |}]

let%expect_test "func - recursive" =
  test_lossless "let rec f x = f x";
  [%expect {| PASS: lossless |}]

let%expect_test "func - lambda" =
  test_lossless "let f = fun x -> x";
  [%expect {| PASS: lossless |}]

let%expect_test "control - if then else" =
  test_lossless "let x = if true then 1 else 0";
  [%expect {| PASS: lossless |}]

let%expect_test "control - match simple" =
  test_lossless "let x = match y with z -> z";
  [%expect {| PASS: lossless |}]

let%expect_test "control - match two arms" =
  test_lossless "let x = match y with a -> 1 | b -> 2";
  [%expect {| PASS: lossless |}]

let%expect_test "typedef - alias" =
  test_lossless "type t = int";
  [%expect {| PASS: lossless |}]

let%expect_test "typedef - variant" =
  test_lossless "type t = A | B";
  [%expect {| PASS: lossless |}]

let%expect_test "typedef - variant with args" =
  test_lossless "type t = A of int | B";
  [%expect {| PASS: lossless |}]

let%expect_test "typedef - record" =
  test_lossless "type t = { x : int }";
  [%expect {| PASS: lossless |}]

let%expect_test "module - simple" =
  test_lossless "module M = struct end";
  [%expect {| PASS: lossless |}]

let%expect_test "module - with let" =
  test_lossless "module M = struct let x = 1 end";
  [%expect {| PASS: lossless |}]

let%expect_test "module - open" =
  test_lossless "open M";
  [%expect {| PASS: lossless |}]

let%expect_test "module - signature" =
  test_lossless "module type S = sig end";
  [%expect {| PASS: lossless |}]

let%expect_test "module - signature with val" =
  test_lossless "module type S = sig val x : int end";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - line before" =
  test_lossless "-- comment\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - line after" =
  test_lossless "let x = 1 -- comment";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - block" =
  test_lossless "(* comment *) let x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - nested block" =
  test_lossless "(* outer (* inner *) outer *) let x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - multiple lines" =
  test_lossless "-- c1\n-- c2\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - between defs" =
  test_lossless "let x = 1\n-- between\nlet y = 2";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - line with trailing space" =
  test_lossless "-- comment with trailing space \nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - line at end of file" =
  test_lossless "let x = 1\n-- final comment";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - line then block" =
  test_lossless "-- line\n(* block *)\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - block then line" =
  test_lossless "(* block *)\n-- line\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - trailing then leading" =
  test_lossless "let x = 1 -- trailing\n-- leading\nlet y = 2";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - three consecutive lines" =
  test_lossless "-- a\n-- b\n-- c\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - in module" =
  test_lossless "module M = struct\n-- inside\nlet x = 1\nend";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - after match arm" =
  test_lossless "let x = match y with\n| a -> 1 -- arm1\n| b -> 2";
  [%expect {| PASS: lossless |}]

let%expect_test "comment - empty line comment" =
  test_lossless "--\nlet x = 1";
  [%expect {| PASS: lossless |}]

let%expect_test "idempotent - simple" =
  test_idempotent "let x = 1";
  [%expect {| PASS: idempotent |}]

let%expect_test "idempotent - with comment" =
  test_idempotent "let x = 1 -- comment";
  [%expect {| PASS: idempotent |}]

let%expect_test "idempotent - function" =
  test_idempotent "let f x = x + 1";
  [%expect {| PASS: idempotent |}]

let%expect_test "idempotent - line comment before" =
  test_idempotent "-- comment\nlet x = 1";
  [%expect {| PASS: idempotent |}]

let%expect_test "idempotent - multiple line comments" =
  test_idempotent "-- a\n-- b\nlet x = 1";
  [%expect {| PASS: idempotent |}]

let%expect_test "idempotent - comment between defs" =
  test_idempotent "let x = 1\n-- between\nlet y = 2";
  [%expect {| PASS: idempotent |}]

let%expect_test "preserve - line comment" =
  test_comments_preserved "let x = 1 -- comment";
  [%expect {| PASS: comments preserved (1 line, 0 block) |}]

let%expect_test "preserve - block comment" =
  test_comments_preserved "(* block *) let x = 1";
  [%expect {| PASS: comments preserved (0 line, 1 block) |}]

let%expect_test "preserve - mixed" =
  test_comments_preserved "-- line\n(* block *)\nlet x = 1";
  [%expect {| PASS: comments preserved (1 line, 1 block) |}]

let%expect_test "nav - root offset" =
  let root = Cst_parser.parse "test" "let x = 1" in
  let span = Red_tree.span root in
  Printf.printf "offset: %d, length: %d\n" span.start span.length;
  [%expect {| offset: 0, length: 9 |}]

let%expect_test "nav - parent pointer" =
  let root = Cst_parser.parse "test" "let x = 1" in
  let has_parent = Option.is_some root.parent in
  Printf.printf "root has parent: %b\n" has_parent;
  [%expect {| root has parent: false |}]

let%expect_test "nav - children exist" =
  let root = Cst_parser.parse "test" "let x = 1" in
  let count = List.length (Red_tree.children root) in
  Printf.printf "child count > 0: %b\n" (count > 0);
  [%expect {| child count > 0: true |}]

let%expect_test "edge - single number" =
  test_lossless "42";
  [%expect {| PASS: lossless |}]

let%expect_test "edge - single identifier" =
  test_lossless "x";
  [%expect {| PASS: lossless |}]

let%expect_test "edge - unit" =
  test_lossless "let x = ()";
  [%expect {| PASS: lossless |}]
