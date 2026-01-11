(** Tests for the Doc IR and layout algorithm.

    These tests verify the core document combinators and the Wadler-Lindig
    layout algorithm that powers the formatter. *)

open Lina_format.Doc

(** {1 Basic Constructors} *)

let%expect_test "empty renders as empty string" =
  print_string (render ~width:80 empty);
  [%expect {| |}]

let%expect_test "text renders literal string" =
  print_string (render ~width:80 (text "hello"));
  [%expect {| hello |}]

let%expect_test "text with empty string returns empty" =
  print_string (render ~width:80 (text ""));
  [%expect {| |}]

let%expect_test "text with newline raises Invalid_argument" =
  (try
     let _ = text "hello\nworld" in
     print_string "no exception"
   with Invalid_argument message ->
     print_string ("Invalid_argument: " ^ message));
  [%expect {| Invalid_argument: Doc.text: string must not contain newlines |}]

(** {1 Break Modes} *)

let%expect_test "space in flat mode renders as single space" =
  print_string (render ~width:80 (group (text "a" ^^ space ^^ text "b")));
  [%expect {| a b |}]

let%expect_test "space in broken mode renders as newline with indent" =
  print_string (render ~width:5 (group (text "hello" ^^ space ^^ text "world")));
  [%expect {|
    hello
    world |}]

let%expect_test "softline in flat mode renders as nothing" =
  print_string (render ~width:80 (group (text "a" ^^ softline ^^ text "b")));
  [%expect {| ab |}]

let%expect_test "softline in broken mode renders as newline" =
  print_string (render ~width:5 (group (text "hello" ^^ softline ^^ text "world")));
  [%expect {|
    hello
    world |}]

let%expect_test "hardline always renders as newline" =
  print_string (render ~width:80 (text "a" ^^ hardline ^^ text "b"));
  [%expect {|
    a
    b |}]

let%expect_test "hardline inside group still breaks" =
  print_string (render ~width:80 (group (text "a" ^^ hardline ^^ text "b")));
  [%expect {|
    a
    b |}]

(** {1 Concatenation Operators} *)

let%expect_test "^^ concatenates without space" =
  print_string (render ~width:80 (text "hello" ^^ text "world"));
  [%expect {| helloworld |}]

let%expect_test "^/^ concatenates with space" =
  print_string (render ~width:80 (text "hello" ^/^ text "world"));
  [%expect {| hello world |}]

let%expect_test "^/^ with empty left operand" =
  print_string (render ~width:80 (empty ^/^ text "world"));
  [%expect {| world |}]

let%expect_test "^/^ with empty right operand" =
  print_string (render ~width:80 (text "hello" ^/^ empty));
  [%expect {| hello |}]

let%expect_test "^//^ concatenates with softline" =
  print_string (render ~width:80 (group (text "a" ^//^ text "b")));
  [%expect {| ab |}]

let%expect_test "^//^ breaks when needed" =
  print_string (render ~width:5 (group (text "hello" ^//^ text "world")));
  [%expect {|
    hello
    world |}]

let%expect_test "^|^ concatenates with hardline" =
  print_string (render ~width:80 (text "a" ^|^ text "b"));
  [%expect {|
    a
    b |}]

let%expect_test "^|^ with empty operands" =
  print_string (render ~width:80 (empty ^|^ text "b"));
  [%expect {| b |}]

(** {1 Nesting and Indentation} *)

let%expect_test "nest adds indentation on line breaks" =
  let doc = group (text "let" ^^ nest 2 (space ^^ text "x" ^^ space ^^ text "=" ^^ space ^^ text "1")) in
  print_string (render ~width:10 doc);
  [%expect {| let x = 1 |}]

let%expect_test "nest with zero does not change indentation" =
  let doc = group (text "let" ^^ nest 0 (space ^^ text "x")) in
  print_string (render ~width:5 doc);
  [%expect {| let x |}]

let%expect_test "nested nest accumulates indentation" =
  let doc = group (
    text "a" ^^
    nest 2 (space ^^ text "b" ^^
      nest 2 (space ^^ text "c"))
  ) in
  print_string (render ~width:3 doc);
  [%expect {|
    a
      b
        c |}]

let%expect_test "indent uses default width of 2" =
  let doc = group (text "a" ^^ indent (space ^^ text "b")) in
  print_string (render ~width:3 doc);
  [%expect {| a b |}]

(** {1 Group and Layout Choice} *)

let%expect_test "group that fits stays flat" =
  let doc = group (text "a" ^/^ text "b" ^/^ text "c") in
  print_string (render ~width:80 doc);
  [%expect {| a b c |}]

let%expect_test "group that does not fit breaks" =
  let doc = group (text "hello" ^/^ text "world" ^/^ text "test") in
  print_string (render ~width:10 doc);
  [%expect {|
    hello
    world
    test |}]

let%expect_test "nested groups optimize away" =
  let doc = group (group (text "a" ^/^ text "b")) in
  print_string (render ~width:80 doc);
  [%expect {| a b |}]

let%expect_test "inner group can break while outer stays flat" =
  let inner = group (text "long_function_name" ^/^ text "argument") in
  let doc = group (text "let x =" ^/^ inner) in
  print_string (render ~width:30 doc);
  [%expect {|
    let x =
    long_function_name argument |}]

(** {1 Align} *)

let%expect_test "align sets indentation to current column" =
  let doc = group (
    text "func(" ^^
    align (text "arg1," ^^ softline ^^ text "arg2," ^^ softline ^^ text "arg3")
    ^^ text ")"
  ) in
  print_string (render ~width:15 doc);
  [%expect {|
    func(arg1,
         arg2,
         arg3) |}]

let%expect_test "align with empty returns empty" =
  print_string (render ~width:80 (align empty));
  [%expect {| |}]

(** {1 IfBreak} *)

let%expect_test "if_break chooses flat when group is flat" =
  let doc = group (
    text "list(" ^^
    if_break ~broken:(text "BROKEN") ~flat:(text "flat") ^^
    text ")"
  ) in
  print_string (render ~width:80 doc);
  [%expect {| list(flat) |}]

let%expect_test "if_break chooses broken when group is broken" =
  let doc = group (
    text "very_long_function_name(" ^^
    if_break ~broken:(text "BROKEN") ~flat:(text "flat") ^^
    text ")"
  ) in
  print_string (render ~width:20 doc);
  [%expect {| very_long_function_name(BROKEN) |}]

(** {1 Width Edge Cases} *)

let%expect_test "exactly fits on line" =
  let doc = group (text "12345" ^/^ text "12345") in
  (* "12345 12345" = 11 chars *)
  print_string (render ~width:11 doc);
  [%expect {| 12345 12345 |}]

let%expect_test "one char over breaks" =
  let doc = group (text "12345" ^/^ text "12345") in
  print_string (render ~width:10 doc);
  [%expect {|
    12345
    12345 |}]

let%expect_test "very narrow width still works" =
  let doc = group (text "abc" ^/^ text "def") in
  print_string (render ~width:1 doc);
  [%expect {|
    abc
    def |}]

(** {1 Derived Combinators} *)

let%expect_test "concat_list concatenates list of docs" =
  let docs = [text "a"; text "b"; text "c"] in
  print_string (render ~width:80 (concat_list docs));
  [%expect {| abc |}]

let%expect_test "concat_list with empty list" =
  print_string (render ~width:80 (concat_list []));
  [%expect {| |}]

let%expect_test "separate with comma" =
  let docs = [text "a"; text "b"; text "c"] in
  print_string (render ~width:80 (separate (text ", ") docs));
  [%expect {| a, b, c |}]

let%expect_test "separate with single element" =
  let docs = [text "a"] in
  print_string (render ~width:80 (separate (text ", ") docs));
  [%expect {| a |}]

let%expect_test "separate with empty list" =
  print_string (render ~width:80 (separate (text ", ") []));
  [%expect {| |}]

let%expect_test "join with comma adds spaces" =
  let docs = [text "a"; text "b"; text "c"] in
  print_string (render ~width:80 (join comma docs));
  [%expect {| a, b, c |}]

let%expect_test "surround wraps content" =
  print_string (render ~width:80 (surround lparen (text "x") rparen));
  [%expect {| (x) |}]

let%expect_test "block formats correctly when fits" =
  let body = text "x = 1" in
  print_string (render ~width:80 (block lbrace body rbrace));
  [%expect {| {x = 1} |}]

let%expect_test "block breaks when too long" =
  let body = text "very_long_content_here" in
  print_string (render ~width:15 (block lbrace body rbrace));
  [%expect {|
    {
      very_long_content_here
    } |}]

let%expect_test "prefix keeps header on same line" =
  let header = text "if condition then" in
  let body = text "do_something" in
  print_string (render ~width:80 (prefix header body));
  [%expect {| if condition thendo_something |}]

let%expect_test "prefix breaks and indents body" =
  let header = text "if condition then" in
  let body = text "do_something" in
  print_string (render ~width:20 (prefix header body));
  [%expect {|
    if condition then
      do_something |}]

let%expect_test "infix formats operator between operands" =
  let left = text "left" in
  let right = text "right" in
  print_string (render ~width:80 (infix (text "+") left right));
  [%expect {| left +right |}]

let%expect_test "fill puts items on same line when possible" =
  let docs = [text "a"; text "b"; text "c"; text "d"] in
  print_string (render ~width:80 (fill docs));
  [%expect {| a b c d |}]

let%expect_test "fill breaks when needed" =
  let docs = [text "aaa"; text "bbb"; text "ccc"; text "ddd"] in
  print_string (render ~width:10 (fill docs));
  [%expect {|
    aaa
    bbb
    ccc
    ddd |}]

(** {1 Common Delimiters} *)

let%expect_test "parens wraps in parentheses" =
  print_string (render ~width:80 (parens (text "x")));
  [%expect {| (x) |}]

let%expect_test "braces wraps in braces" =
  print_string (render ~width:80 (braces (text "x")));
  [%expect {| {x} |}]

let%expect_test "brackets wraps in brackets" =
  print_string (render ~width:80 (brackets (text "x")));
  [%expect {| [x] |}]

let%expect_test "common symbols render correctly" =
  let doc =
    comma ^^ semi ^^ colon ^^ equals ^^ arrow ^^ fat_arrow ^^ pipe ^^ dot
  in
  print_string (render ~width:80 doc);
  [%expect {| ,;:=->=>|. |}]

(** {1 LineSuffix} *)

let%expect_test "line_suffix appends at end of line" =
  let doc =
    text "code" ^^
    line_suffix (text " // comment") ^^
    hardline ^^
    text "more"
  in
  print_string (render ~width:80 doc);
  [%expect {|
    code // comment
    more |}]

(** {1 Pretty Printer Aliases} *)

let%expect_test "pretty uses width 80" =
  print_string (pretty (text "hello"));
  [%expect {| hello |}]

(** {1 Complex Layouts} *)

let%expect_test "function definition layout" =
  let doc = group (
    text "let" ^/^ text "f" ^/^ text "x" ^/^ text "y" ^/^ equals ^^
    nest 2 (softline ^^ text "x + y")
  ) in
  print_string (render ~width:80 doc);
  [%expect {| let f x y =x + y |}]

let%expect_test "function definition breaks when too long" =
  let doc = group (
    text "let" ^/^ text "very_long_function_name" ^/^ text "argument1" ^/^ text "argument2" ^/^ equals ^^
    nest 2 (softline ^^ text "body_expression")
  ) in
  print_string (render ~width:40 doc);
  [%expect {|
    let
    very_long_function_name
    argument1
    argument2
    =
      body_expression
    |}]

let%expect_test "if-then-else layout flat" =
  let doc = group (
    text "if" ^/^ text "cond" ^/^ text "then" ^/^ text "a" ^/^ text "else" ^/^ text "b"
  ) in
  print_string (render ~width:80 doc);
  [%expect {| if cond then a else b |}]

let%expect_test "if-then-else layout broken" =
  let doc = group (
    text "if" ^/^ text "condition" ^/^ text "then" ^^
    nest 2 (softline ^^ text "true_branch") ^^
    softline ^^ text "else" ^^
    nest 2 (softline ^^ text "false_branch")
  ) in
  print_string (render ~width:25 doc);
  [%expect {|
    if
    condition
    then
      true_branch
    else
      false_branch
    |}]

let%expect_test "record layout flat" =
  let fields = separate (semi ^^ space) [
    text "x = 1";
    text "y = 2";
    text "z = 3"
  ] in
  let doc = group (lbrace ^/^ fields ^/^ rbrace) in
  print_string (render ~width:80 doc);
  [%expect {| { x = 1; y = 2; z = 3 } |}]

let%expect_test "record layout broken" =
  let fields = separate (semi ^^ softline) [
    text "x = 1";
    text "y = 2";
    text "z = 3"
  ] in
  let doc = group (
    lbrace ^^
    nest 2 (softline ^^ fields) ^^
    softline ^^ rbrace
  ) in
  print_string (render ~width:15 doc);
  [%expect {|
    {
      x = 1;
      y = 2;
      z = 3
    } |}]

let%expect_test "match expression layout" =
  let arms = concat_list [
    hardline ^^ pipe ^/^ text "None" ^/^ arrow ^/^ text "0";
    hardline ^^ pipe ^/^ text "Some x" ^/^ arrow ^/^ text "x"
  ] in
  let doc = group (
    text "match" ^/^ text "value" ^/^ text "with" ^^
    nest 2 arms
  ) in
  print_string (render ~width:80 doc);
  [%expect {|
    match value with
      | None -> 0
      | Some x -> x |}]

let%expect_test "nested function calls" =
  let inner = group (text "inner_func" ^/^ text "arg1" ^/^ text "arg2") in
  let outer = group (text "outer_func" ^/^ parens inner) in
  print_string (render ~width:80 outer);
  [%expect {| outer_func (inner_func arg1 arg2) |}]

let%expect_test "nested function calls breaking" =
  let inner = group (text "inner_function" ^/^ text "argument1" ^/^ text "argument2") in
  let outer = group (text "outer_function" ^/^ parens inner) in
  print_string (render ~width:35 outer);
  [%expect {|
    outer_function
    (inner_function argument1 argument2) |}]

let%expect_test "chained operators" =
  let doc = group (
    text "a" ^/^ text "+" ^/^ text "b" ^/^ text "*" ^/^ text "c" ^/^
    text "-" ^/^ text "d" ^/^ text "/" ^/^ text "e"
  ) in
  print_string (render ~width:80 doc);
  [%expect {| a + b * c - d / e |}]

let%expect_test "module structure layout" =
  let items = concat_list [
    hardline ^^ text "type t = int";
    hardline ^^ text "let x = 42";
    hardline ^^ text "let f y = y + 1"
  ] in
  let doc = group (
    text "struct" ^^
    nest 2 items ^^
    hardline ^^ text "end"
  ) in
  print_string (render ~width:80 doc);
  [%expect {|
    struct
      type t = int
      let x = 42
      let f y = y + 1
    end |}]
