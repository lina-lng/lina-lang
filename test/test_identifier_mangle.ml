(** Unit tests for Identifier_mangle module.

    Tests cover:
    - is_lua_keyword: recognizing Lua reserved keywords
    - mangle_identifier: converting Lina identifiers to valid Lua identifiers *)

open Lua

(** {1 is_lua_keyword Tests} *)

let%expect_test "is_lua_keyword: recognizes 'and'" =
  print_endline (if Identifier_mangle.is_lua_keyword "and" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'break'" =
  print_endline (if Identifier_mangle.is_lua_keyword "break" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'do'" =
  print_endline (if Identifier_mangle.is_lua_keyword "do" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'else'" =
  print_endline (if Identifier_mangle.is_lua_keyword "else" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'elseif'" =
  print_endline (if Identifier_mangle.is_lua_keyword "elseif" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'end'" =
  print_endline (if Identifier_mangle.is_lua_keyword "end" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'false'" =
  print_endline (if Identifier_mangle.is_lua_keyword "false" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'for'" =
  print_endline (if Identifier_mangle.is_lua_keyword "for" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'function'" =
  print_endline (if Identifier_mangle.is_lua_keyword "function" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'goto'" =
  print_endline (if Identifier_mangle.is_lua_keyword "goto" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'if'" =
  print_endline (if Identifier_mangle.is_lua_keyword "if" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'in'" =
  print_endline (if Identifier_mangle.is_lua_keyword "in" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'local'" =
  print_endline (if Identifier_mangle.is_lua_keyword "local" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'nil'" =
  print_endline (if Identifier_mangle.is_lua_keyword "nil" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'not'" =
  print_endline (if Identifier_mangle.is_lua_keyword "not" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'or'" =
  print_endline (if Identifier_mangle.is_lua_keyword "or" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'repeat'" =
  print_endline (if Identifier_mangle.is_lua_keyword "repeat" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'return'" =
  print_endline (if Identifier_mangle.is_lua_keyword "return" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'then'" =
  print_endline (if Identifier_mangle.is_lua_keyword "then" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'true'" =
  print_endline (if Identifier_mangle.is_lua_keyword "true" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'until'" =
  print_endline (if Identifier_mangle.is_lua_keyword "until" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: recognizes 'while'" =
  print_endline (if Identifier_mangle.is_lua_keyword "while" then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "is_lua_keyword: non-keyword returns false" =
  print_endline (if Identifier_mangle.is_lua_keyword "foo" then "yes" else "no");
  [%expect {| no |}]

let%expect_test "is_lua_keyword: case sensitive (IF is not keyword)" =
  print_endline (if Identifier_mangle.is_lua_keyword "IF" then "yes" else "no");
  [%expect {| no |}]

let%expect_test "is_lua_keyword: case sensitive (And is not keyword)" =
  print_endline (if Identifier_mangle.is_lua_keyword "And" then "yes" else "no");
  [%expect {| no |}]

let%expect_test "is_lua_keyword: empty string is not keyword" =
  print_endline (if Identifier_mangle.is_lua_keyword "" then "yes" else "no");
  [%expect {| no |}]

let%expect_test "is_lua_keyword: similar but different words" =
  print_endline (if Identifier_mangle.is_lua_keyword "iff" then "yes" else "no");
  print_endline (if Identifier_mangle.is_lua_keyword "endif" then "yes" else "no");
  print_endline (if Identifier_mangle.is_lua_keyword "returns" then "yes" else "no");
  [%expect {|
    no
    no
    no |}]

(** {1 mangle_identifier Tests}

    Note: We use [Identifier.create_with_stamp] to control stamp values,
    since [Identifier.create] uses a global counter that persists across tests. *)

let%expect_test "mangle_identifier: non-keyword with stamp 0 unchanged" =
  let id = Common.Identifier.create_with_stamp "foo" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| foo |}]

let%expect_test "mangle_identifier: non-keyword with stamp > 0 gets suffix" =
  let id = Common.Identifier.create_with_stamp "bar" 0 in
  let id2 = Common.Identifier.create_with_stamp "bar" 1 in
  print_endline (Identifier_mangle.mangle_identifier id);
  print_endline (Identifier_mangle.mangle_identifier id2);
  [%expect {|
    bar
    bar_1 |}]

let%expect_test "mangle_identifier: keyword gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "if" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _if |}]

let%expect_test "mangle_identifier: keyword 'and' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "and" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _and |}]

let%expect_test "mangle_identifier: keyword 'or' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "or" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _or |}]

let%expect_test "mangle_identifier: keyword 'not' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "not" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _not |}]

let%expect_test "mangle_identifier: keyword 'function' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "function" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _function |}]

let%expect_test "mangle_identifier: keyword 'end' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "end" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _end |}]

let%expect_test "mangle_identifier: keyword 'local' gets underscore prefix" =
  let id = Common.Identifier.create_with_stamp "local" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _local |}]

let%expect_test "mangle_identifier: keyword with stamp gets prefix and suffix" =
  let id1 = Common.Identifier.create_with_stamp "if" 0 in
  let id2 = Common.Identifier.create_with_stamp "if" 1 in
  print_endline (Identifier_mangle.mangle_identifier id1);
  print_endline (Identifier_mangle.mangle_identifier id2);
  [%expect {|
    _if
    _if_1 |}]

let%expect_test "mangle_identifier: multiple stamps increment" =
  let id1 = Common.Identifier.create_with_stamp "x" 0 in
  let id2 = Common.Identifier.create_with_stamp "x" 1 in
  let id3 = Common.Identifier.create_with_stamp "x" 2 in
  print_endline (Identifier_mangle.mangle_identifier id1);
  print_endline (Identifier_mangle.mangle_identifier id2);
  print_endline (Identifier_mangle.mangle_identifier id3);
  [%expect {|
    x
    x_1
    x_2 |}]

let%expect_test "mangle_identifier: special characters preserved" =
  let id = Common.Identifier.create_with_stamp "my_var" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| my_var |}]

let%expect_test "mangle_identifier: underscore prefix preserved if not keyword" =
  let id = Common.Identifier.create_with_stamp "_private" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| _private |}]

(** {1 Edge Cases} *)

let%expect_test "mangle_identifier: short names" =
  let a = Common.Identifier.create_with_stamp "a" 0 in
  let b = Common.Identifier.create_with_stamp "b" 0 in
  print_endline (Identifier_mangle.mangle_identifier a);
  print_endline (Identifier_mangle.mangle_identifier b);
  [%expect {|
    a
    b |}]

let%expect_test "mangle_identifier: numeric suffix names" =
  let id = Common.Identifier.create_with_stamp "var1" 0 in
  print_endline (Identifier_mangle.mangle_identifier id);
  [%expect {| var1 |}]
