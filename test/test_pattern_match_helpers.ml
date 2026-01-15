(** Unit tests for Pattern_match helper functions.

    Tests cover:
    - split_at_remove: single-pass list splitting
    - remove_at: element removal
    - head_constructor_key: unique key generation for constructors *)

(* Pattern_match module from the lambda library, which is unwrapped *)

(** {1 split_at_remove Tests} *)

let%expect_test "split_at_remove: split at index 0 (empty before)" =
  let before, after = Pattern_match.split_at_remove 0 [1; 2; 3; 4; 5] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [], after: [2;3;4;5] |}]

let%expect_test "split_at_remove: split at last index (empty after)" =
  let before, after = Pattern_match.split_at_remove 4 [1; 2; 3; 4; 5] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [1;2;3;4], after: [] |}]

let%expect_test "split_at_remove: split in middle" =
  let before, after = Pattern_match.split_at_remove 2 [1; 2; 3; 4; 5] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [1;2], after: [4;5] |}]

let%expect_test "split_at_remove: split at index 1" =
  let before, after = Pattern_match.split_at_remove 1 [1; 2; 3; 4; 5] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [1], after: [3;4;5] |}]

let%expect_test "split_at_remove: empty list" =
  let before, after = Pattern_match.split_at_remove 0 ([] : int list) in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [], after: [] |}]

let%expect_test "split_at_remove: single element list" =
  let before, after = Pattern_match.split_at_remove 0 [42] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [], after: [] |}]

let%expect_test "split_at_remove: two element list, remove first" =
  let before, after = Pattern_match.split_at_remove 0 ["a"; "b"] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" before)
    (String.concat ";" after);
  [%expect {| before: [], after: [b] |}]

let%expect_test "split_at_remove: two element list, remove second" =
  let before, after = Pattern_match.split_at_remove 1 ["a"; "b"] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" before)
    (String.concat ";" after);
  [%expect {| before: [a], after: [] |}]

let%expect_test "split_at_remove: index beyond list length" =
  let before, after = Pattern_match.split_at_remove 10 [1; 2; 3] in
  Printf.printf "before: [%s], after: [%s]"
    (String.concat ";" (List.map string_of_int before))
    (String.concat ";" (List.map string_of_int after));
  [%expect {| before: [1;2;3], after: [] |}]

(** {1 remove_at Tests} *)

let%expect_test "remove_at: remove first element" =
  let result = Pattern_match.remove_at 0 [1; 2; 3; 4; 5] in
  Printf.printf "[%s]" (String.concat ";" (List.map string_of_int result));
  [%expect {| [2;3;4;5] |}]

let%expect_test "remove_at: remove last element" =
  let result = Pattern_match.remove_at 4 [1; 2; 3; 4; 5] in
  Printf.printf "[%s]" (String.concat ";" (List.map string_of_int result));
  [%expect {| [1;2;3;4] |}]

let%expect_test "remove_at: remove middle element" =
  let result = Pattern_match.remove_at 2 [1; 2; 3; 4; 5] in
  Printf.printf "[%s]" (String.concat ";" (List.map string_of_int result));
  [%expect {| [1;2;4;5] |}]

let%expect_test "remove_at: remove from single-element list" =
  let result = Pattern_match.remove_at 0 [42] in
  Printf.printf "[%s]" (String.concat ";" (List.map string_of_int result));
  [%expect {| [] |}]

let%expect_test "remove_at: preserves element order" =
  let result = Pattern_match.remove_at 1 ["a"; "b"; "c"; "d"] in
  Printf.printf "[%s]" (String.concat ";" result);
  [%expect {| [a;c;d] |}]

(** {1 head_constructor_key Tests} *)

let%expect_test "head_constructor_key: HCWildcard" =
  print_endline (Pattern_match.head_constructor_key Pattern_match.HCWildcard);
  [%expect {| W |}]

let%expect_test "head_constructor_key: HCConstructor includes name" =
  print_endline (Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("Some", 1, false)));
  print_endline (Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("None", 0, false)));
  [%expect {|
    C:Some
    C:None |}]

let%expect_test "head_constructor_key: HCConstant integer" =
  let key = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantInteger 42)) in
  print_endline key;
  [%expect {| K:i42 |}]

let%expect_test "head_constructor_key: HCConstant float" =
  let key = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantFloat 3.14)) in
  print_endline key;
  [%expect {| K:f3.14 |}]

let%expect_test "head_constructor_key: HCConstant string" =
  let key = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantString "hello")) in
  print_endline key;
  [%expect {| K:shello |}]

let%expect_test "head_constructor_key: HCConstant boolean" =
  let key_true = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantBoolean true)) in
  let key_false = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantBoolean false)) in
  print_endline key_true;
  print_endline key_false;
  [%expect {|
    K:btrue
    K:bfalse |}]

let%expect_test "head_constructor_key: HCConstant unit" =
  let key = Pattern_match.head_constructor_key
    (Pattern_match.HCConstant Parsing.Syntax_tree.ConstantUnit) in
  print_endline key;
  [%expect {| K:u |}]

let%expect_test "head_constructor_key: HCTuple keys differ by arity" =
  let key2 = Pattern_match.head_constructor_key (Pattern_match.HCTuple 2) in
  let key3 = Pattern_match.head_constructor_key (Pattern_match.HCTuple 3) in
  let key5 = Pattern_match.head_constructor_key (Pattern_match.HCTuple 5) in
  print_endline key2;
  print_endline key3;
  print_endline key5;
  [%expect {|
    T:2
    T:3
    T:5 |}]

let%expect_test "head_constructor_key: HCRecord keys differ by field names" =
  let key1 = Pattern_match.head_constructor_key (Pattern_match.HCRecord ["x"; "y"]) in
  let key2 = Pattern_match.head_constructor_key (Pattern_match.HCRecord ["a"; "b"]) in
  let key3 = Pattern_match.head_constructor_key (Pattern_match.HCRecord ["x"; "y"; "z"]) in
  print_endline key1;
  print_endline key2;
  print_endline key3;
  [%expect {|
    R:x,y
    R:a,b
    R:x,y,z |}]

let%expect_test "head_constructor_key: HCRecord empty fields" =
  let key = Pattern_match.head_constructor_key (Pattern_match.HCRecord []) in
  print_endline key;
  [%expect {| R: |}]

(** {1 Key Uniqueness Tests} *)

let%expect_test "head_constructor_key: different constructors have different keys" =
  let keys = [
    Pattern_match.head_constructor_key Pattern_match.HCWildcard;
    Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("Foo", 0, false));
    Pattern_match.head_constructor_key (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantInteger 1));
    Pattern_match.head_constructor_key (Pattern_match.HCTuple 2);
    Pattern_match.head_constructor_key (Pattern_match.HCRecord ["x"]);
  ] in
  let unique_count = List.length (List.sort_uniq String.compare keys) in
  Printf.printf "all unique: %b" (unique_count = 5);
  [%expect {| all unique: true |}]

let%expect_test "head_constructor_key: same constructor type different values are distinct" =
  let keys = [
    Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("A", 0, false));
    Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("B", 0, false));
    Pattern_match.head_constructor_key (Pattern_match.HCConstructor ("C", 1, false));
  ] in
  let unique_count = List.length (List.sort_uniq String.compare keys) in
  Printf.printf "all unique: %b" (unique_count = 3);
  [%expect {| all unique: true |}]

let%expect_test "head_constructor_key: integer constants with different values" =
  let keys = [
    Pattern_match.head_constructor_key (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantInteger 0));
    Pattern_match.head_constructor_key (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantInteger 1));
    Pattern_match.head_constructor_key (Pattern_match.HCConstant (Parsing.Syntax_tree.ConstantInteger (-1)));
  ] in
  let unique_count = List.length (List.sort_uniq String.compare keys) in
  Printf.printf "all unique: %b" (unique_count = 3);
  [%expect {| all unique: true |}]
