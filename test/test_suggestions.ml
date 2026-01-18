(** Unit tests for the suggestions module. *)

open Common

(** {1 Edit Distance Tests} *)

let%expect_test "edit_distance: identical strings" =
  Printf.printf "%d\n" (Suggestions.edit_distance "hello" "hello");
  [%expect {| 0 |}]

let%expect_test "edit_distance: empty strings" =
  Printf.printf "%d\n" (Suggestions.edit_distance "" "");
  [%expect {| 0 |}]

let%expect_test "edit_distance: one empty string" =
  Printf.printf "%d %d\n"
    (Suggestions.edit_distance "" "hello")
    (Suggestions.edit_distance "hello" "");
  [%expect {| 5 5 |}]

let%expect_test "edit_distance: single substitution" =
  Printf.printf "%d\n" (Suggestions.edit_distance "cat" "bat");
  [%expect {| 1 |}]

let%expect_test "edit_distance: single insertion" =
  Printf.printf "%d\n" (Suggestions.edit_distance "cat" "cats");
  [%expect {| 1 |}]

let%expect_test "edit_distance: single deletion" =
  Printf.printf "%d\n" (Suggestions.edit_distance "cats" "cat");
  [%expect {| 1 |}]

let%expect_test "edit_distance: multiple edits" =
  Printf.printf "%d\n" (Suggestions.edit_distance "kitten" "sitting");
  [%expect {| 3 |}]

let%expect_test "edit_distance: case sensitive" =
  Printf.printf "%d\n" (Suggestions.edit_distance "Hello" "hello");
  [%expect {| 1 |}]

(** {1 Threshold Tests} *)

let%expect_test "suggestion_threshold: short names" =
  Printf.printf "%d %d %d\n"
    (Suggestions.suggestion_threshold "x")
    (Suggestions.suggestion_threshold "ab")
    (Suggestions.suggestion_threshold "foo");
  [%expect {| 1 1 1 |}]

let%expect_test "suggestion_threshold: medium names" =
  Printf.printf "%d %d %d\n"
    (Suggestions.suggestion_threshold "config")
    (Suggestions.suggestion_threshold "handler")
    (Suggestions.suggestion_threshold "process");
  [%expect {| 2 3 3 |}]

let%expect_test "suggestion_threshold: long names" =
  Printf.printf "%d %d\n"
    (Suggestions.suggestion_threshold "configuration")
    (Suggestions.suggestion_threshold "authentication");
  [%expect {| 3 3 |}]

(** {1 within_threshold Tests} *)

let%expect_test "within_threshold: identical" =
  Printf.printf "%b\n" (Suggestions.within_threshold ~target:"foo" ~candidate:"foo");
  [%expect {| true |}]

let%expect_test "within_threshold: one edit for short name" =
  Printf.printf "%b\n" (Suggestions.within_threshold ~target:"foo" ~candidate:"fob");
  [%expect {| true |}]

let%expect_test "within_threshold: two edits for short name" =
  Printf.printf "%b\n" (Suggestions.within_threshold ~target:"foo" ~candidate:"bar");
  [%expect {| false |}]

let%expect_test "within_threshold: two edits for medium name" =
  Printf.printf "%b\n" (Suggestions.within_threshold ~target:"config" ~candidate:"confog");
  [%expect {| true |}]

(** {1 find_similar Tests} *)

let%expect_test "find_similar: no candidates" =
  let result = Suggestions.find_similar ~target:"foo" ~candidates:[] in
  Printf.printf "%d\n" (List.length result);
  [%expect {| 0 |}]

let%expect_test "find_similar: one close match" =
  let candidates = ["bar"; "fob"; "baz"] in
  let result = Suggestions.find_similar ~target:"foo" ~candidates in
  List.iter (fun (name, dist) -> Printf.printf "%s: %d\n" name dist) result;
  [%expect {| fob: 1 |}]

let%expect_test "find_similar: multiple matches sorted by distance" =
  let candidates = ["handler"; "hander"; "handle"; "holder"] in
  let result = Suggestions.find_similar ~target:"hanlder" ~candidates in
  List.iter (fun (name, dist) -> Printf.printf "%s: %d\n" name dist) result;
  [%expect {|
    hander: 1
    handler: 2
    holder: 2
    handle: 3
    |}]

let%expect_test "find_similar: excludes target itself" =
  let candidates = ["foo"; "fob"; "bar"] in
  let result = Suggestions.find_similar ~target:"foo" ~candidates in
  List.iter (fun (name, _) -> Printf.printf "%s\n" name) result;
  [%expect {| fob |}]

(** {1 find_closest Tests} *)

let%expect_test "find_closest: returns closest match" =
  let candidates = ["bar"; "fob"; "baz"] in
  let result = Suggestions.find_closest ~target:"foo" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| fob |}]

let%expect_test "find_closest: returns None when no matches" =
  let candidates = ["completely"; "different"; "words"] in
  let result = Suggestions.find_closest ~target:"foo" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| None |}]

(** {1 did_you_mean Tests} *)

let%expect_test "did_you_mean: formats suggestion" =
  let candidates = ["foo"; "fob"; "bar"] in
  let result = Suggestions.did_you_mean ~target:"foob" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| Did you mean `foo`? |}]

let%expect_test "did_you_mean: returns None when no matches" =
  let candidates = ["xyz"; "abc"; "def"] in
  let result = Suggestions.did_you_mean ~target:"foo" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| None |}]

(** {1 format_suggestions Tests} *)

let%expect_test "format_suggestions: single match" =
  let candidates = ["fob"; "xyz"; "abc"] in
  let result = Suggestions.format_suggestions ~target:"foo" ~candidates () in
  Printf.printf "%s\n" result;
  [%expect {| Did you mean `fob`? |}]

let%expect_test "format_suggestions: multiple matches" =
  let candidates = ["handler"; "hander"; "handle"; "hanlder"] in
  let result = Suggestions.format_suggestions ~target:"handleer" ~candidates () in
  Printf.printf "%s\n" result;
  [%expect {|
    Did you mean one of these?
      - handler
      - hander
      - handle |}]

let%expect_test "format_suggestions: no matches" =
  let candidates = ["xyz"; "abc"; "def"] in
  let result = Suggestions.format_suggestions ~target:"foo" ~candidates () in
  Printf.printf "'%s'\n" result;
  [%expect {| '' |}]

let%expect_test "format_suggestions: respects max_suggestions" =
  let candidates = ["fooa"; "foob"; "fooc"; "food"; "fooe"] in
  let result = Suggestions.format_suggestions ~max_suggestions:2 ~target:"foo" ~candidates () in
  Printf.printf "%s\n" result;
  [%expect {|
    Did you mean one of these?
      - fooa
      - foob |}]

(** {1 Realistic Error Scenario Tests} *)

let%expect_test "realistic: typo in common function name" =
  let candidates = ["print"; "printf"; "println"; "sprint"; "eprintf"] in
  let result = Suggestions.did_you_mean ~target:"pirnt" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| Did you mean `print`? |}]

let%expect_test "realistic: missing letter" =
  let candidates = ["length"; "filter"; "map"; "fold"; "iter"] in
  let result = Suggestions.did_you_mean ~target:"lenght" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| Did you mean `length`? |}]

let%expect_test "realistic: extra letter" =
  let candidates = ["Option"; "Result"; "List"; "Array"; "String"] in
  let result = Suggestions.did_you_mean ~target:"Optionn" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| Did you mean `Option`? |}]

let%expect_test "realistic: swapped letters" =
  let candidates = ["config"; "context"; "container"; "content"; "connect"] in
  let result = Suggestions.did_you_mean ~target:"ocnfig" ~candidates in
  print_endline (Option.value ~default:"None" result);
  [%expect {| Did you mean `config`? |}]
