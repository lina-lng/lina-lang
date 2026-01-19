(** Unit tests for Dict module and dict primitives.

    Tests cover:
    - Dict primitive type inference
    - Dict module function types
    - Dict operations (compile and run)
    - Type error cases *)

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Driver.Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let compile_and_run source =
  let lua_code = compile source in
  if String.length lua_code >= 6 && String.sub lua_code 0 6 = "ERROR:" then
    lua_code
  else
    let temp_file = Filename.temp_file "lina_test" ".lua" in
    let oc = open_out temp_file in
    output_string oc lua_code;
    close_out oc;
    let ic = Unix.open_process_in ("luajit " ^ temp_file ^ " 2>&1") in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    Sys.remove temp_file;
    String.trim output

let type_check source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Ok _ -> "OK"
  | Error msg -> "ERROR: " ^ msg

(** {1 Dict Primitive Type Inference} *)

let%expect_test "dict_empty has type unit -> ('a, 'b) dict" =
  print_endline (type_check {|
let d = dict_empty ()
|});
  [%expect {| OK |}]

let%expect_test "dict_get has type 'k -> ('k, 'v) dict -> 'v option" =
  print_endline (type_check {|
let d = dict_empty ()
let result = dict_get "key" d
|});
  [%expect {| OK |}]

let%expect_test "dict_set has type 'k -> 'v -> ('k, 'v) dict -> ('k, 'v) dict" =
  print_endline (type_check {|
let d = dict_empty ()
let d2 = dict_set "key" 42 d
|});
  [%expect {| OK |}]

let%expect_test "dict_has has type 'k -> ('k, 'v) dict -> bool" =
  print_endline (type_check {|
let d = dict_empty ()
let b = dict_has "key" d
|});
  [%expect {| OK |}]

let%expect_test "dict_remove has type 'k -> ('k, 'v) dict -> ('k, 'v) dict" =
  print_endline (type_check {|
let d = dict_set "key" 42 (dict_empty ())
let d2 = dict_remove "key" d
|});
  [%expect {| OK |}]

let%expect_test "dict_size has type ('k, 'v) dict -> int" =
  print_endline (type_check {|
let d = dict_empty ()
let n = dict_size d
|});
  [%expect {| OK |}]

let%expect_test "dict_keys has type ('k, 'v) dict -> 'k list" =
  print_endline (type_check {|
let d = dict_set "a" 1 (dict_empty ())
let keys = dict_keys d
|});
  [%expect {| OK |}]

let%expect_test "dict_entries has type ('k, 'v) dict -> ('k * 'v) list" =
  print_endline (type_check {|
let d = dict_set "a" 1 (dict_empty ())
let entries = dict_entries d
|});
  [%expect {| OK |}]

(** {1 Dict Module Type Inference} *)

let%expect_test "Dict.empty returns empty dict" =
  print_endline (type_check {|
let d = Dict.empty ()
|});
  [%expect {| OK |}]

let%expect_test "Dict.singleton creates single-binding dict" =
  print_endline (type_check {|
let d = Dict.singleton "key" 42
|});
  [%expect {| OK |}]

let%expect_test "Dict.of_list creates dict from list" =
  print_endline (type_check {|
let d = Dict.of_list [("a", 1); ("b", 2)]
|});
  [%expect {| OK |}]

let%expect_test "Dict.get returns option" =
  print_endline (type_check {|
let d = Dict.singleton "a" 1
let result = Dict.get "a" d
|});
  [%expect {| OK |}]

let%expect_test "Dict.get_or returns value or default" =
  print_endline (type_check {|
let d = Dict.singleton "a" 1
let v = Dict.get_or "b" 0 d
|});
  [%expect {| OK |}]

let%expect_test "Dict.map transforms values" =
  print_endline (type_check {|
let d = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.map (fun x -> x * 2) d
|});
  [%expect {| OK |}]

let%expect_test "Dict.filter keeps matching bindings" =
  print_endline (type_check {|
let d = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.filter (fun k v -> v > 1) d
|});
  [%expect {| OK |}]

let%expect_test "Dict.fold accumulates over bindings" =
  print_endline (type_check {|
let d = Dict.of_list [("a", 1); ("b", 2)]
let sum = Dict.fold (fun k v acc -> acc + v) d 0
|});
  [%expect {| OK |}]

let%expect_test "Dict.merge combines two dicts" =
  print_endline (type_check {|
let d1 = Dict.of_list [("a", 1)]
let d2 = Dict.of_list [("b", 2)]
let merged = Dict.merge d1 d2
|});
  [%expect {| OK |}]

let%expect_test "Dict.equal compares dicts" =
  print_endline (type_check {|
let d1 = Dict.of_list [("a", 1)]
let d2 = Dict.of_list [("a", 1)]
let eq = Dict.equal (fun a b -> a == b) d1 d2
|});
  [%expect {| OK |}]

(** {1 Dict Runtime Behavior} *)

let%expect_test "Dict.empty creates empty dict" =
  print_endline (compile_and_run {|
let d = Dict.empty ()
let _ = print (Dict.is_empty d)
|});
  [%expect {| true |}]

let%expect_test "Dict.singleton creates dict with one binding" =
  print_endline (compile_and_run {|
let d = Dict.singleton "a" 42
let _ = print (Dict.size d)
let _ = print (Dict.get_or "a" 0 d)
|});
  [%expect {|
    1
    42 |}]

let%expect_test "Dict.set adds binding" =
  print_endline (compile_and_run {|
let d = Dict.empty ()
let d = Dict.set "a" 1 d
let d = Dict.set "b" 2 d
let _ = print (Dict.size d)
|});
  [%expect {| 2 |}]

let%expect_test "Dict.set is immutable" =
  print_endline (compile_and_run {|
let d1 = Dict.singleton "a" 1
let d2 = Dict.set "b" 2 d1
let _ = print (Dict.size d1)
let _ = print (Dict.size d2)
|});
  [%expect {|
    1
    2 |}]

let%expect_test "Dict.get returns Some for existing key" =
  print_endline (compile_and_run {|
let d = Dict.singleton "a" 42
let _ = match Dict.get "a" d with
  | Some v -> print v
  | None -> print 0
|});
  [%expect {| 42 |}]

let%expect_test "Dict.get returns None for missing key" =
  print_endline (compile_and_run {|
let d = Dict.singleton "a" 42
let _ = match Dict.get "b" d with
  | Some v -> print v
  | None -> print "not found"
|});
  [%expect {| not found |}]

let%expect_test "Dict.has checks key existence" =
  print_endline (compile_and_run {|
let d = Dict.singleton "a" 1
let _ = print (Dict.has "a" d)
let _ = print (Dict.has "b" d)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Dict.remove removes key" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.remove "a" d
let _ = print (Dict.has "a" d2)
let _ = print (Dict.size d2)
|});
  [%expect {|
    false
    1 |}]

let%expect_test "Dict.remove is immutable" =
  print_endline (compile_and_run {|
let d1 = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.remove "a" d1
let _ = print (Dict.size d1)
let _ = print (Dict.size d2)
|});
  [%expect {|
    2
    1 |}]

let%expect_test "Dict.map transforms values" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.map (fun v -> v * 10) d
let _ = print (Dict.get_or "a" 0 d2)
let _ = print (Dict.get_or "b" 0 d2)
|});
  [%expect {|
    10
    20 |}]

let%expect_test "Dict.filter keeps matching" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]
let d2 = Dict.filter (fun k v -> v > 1) d
let _ = print (Dict.size d2)
let _ = print (Dict.has "a" d2)
|});
  [%expect {|
    2
    false |}]

let%expect_test "Dict.fold sums values" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]
let sum = Dict.fold (fun k v acc -> acc + v) d 0
let _ = print sum
|});
  [%expect {| 6 |}]

let%expect_test "Dict.merge combines dicts" =
  print_endline (compile_and_run {|
let d1 = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.of_list [("b", 20); ("c", 3)]
let merged = Dict.merge d1 d2
let _ = print (Dict.get_or "a" 0 merged)
let _ = print (Dict.get_or "b" 0 merged)
let _ = print (Dict.get_or "c" 0 merged)
|});
  [%expect {|
    1
    20
    3 |}]

let%expect_test "Dict.equal compares dicts" =
  print_endline (compile_and_run {|
let d1 = Dict.of_list [("a", 1); ("b", 2)]
let d2 = Dict.of_list [("a", 1); ("b", 2)]
let d3 = Dict.of_list [("a", 1); ("b", 99)]
let _ = print (Dict.equal (fun a b -> a == b) d1 d2)
let _ = print (Dict.equal (fun a b -> a == b) d1 d3)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Dict works with integer keys" =
  print_endline (compile_and_run {|
let d = Dict.of_list [(1, "one"); (2, "two")]
let _ = print (Dict.get_or 1 "?" d)
let _ = print (Dict.get_or 2 "?" d)
|});
  [%expect {|
    one
    two |}]

let%expect_test "Dict works with boolean keys" =
  print_endline (compile_and_run {|
let d = Dict.of_list [(true, 1); (false, 0)]
let _ = print (Dict.get_or true 99 d)
let _ = print (Dict.get_or false 99 d)
|});
  [%expect {|
    1
    0 |}]

let%expect_test "Dict.exists finds matching binding" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]
let _ = print (Dict.exists (fun k v -> v > 2) d)
let _ = print (Dict.exists (fun k v -> v > 10) d)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Dict.for_all checks all bindings" =
  print_endline (compile_and_run {|
let d = Dict.of_list [("a", 1); ("b", 2); ("c", 3)]
let _ = print (Dict.for_all (fun k v -> v > 0) d)
let _ = print (Dict.for_all (fun k v -> v > 1) d)
|});
  [%expect {|
    true
    false |}]

(** {1 Type Error Cases} *)

let%expect_test "dict_set with mismatched key type fails" =
  print_endline (type_check {|
let d = dict_set "key" 1 (dict_empty ())
let d2 = dict_set 42 2 d
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:24

       2 | let d = dict_set "key" 1 (dict_empty ())
       3 | let d2 = dict_set 42 2 d
                                  ^
                                  expected `int`, found `string`

    This function's 3rd argument expects type `(int, int) dict`, but you gave it `(string, int) dict`.

    note: The problem is in type argument 1.

    note: To convert a string to an int, use `int_of_string value`
    |}]

let%expect_test "dict_set with mismatched value type fails" =
  print_endline (type_check {|
let d = dict_set "key" 1 (dict_empty ())
let d2 = dict_set "other" "string" d
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:36

       2 | let d = dict_set "key" 1 (dict_empty ())
       3 | let d2 = dict_set "other" "string" d
                                              ^
                                              expected `string`, found `int`

    This function's 3rd argument expects type `(string, string) dict`, but you gave it `(string, int) dict`.

    note: The problem is in type argument 2.

    note: To convert an int to a string, use `string_of_int value`
    |}]

let%expect_test "dict_get with wrong key type fails" =
  print_endline (type_check {|
let d = dict_set "key" 1 (dict_empty ())
let v = dict_get 42 d
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:21

       2 | let d = dict_set "key" 1 (dict_empty ())
       3 | let v = dict_get 42 d
                               ^
                               expected `int`, found `string`

    This function's 2nd argument expects type `(int, 'a) dict`, but you gave it `(string, int) dict`.

    note: The problem is in type argument 1.

    note: To convert a string to an int, use `int_of_string value`
    |}]

let%expect_test "Dict.get_or with wrong default type fails" =
  print_endline (type_check {|
let d = Dict.singleton "a" 1
let v = Dict.get_or "a" "wrong" d
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:33

       2 | let d = Dict.singleton "a" 1
       3 | let v = Dict.get_or "a" "wrong" d
                                           ^
                                           expected `string`, found `int`

    This function's 3rd argument expects type `(string, string) dict`, but you gave it `(string, int) dict`.

    note: The problem is in type argument 2.

    note: To convert an int to a string, use `string_of_int value`
    |}]
