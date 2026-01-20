(** Unit tests for Set module and set primitives.

    Tests cover:
    - Set primitive type inference
    - Set module function types
    - Set operations (compile and run)
    - Type error cases *)

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Driver.Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

(** Compile source and return Lua code (without stdlib prelude) or error. *)
let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Ok lua_code ->
    let prelude_len = String.length (Driver.Stdlib_loader.stdlib_prelude ()) in
    String.sub lua_code prelude_len (String.length lua_code - prelude_len)
  | Error msg -> "ERROR: " ^ msg

(** Compile and run with luajit, returning the output.
    Uses full compilation (with stdlib) since the code needs to actually run. *)
let compile_and_run source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Error msg -> "ERROR: " ^ msg
  | Ok lua_code ->
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

(** {1 Set Primitive Type Inference} *)

let%expect_test "set_empty has type unit -> 'a set" =
  print_endline (type_check {|
let s = set_empty ()
|});
  [%expect {| OK |}]

let%expect_test "set_add has type 'a -> 'a set -> 'a set" =
  print_endline (type_check {|
let s = set_empty ()
let s2 = set_add 42 s
|});
  [%expect {| OK |}]

let%expect_test "set_remove has type 'a -> 'a set -> 'a set" =
  print_endline (type_check {|
let s = set_add 42 (set_empty ())
let s2 = set_remove 42 s
|});
  [%expect {| OK |}]

let%expect_test "set_mem has type 'a -> 'a set -> bool" =
  print_endline (type_check {|
let s = set_empty ()
let b = set_mem 42 s
|});
  [%expect {| OK |}]

let%expect_test "set_size has type 'a set -> int" =
  print_endline (type_check {|
let s = set_empty ()
let n = set_size s
|});
  [%expect {| OK |}]

let%expect_test "set_elements has type 'a set -> 'a list" =
  print_endline (type_check {|
let s = set_add 1 (set_empty ())
let elems = set_elements s
|});
  [%expect {| OK |}]

(** {1 Set Module Type Inference} *)

let%expect_test "Set.empty returns empty set" =
  print_endline (type_check {|
let s = Set.empty ()
|});
  [%expect {| OK |}]

let%expect_test "Set.singleton creates single-element set" =
  print_endline (type_check {|
let s = Set.singleton 42
|});
  [%expect {| OK |}]

let%expect_test "Set.of_list creates set from list" =
  print_endline (type_check {|
let s = Set.of_list [1; 2; 3]
|});
  [%expect {| OK |}]

let%expect_test "Set.mem returns bool" =
  print_endline (type_check {|
let s = Set.singleton 42
let result = Set.mem 42 s
|});
  [%expect {| OK |}]

let%expect_test "Set.add returns set" =
  print_endline (type_check {|
let s = Set.empty ()
let s2 = Set.add 1 s
|});
  [%expect {| OK |}]

let%expect_test "Set.remove returns set" =
  print_endline (type_check {|
let s = Set.singleton 42
let s2 = Set.remove 42 s
|});
  [%expect {| OK |}]

let%expect_test "Set.union combines sets" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [2; 3]
let s3 = Set.union s1 s2
|});
  [%expect {| OK |}]

let%expect_test "Set.inter intersects sets" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [2; 3]
let s3 = Set.inter s1 s2
|});
  [%expect {| OK |}]

let%expect_test "Set.diff subtracts sets" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list [2]
let s3 = Set.diff s1 s2
|});
  [%expect {| OK |}]

let%expect_test "Set.subset checks subset relation" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [1; 2; 3]
let b = Set.subset s1 s2
|});
  [%expect {| OK |}]

let%expect_test "Set.map transforms elements" =
  print_endline (type_check {|
let s = Set.of_list [1; 2; 3]
let s2 = Set.map (fun x -> x * 2) s
|});
  [%expect {| OK |}]

let%expect_test "Set.filter keeps matching elements" =
  print_endline (type_check {|
let s = Set.of_list [1; 2; 3; 4]
let s2 = Set.filter (fun x -> x > 2) s
|});
  [%expect {| OK |}]

let%expect_test "Set.fold accumulates over elements" =
  print_endline (type_check {|
let s = Set.of_list [1; 2; 3]
let sum = Set.fold (fun x acc -> acc + x) s 0
|});
  [%expect {| OK |}]

let%expect_test "Set.equal compares sets" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list [3; 2; 1]
let eq = Set.equal s1 s2
|});
  [%expect {| OK |}]

(** {1 Set Runtime Behavior} *)

let%expect_test "Set.empty creates empty set" =
  print_endline (compile_and_run {|
let s = Set.empty ()
let _ = print (Set.is_empty s)
|});
  [%expect {| true |}]

let%expect_test "Set.singleton creates set with one element" =
  print_endline (compile_and_run {|
let s = Set.singleton 42
let _ = print (Set.size s)
let _ = print (Set.mem 42 s)
|});
  [%expect {|
    1
    true |}]

let%expect_test "Set.add adds element" =
  print_endline (compile_and_run {|
let s = Set.empty ()
let s = Set.add 1 s
let s = Set.add 2 s
let _ = print (Set.size s)
|});
  [%expect {| 2 |}]

let%expect_test "Set.add is idempotent" =
  print_endline (compile_and_run {|
let s = Set.singleton 1
let s = Set.add 1 s
let s = Set.add 1 s
let _ = print (Set.size s)
|});
  [%expect {| 1 |}]

let%expect_test "Set.add is immutable" =
  print_endline (compile_and_run {|
let s1 = Set.singleton 1
let s2 = Set.add 2 s1
let _ = print (Set.size s1)
let _ = print (Set.size s2)
|});
  [%expect {|
    1
    2 |}]

let%expect_test "Set.mem checks membership" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3]
let _ = print (Set.mem 2 s)
let _ = print (Set.mem 5 s)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.remove removes element" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3]
let s2 = Set.remove 2 s
let _ = print (Set.mem 2 s2)
let _ = print (Set.size s2)
|});
  [%expect {|
    false
    2 |}]

let%expect_test "Set.remove is immutable" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.remove 2 s1
let _ = print (Set.size s1)
let _ = print (Set.size s2)
|});
  [%expect {|
    3
    2 |}]

let%expect_test "Set.union combines sets" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [2; 3]
let s3 = Set.union s1 s2
let _ = print (Set.size s3)
let _ = print (Set.mem 1 s3)
let _ = print (Set.mem 2 s3)
let _ = print (Set.mem 3 s3)
|});
  [%expect {|
    3
    true
    true
    true |}]

let%expect_test "Set.inter finds common elements" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list [2; 3; 4]
let s3 = Set.inter s1 s2
let _ = print (Set.size s3)
let _ = print (Set.mem 1 s3)
let _ = print (Set.mem 2 s3)
|});
  [%expect {|
    2
    false
    true |}]

let%expect_test "Set.diff finds elements in first but not second" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list [2; 3; 4]
let s3 = Set.diff s1 s2
let _ = print (Set.size s3)
let _ = print (Set.mem 1 s3)
|});
  [%expect {|
    1
    true |}]

let%expect_test "Set.subset checks subset relation" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [1; 2; 3]
let _ = print (Set.subset s1 s2)
let _ = print (Set.subset s2 s1)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.disjoint checks no common elements" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2]
let s2 = Set.of_list [3; 4]
let s3 = Set.of_list [2; 3]
let _ = print (Set.disjoint s1 s2)
let _ = print (Set.disjoint s1 s3)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.map transforms elements" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3]
let s2 = Set.map (fun x -> x * 10) s
let _ = print (Set.mem 10 s2)
let _ = print (Set.mem 20 s2)
let _ = print (Set.mem 1 s2)
|});
  [%expect {|
    true
    true
    false |}]

let%expect_test "Set.filter keeps matching elements" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3; 4; 5]
let s2 = Set.filter (fun x -> x > 2) s
let _ = print (Set.size s2)
let _ = print (Set.mem 1 s2)
let _ = print (Set.mem 3 s2)
|});
  [%expect {|
    3
    false
    true |}]

let%expect_test "Set.fold sums elements" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3; 4]
let sum = Set.fold (fun x acc -> acc + x) s 0
let _ = print sum
|});
  [%expect {| 10 |}]

let%expect_test "Set.equal compares sets" =
  print_endline (compile_and_run {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list [3; 1; 2]
let s3 = Set.of_list [1; 2]
let _ = print (Set.equal s1 s2)
let _ = print (Set.equal s1 s3)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.of_list removes duplicates" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 2; 3; 3; 3]
let _ = print (Set.size s)
|});
  [%expect {| 3 |}]

let%expect_test "Set works with string elements" =
  print_endline (compile_and_run {|
let s = Set.of_list ["a"; "b"; "c"]
let _ = print (Set.mem "a" s)
let _ = print (Set.mem "d" s)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.exists finds matching element" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3; 4; 5]
let _ = print (Set.exists (fun x -> x > 4) s)
let _ = print (Set.exists (fun x -> x > 10) s)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.for_all checks all elements" =
  print_endline (compile_and_run {|
let s = Set.of_list [2; 4; 6; 8]
let _ = print (Set.for_all (fun x -> x mod 2 == 0) s)
let s2 = Set.add 3 s
let _ = print (Set.for_all (fun x -> x mod 2 == 0) s2)
|});
  [%expect {|
    true
    false |}]

let%expect_test "Set.partition splits by predicate" =
  print_endline (compile_and_run {|
let s = Set.of_list [1; 2; 3; 4; 5]
let pair = Set.partition (fun x -> x > 2) s
let _ = match pair with (yes, no) ->
  let _ = print (Set.size yes) in
  print (Set.size no)
|});
  [%expect {|
    3
    2 |}]

(** {1 Type Error Cases} *)

let%expect_test "set_add with mismatched element type fails" =
  print_endline (type_check {|
let s = set_add 1 (set_empty ())
let s2 = set_add "string" s
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:22

       2 | let s = set_add 1 (set_empty ())
       3 | let s2 = set_add "string" s
                                    ^
                                    expected `string`, found `int`

    This function's 2nd argument expects type `string set`, but you gave it `int set`.

    note: The problem is in type argument 1.

    note: To convert an int to a string, use `string_of_int value`
    |}]

let%expect_test "set_mem with mismatched element type fails" =
  print_endline (type_check {|
let s = set_add 1 (set_empty ())
let b = set_mem "string" s
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:3:26

       2 | let s = set_add 1 (set_empty ())
       3 | let b = set_mem "string" s
                                    ^
                                    expected `string`, found `int`

    This function's 2nd argument expects type `string set`, but you gave it `int set`.

    note: The problem is in type argument 1.

    note: To convert an int to a string, use `string_of_int value`
    |}]

let%expect_test "Set.union with mismatched element types fails" =
  print_endline (type_check {|
let s1 = Set.of_list [1; 2; 3]
let s2 = Set.of_list ["a"; "b"]
let s3 = Set.union s1 s2
|});
  [%expect {|
    ERROR: error: Type Mismatch --> <test>:4:23

       3 | let s2 = Set.of_list ["a"; "b"]
       4 | let s3 = Set.union s1 s2
                                 ^
                                 expected `string`, found `int`

    This function's 2nd argument expects type `string set`, but you gave it `int set`.

    note: The problem is in type argument 1.

    note: To convert an int to a string, use `string_of_int value`
    |}]
