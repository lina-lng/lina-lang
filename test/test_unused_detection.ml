(** Tests for unused code detection.

    Tests the unused_detector module which finds unused bindings
    based on scope analysis results.

    Note: These tests verify the actual detection behavior. Some unused
    detection categories (parameters, fields) may have limited coverage
    due to scope analyzer limitations. *)

module Warning_config = Common.Warning_config
module Compiler_error = Common.Compiler_error
module Error_code = Common.Error_code

let string_starts_with = Analysis_test_helpers.string_starts_with
let string_contains = Analysis_test_helpers.string_contains

let analyze_unused ?(strict = true) source =
  let typed_ast = Analysis_test_helpers.type_check source in
  let scope_tree = Analysis.Scope_analyzer.analyze typed_ast in
  let config =
    if strict then Warning_config.default
    else Warning_config.disable_all Warning_config.default
  in
  let result = Analysis.Unused_detector.detect config scope_tree in
  result.diagnostics

let diagnostic_messages = Analysis_test_helpers.diagnostic_messages
let has_diagnostic_code = Analysis_test_helpers.has_diagnostic_code

let%expect_test "unused variable detected" =
  let diags = analyze_unused "let x = 42" in
  Printf.printf "Diagnostic count: %d\n" (List.length diags);
  List.iter (fun msg -> Printf.printf "Message: %s\n" msg) (diagnostic_messages diags);
  [%expect{|
    Diagnostic count: 1
    Message: unused variable `x`
    |}]

let%expect_test "used variable not flagged" =
  let diags = analyze_unused "let x = 42 let y = x + 1" in
  (* x is used by y, so only y should be flagged *)
  let unused_msgs = List.filter (fun msg -> string_contains ~substring:"x" msg) (diagnostic_messages diags) in
  Printf.printf "Unused x messages: %d\n" (List.length unused_msgs);
  [%expect{|
    Unused x messages: 0
    |}]

let%expect_test "multiple unused variables" =
  let diags = analyze_unused "let a = 1 let b = 2 let c = 3" in
  Printf.printf "Diagnostic count: %d\n" (List.length diags);
  List.iter (fun msg -> Printf.printf "  %s\n" msg) (diagnostic_messages diags);
  [%expect{|
    Diagnostic count: 3
      unused variable `a`
      unused variable `b`
      unused variable `c`
    |}]

let%expect_test "unused function detected" =
  let diags = analyze_unused "let f x = x + 1" in
  let has_unused = List.exists (fun msg ->
    string_contains ~substring:"f" msg
  ) (diagnostic_messages diags) in
  Printf.printf "Has unused f: %b\n" has_unused;
  [%expect{|
    Has unused f: true
    |}]

let%expect_test "used function not flagged" =
  let diags = analyze_unused "let f x = x + 1 let y = f 5" in
  let unused_f = List.exists (fun (d : Compiler_error.diagnostic) ->
    string_starts_with ~prefix:"unused" d.message &&
    string_contains ~substring:"f" d.message
  ) diags in
  Printf.printf "Has unused f: %b\n" unused_f;
  [%expect{|
    Has unused f: false
    |}]

let%expect_test "used recursive function not flagged" =
  let diags = analyze_unused {|
    let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
    let result = fact 5
  |} in
  let unused_fact = List.exists (fun (d : Compiler_error.diagnostic) ->
    string_starts_with ~prefix:"unused" d.message &&
    string_contains ~substring:"fact" d.message
  ) diags in
  Printf.printf "Has unused fact: %b\n" unused_fact;
  [%expect{|
    Has unused fact: false
    |}]

let%expect_test "unused type detected" =
  let diags = analyze_unused "type t = int" in
  Printf.printf "Has unused type: %b\n"
    (has_diagnostic_code Error_code.w_unused_type diags);
  [%expect{|
    Has unused type: true
    |}]

(* NOTE: Type reference tracking from annotations and type definitions is
   limited because type aliases are fully resolved during type inference.
   The typed AST stores resolved types, not syntactic references.

   For example:
   - "type t = int let x : t = 42" - the annotation `: t` is resolved to `int`
   - "type t = int type s = t * t" - s's manifest is `int * int`, not `t * t`

   This is a known limitation of tracking at the typed AST level. Types will
   be flagged as unused even if they are referenced in other type definitions
   or annotations, unless the reference survives type resolution. *)
let%expect_test "type alias tracking limitation" =
  let diags = analyze_unused "type t = int type s = t" in
  let unused_t = List.exists (fun (d : Compiler_error.diagnostic) ->
    string_starts_with ~prefix:"unused type" d.message &&
    string_contains ~substring:"`t`" d.message
  ) diags in
  (* t is flagged as unused because its reference in s's definition is resolved *)
  Printf.printf "Has unused type t: %b\n" unused_t;
  [%expect{|
    Has unused type t: true
    |}]

let%expect_test "unused module detected" =
  let diags = analyze_unused "module M = struct let x = 42 end" in
  Printf.printf "Has unused module: %b\n"
    (has_diagnostic_code Error_code.w_unused_module diags);
  [%expect{|
    Has unused module: true
    |}]

let%expect_test "used module not flagged" =
  let diags = analyze_unused {|
    module M = struct let x = 42 end
    let y = M.x
  |} in
  let unused_module = List.exists (fun (d : Compiler_error.diagnostic) ->
    string_starts_with ~prefix:"unused module" d.message &&
    string_contains ~substring:"M" d.message
  ) diags in
  Printf.printf "Has unused M: %b\n" unused_module;
  [%expect{|
    Has unused M: false
    |}]

let%expect_test "unused open detected" =
  let diags = analyze_unused {|
    module M = struct let x = 42 end
    open M
  |} in
  Printf.printf "Has unused open: %b\n"
    (has_diagnostic_code Error_code.w_unused_open diags);
  [%expect{|
    Has unused open: true
    |}]

let%expect_test "used open not flagged" =
  let diags = analyze_unused {|
    module M = struct let x = 42 end
    open M
    let y = x
  |} in
  let unused_open = List.exists (fun (d : Compiler_error.diagnostic) ->
    string_contains ~substring:"unused open" d.message
  ) diags in
  Printf.printf "Has unused open: %b\n" unused_open;
  [%expect{|
    Has unused open: false
    |}]

let%expect_test "strict mode produces errors" =
  let diags = analyze_unused ~strict:true "let x = 42" in
  (* Find the diagnostic for unused x *)
  let unused_x = List.find_opt (fun (d : Compiler_error.diagnostic) ->
    string_contains ~substring:"`x`" d.message
  ) diags in
  (match unused_x with
   | Some d ->
       Printf.printf "Severity: %s\n"
         (match d.severity with
          | Compiler_error.Error -> "error"
          | Compiler_error.Warning -> "warning"
          | _ -> "other")
   | None -> print_endline "Diagnostic for x not found");
  [%expect{|
    Severity: error
    |}]

let%expect_test "shadowing variable is still unused if original is unused" =
  let diags = analyze_unused {|
    let x = 1
    let x = 2
    let _ = x
  |} in
  let unused_count = List.length diags in
  Printf.printf "Unused count: %d\n" unused_count;
  [%expect{|
    Unused count: 1
    |}]

let%expect_test "mutual recursion both used" =
  let diags = analyze_unused {|
    let rec even n = if n = 0 then true else odd (n - 1)
    and odd n = if n = 0 then false else even (n - 1)
    let _ = even 5
  |} in
  (* Count only function-related unused diagnostics, not parameter ones *)
  let unused_funcs = List.filter (fun (d : Compiler_error.diagnostic) ->
    string_contains ~substring:"function" d.message
  ) diags in
  Printf.printf "Unused function count: %d\n" (List.length unused_funcs);
  [%expect{|
    Unused function count: 0
    |}]
