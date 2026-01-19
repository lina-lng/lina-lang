(** Tests for the analysis pipeline integration.

    Tests that the analysis pipeline correctly combines scope analysis,
    unused detection, and dead code detection. *)

module Warning_config = Common.Warning_config
module Error_code = Common.Error_code

(** Helper to run the full analysis pipeline on source code. *)
let analyze_code ?(strict = true) source =
  Typing.Types.reset_type_variable_id ();
  let ast = Parsing.Parse.structure_from_string source in
  let ctx = Typing.Typing_context.create Typing.Environment.initial in
  let typed_ast, _ctx = Typing.Inference.infer_structure ctx ast in
  let config =
    if strict then Warning_config.default
    else Warning_config.disable_all Warning_config.default
  in
  Analysis.Analysis_pipeline.run config typed_ast

(** Helper to check if a diagnostic with a specific code is present. *)
let has_diagnostic_code code result =
  List.exists
    (fun (d : Common.Compiler_error.diagnostic) ->
      match d.code with Some c -> Error_code.equal c code | None -> false)
    result.Analysis.Analysis_pipeline.diagnostics

(** Helper to check if a diagnostic with a specific message substring is present. *)
let has_diagnostic_message substring result =
  List.exists
    (fun (d : Common.Compiler_error.diagnostic) ->
      let len = String.length substring in
      let msg_len = String.length d.message in
      if len > msg_len then false
      else
        let rec check idx =
          if idx > msg_len - len then false
          else if String.sub d.message idx len = substring then true
          else check (idx + 1)
        in
        check 0)
    result.Analysis.Analysis_pipeline.diagnostics

(* ==================== Pipeline Integration ==================== *)

let%expect_test "pipeline detects unused variable" =
  let result = analyze_code "let x = 42" in
  Printf.printf "Has errors: %b\n" result.has_errors;
  Printf.printf "Has unused variable: %b\n"
    (has_diagnostic_code Error_code.w_unused_variable result);
  [%expect {|
    Has errors: true
    Has unused variable: true
    |}]

let%expect_test "pipeline detects unused function" =
  let result = analyze_code "let f x = x + 1" in
  Printf.printf "Has unused function: %b\n"
    (has_diagnostic_code Error_code.w_unused_function result);
  [%expect {|
    Has unused function: true
    |}]

let%expect_test "pipeline detects dead code" =
  let result =
    analyze_code
      {|
    external raise : string -> 'a = "error"
    let f x =
      raise "error";
      x + 1
  |}
  in
  Printf.printf "Has dead code: %b\n"
    (has_diagnostic_code Error_code.w_dead_code result);
  [%expect {|
    Has dead code: true
    |}]

let%expect_test "pipeline detects unused module" =
  let result = analyze_code "module M = struct let x = 42 end" in
  Printf.printf "Has unused module: %b\n"
    (has_diagnostic_code Error_code.w_unused_module result);
  [%expect {|
    Has unused module: true
    |}]

let%expect_test "pipeline combines multiple diagnostics" =
  let result =
    analyze_code
      {|
    external raise : string -> 'a = "error"
    let x = 42
    let f y =
      raise "error";
      y + 1
    module M = struct let z = 0 end
  |}
  in
  Printf.printf "Diagnostic count: %d\n"
    (List.length result.Analysis.Analysis_pipeline.diagnostics);
  Printf.printf "Has unused x: %b\n" (has_diagnostic_message "`x`" result);
  Printf.printf "Has unused f: %b\n" (has_diagnostic_message "`f`" result);
  Printf.printf "Has unused M: %b\n" (has_diagnostic_message "`M`" result);
  Printf.printf "Has dead code: %b\n"
    (has_diagnostic_code Error_code.w_dead_code result);
  (* Note: count includes unused y parameter and unused z inside M *)
  [%expect {|
    Diagnostic count: 5
    Has unused x: true
    Has unused f: true
    Has unused M: true
    Has dead code: true
    |}]

let%expect_test "pipeline respects relaxed mode" =
  let result = analyze_code ~strict:false "let x = 42" in
  Printf.printf "Has errors: %b\n" result.has_errors;
  Printf.printf "Diagnostic count: %d\n"
    (List.length result.Analysis.Analysis_pipeline.diagnostics);
  [%expect {|
    Has errors: false
    Diagnostic count: 0
    |}]

let%expect_test "pipeline no diagnostics for clean code" =
  let result = analyze_code "let x = 42 let y = x + 1 let _ = y" in
  (* Only unused diagnostics, no dead code *)
  Printf.printf "Diagnostic count: %d\n"
    (List.length result.Analysis.Analysis_pipeline.diagnostics);
  [%expect {|
    Diagnostic count: 0
    |}]

let%expect_test "pipeline returns scope tree" =
  let result = analyze_code "let x = 42 let y = x + 1" in
  Printf.printf "Total bindings: %d\n"
    (List.length result.scope_tree.Analysis.Scope.tree_all_bindings);
  Printf.printf "Total scopes: %d\n"
    (List.length result.scope_tree.Analysis.Scope.tree_all_scopes);
  [%expect {|
    Total bindings: 2
    Total scopes: 1
    |}]

(* ==================== Error Classification ==================== *)

let%expect_test "strict mode makes unused code errors" =
  let result = analyze_code ~strict:true "let x = 42" in
  let first_diag = List.hd result.Analysis.Analysis_pipeline.diagnostics in
  Printf.printf "Severity: %s\n"
    (match first_diag.Common.Compiler_error.severity with
     | Common.Compiler_error.Error -> "error"
     | Common.Compiler_error.Warning -> "warning"
     | _ -> "other");
  [%expect {|
    Severity: error
    |}]
