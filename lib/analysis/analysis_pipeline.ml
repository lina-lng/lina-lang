open Common

type result = {
  scope_tree : Scope.scope_tree;
  diagnostics : Compiler_error.diagnostic list;
  has_errors : bool;
}

let run_with_signature config typed_ast signature =
  let scope_tree = Scope_analyzer.analyze_with_signature typed_ast signature in

  let unused_result = Unused_detector.detect config scope_tree in

  let dead_code_findings = Dead_code.detect_in_structure typed_ast in
  let dead_code_diagnostics =
    if Common.Warning_config.should_report config Error_code.w_dead_code then
      List.map (Dead_code.to_diagnostic config) dead_code_findings
    else []
  in

  let all_diagnostics = unused_result.diagnostics @ dead_code_diagnostics in

  let has_errors =
    unused_result.has_errors
    || List.exists
         (fun d -> d.Compiler_error.severity = Compiler_error.Error)
         dead_code_diagnostics
  in

  { scope_tree; diagnostics = all_diagnostics; has_errors }

let run config typed_ast = run_with_signature config typed_ast None
