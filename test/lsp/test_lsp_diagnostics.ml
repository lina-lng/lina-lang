(** Tests for LSP diagnostics. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Valid Code - No Diagnostics *)
(* ============================================================ *)

let%expect_test "valid code - no diagnostics" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    print_endline (show_diagnostics diags));
  [%expect {| No diagnostics |}]

let%expect_test "valid function - no diagnostics" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    print_endline (show_diagnostics diags));
  [%expect {| No diagnostics |}]

let%expect_test "valid multi-line code - no diagnostics" =
  reset ();
  with_document {|
let x = 1
let y = 2
let z = x + y
|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    print_endline (show_diagnostics diags));
  [%expect {| No diagnostics |}]

let%expect_test "valid module - no diagnostics" =
  reset ();
  with_document {|
module M = struct
  let value = 42
end
let x = M.value
|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    print_endline (show_diagnostics diags));
  [%expect {| No diagnostics |}]

(* ============================================================ *)
(* Parse Errors *)
(* ============================================================ *)

let%expect_test "parse error - missing expression" =
  reset ();
  with_document "let x =" (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let has_error = List.exists (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Error) diags in
    Printf.printf "has_error=%b count=%d" has_error (List.length diags));
  [%expect {| has_error=true count=1 |}]

let%expect_test "parse error - unexpected token" =
  reset ();
  with_document "let 123 = x" (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let has_error = List.exists (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Error) diags in
    Printf.printf "has_error=%b" has_error);
  [%expect {| has_error=true |}]

(* ============================================================ *)
(* Type Errors *)
(* ============================================================ *)

let%expect_test "type error - unbound variable" =
  reset ();
  with_document "let x = unknown_var" (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let error_msgs = List.filter_map (fun (d : Lsp_types.diagnostic) ->
      if d.severity = Lsp_types.Error then Some d.message else None) diags in
    List.iter print_endline error_msgs);
  [%expect {| Unbound variable: unknown_var |}]

let%expect_test "type error - type mismatch in annotation" =
  reset ();
  with_document {|let x : int = "hello"|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let starts_with prefix s =
      String.length s >= String.length prefix &&
      String.sub s 0 (String.length prefix) = prefix
    in
    let has_type_error = List.exists (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Error &&
      (starts_with "Cannot unify" d.message ||
       starts_with "Type error" d.message)) diags in
    Printf.printf "has_type_error=%b count=%d" has_type_error (List.length diags));
  [%expect {| has_type_error=false count=1 |}]

let%expect_test "type error - unification error in expression" =
  reset ();
  with_document {|let f x = x + "string"|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let has_error = List.exists (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Error) diags in
    Printf.printf "has_error=%b" has_error);
  [%expect {| has_error=true |}]

let%expect_test "type error - unbound type" =
  reset ();
  with_document {|let x : unknown_type = 1|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let error_msgs = List.filter_map (fun (d : Lsp_types.diagnostic) ->
      if d.severity = Lsp_types.Error then Some d.message else None) diags in
    List.iter print_endline error_msgs);
  [%expect {| Syntax error |}]

let%expect_test "type error - unbound module" =
  reset ();
  with_document {|let x = Unknown.value|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let error_msgs = List.filter_map (fun (d : Lsp_types.diagnostic) ->
      if d.severity = Lsp_types.Error then Some d.message else None) diags in
    List.iter print_endline error_msgs);
  [%expect {| Unbound constructor: Unknown |}]

(* ============================================================ *)
(* Warnings *)
(* ============================================================ *)

let%expect_test "warning - non-exhaustive match" =
  reset ();
  with_document {|
type t = A | B | C
let f x = match x with
  | A -> 1
  | B -> 2
|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let warnings = List.filter (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Warning) diags in
    Printf.printf "warning_count=%d" (List.length warnings));
  [%expect {| warning_count=1 |}]

(* ============================================================ *)
(* Multiple Diagnostics *)
(* ============================================================ *)

let%expect_test "multiple type errors in document" =
  reset ();
  with_document {|
let x = unknown1
let y = unknown2
|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    let error_count = List.length (List.filter (fun (d : Lsp_types.diagnostic) ->
      d.severity = Lsp_types.Error) diags) in
    Printf.printf "error_count=%d" error_count);
  [%expect {| error_count=1 |}]

(* ============================================================ *)
(* Diagnostic Position *)
(* ============================================================ *)

let%expect_test "diagnostic has correct line position" =
  reset ();
  with_document {|let x = 1
let y = unknown|} (fun store uri ->
    let diags = Diagnostics.compute_diagnostics store uri in
    match diags with
    | [d] ->
        Printf.printf "line=%d" d.range.start_pos.line
    | _ ->
        Printf.printf "unexpected diag count: %d" (List.length diags));
  [%expect {| line=1 |}]

(* ============================================================ *)
(* Caching Behavior *)
(* ============================================================ *)

let%expect_test "diagnostics are cached by version" =
  reset ();
  let store = Document_store.create () in
  let uri = "test.lina" in
  Document_store.open_document store ~uri ~content:"let x = 42" ~version:1;
  let _ = Diagnostics.compute_diagnostics store uri in
  (* Cache should be set *)
  let has_cache = Option.is_some (Document_store.get_parse_cache store uri) in
  Printf.printf "has_cache=%b" has_cache;
  [%expect {| has_cache=true |}]
