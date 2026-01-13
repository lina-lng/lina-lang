(** Tests for LSP go-to-definition functionality.

    Tests verify that go-to-definition correctly navigates from variable
    uses to their definitions.

    Now supports:
    - Top-level bindings
    - Function parameters
    - Local let bindings (including shadowing)
    - Match pattern bindings
    - Module member access (M.x)

    Still not supported:
    - Recursive function references inside their body
    - Type name navigation *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Top-level Binding Lookup (Supported) *)
(* ============================================================ *)

let%expect_test "definition for variable use returns definition location" =
  reset ();
  with_document {|let x = 42
let y = x|} (fun store uri ->
    (* Try to go to definition of 'x' in 'let y = x' (line 1, char 8) *)
    let result = Lsp_definition.find_definition store uri (pos 1 8) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=4 is the start of 'x' in 'let x = 42' (0-indexed) *)
  [%expect {| Found: line=0 char=4 |}]

let%expect_test "definition for function call returns function definition" =
  reset ();
  with_document {|let f x = x + 1
let y = f 42|} (fun store uri ->
    (* Try to go to definition of 'f' in 'f 42' *)
    let result = Lsp_definition.find_definition store uri (pos 1 8) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=4 is the start of 'f' in 'let f x = ...' (0-indexed) *)
  [%expect {| Found: line=0 char=4 |}]

let%expect_test "definition on definition site returns None" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    (* Cursor on the definition of 'x' itself - no navigation *)
    let result = Lsp_definition.find_definition store uri (pos 0 4) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

(* ============================================================ *)
(* Local Bindings (Now Supported) *)
(* ============================================================ *)

let%expect_test "definition for parameter use - identity function" =
  reset ();
  with_document "let id x = x" (fun store uri ->
    (* Try to go to definition of 'x' in body (line 0, char 11) *)
    let result = Lsp_definition.find_definition store uri (pos 0 11) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=7 is the start of 'x' parameter in 'let id x = x' (0-indexed) *)
  [%expect {| Found: line=0 char=7 |}]

let%expect_test "definition for parameter use - with infix" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    (* Try to go to definition of 'x' in 'x + 1' (line 0, char 10) *)
    let result = Lsp_definition.find_definition store uri (pos 0 10) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=6 is the start of 'x' parameter in 'let f x = x + 1' (0-indexed) *)
  [%expect {| Found: line=0 char=6 |}]

let%expect_test "definition in nested let - partially supported" =
  reset ();
  with_document {|let x = 1
let y =
  let x = 2 in
  x|} (fun store uri ->
    (* 'x' on line 3 - currently finds outer binding, not inner *)
    (* This is a limitation: inner let bindings shadow outer ones *)
    let result = Lsp_definition.find_definition store uri (pos 3 2) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* Currently returns outer 'x' (line 0) instead of inner 'x' (line 2) *)
  [%expect {| Found: line=2 char=6 |}]

let%expect_test "definition in function body for parameter" =
  reset ();
  with_document {|let f a b =
  let result = a + b in
  result|} (fun store uri ->
    (* 'a' on line 1 should point to parameter 'a' *)
    let result = Lsp_definition.find_definition store uri (pos 1 15) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=6 is the start of 'a' parameter in 'let f a b = ...' (0-indexed) *)
  [%expect {| Found: line=0 char=6 |}]

let%expect_test "definition for match pattern binding" =
  reset ();
  with_document {|let x = 42
let y = match x with
  | n -> n + 1|} (fun store uri ->
    (* 'n' in 'n + 1' should point to pattern binding 'n' *)
    let result = Lsp_definition.find_definition store uri (pos 2 9) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  (* char=4 is the start of 'n' pattern on line 2 (0-indexed) *)
  [%expect {| Found: line=2 char=4 |}]

let%expect_test "definition for recursive function call - not yet supported" =
  reset ();
  with_document {|let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1)|} (fun store uri ->
    (* 'fact' in recursive call should point to definition *)
    (* Recursive bindings are added after the expression is checked *)
    let result = Lsp_definition.find_definition store uri (pos 2 10) in
    match result with
    | Some def ->
        Printf.printf "Found: line=%d char=%d"
          def.range.start_pos.line
          def.range.start_pos.character
    | None -> print_endline "None");
  [%expect {| None |}]

(* ============================================================ *)
(* Module Access (Now Supported) *)
(* ============================================================ *)

let%expect_test "definition for module member" =
  reset ();
  with_document {|module M = struct
  let value = 42
end
let x = M.value|} (fun store uri ->
    (* Try to go to definition of 'M.value' *)
    let result = Lsp_definition.find_definition store uri (pos 3 10) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| Found definition |}]

(* ============================================================ *)
(* Type Names (Not Yet Supported) *)
(* ============================================================ *)

let%expect_test "definition for type name - not yet supported" =
  reset ();
  with_document {|type t = int
let x : t = 42|} (fun store uri ->
    (* Try to go to definition of type 't' - cursor on type annotation *)
    let result = Lsp_definition.find_definition store uri (pos 1 8) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let%expect_test "definition for invalid document returns None" =
  reset ();
  let store = Document_store.create () in
  let result = Lsp_definition.find_definition store "nonexistent.lina" (pos 0 0) in
  (match result with
   | Some _ -> print_endline "Found definition"
   | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "definition for empty document returns None" =
  reset ();
  with_document "" (fun store uri ->
    let result = Lsp_definition.find_definition store uri (pos 0 0) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "definition for document with parse error returns None" =
  reset ();
  with_document "let x =" (fun store uri ->
    let result = Lsp_definition.find_definition store uri (pos 0 4) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "definition for builtin returns None" =
  reset ();
  with_document "let x = print 42" (fun store uri ->
    (* 'print' is a builtin with no source location *)
    let result = Lsp_definition.find_definition store uri (pos 0 8) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]
