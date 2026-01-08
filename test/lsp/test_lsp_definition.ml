(** Tests for LSP go-to-definition functionality.

    Note: The definition feature is currently a stub that always returns None.
    These tests document the expected behavior and verify the stub works correctly. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Current Stub Behavior *)
(* ============================================================ *)

let%expect_test "definition returns None for variable (stub)" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let result = Lsp_definition.find_definition store uri (pos 0 4) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "definition returns None for function call (stub)" =
  reset ();
  with_document {|let f x = x + 1
let y = f 42|} (fun store uri ->
    (* Try to go to definition of 'f' in 'f 42' *)
    let result = Lsp_definition.find_definition store uri (pos 1 8) in
    match result with
    | Some _ -> print_endline "Found definition"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "definition returns None for module member (stub)" =
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
  [%expect {| None |}]

let%expect_test "definition returns None for type name (stub)" =
  reset ();
  with_document {|type t = int
let x : t = 42|} (fun store uri ->
    (* Try to go to definition of type 't' *)
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

(* ============================================================ *)
(* Definition Result Type Documentation *)
(* ============================================================ *)

(* When implemented, definition should return:
   - definition_result with uri and range pointing to the definition site
   - For local variables: the let binding pattern
   - For function parameters: the parameter in the function signature
   - For module members: the definition inside the module
   - For types: the type declaration

   Example expected behavior (not yet implemented):

   let%expect_test "definition for local variable" =
     with_document "let x = 42\nlet y = x" (fun store uri ->
       let result = Lsp_definition.find_definition store uri (pos 1 8) in
       match result with
       | Some def ->
           Printf.printf "uri=%s line=%d char=%d"
             def.uri
             def.range.start_pos.line
             def.range.start_pos.character
       | None -> print_endline "None");
     [%expect {| uri=test.lina line=0 char=4 |}]
*)
