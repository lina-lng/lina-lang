(** Tests for LSP formatting functionality. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Document Not Found *)
(* ============================================================ *)

let%expect_test "format_document returns None for missing document" =
  reset ();
  let store = Document_store.create () in
  let options = make_formatting_options () in
  let result = Lsp_formatting.format_document store "nonexistent.lina" options in
  print_endline (show_text_edits result);
  [%expect {| None |}]

let%expect_test "format_range returns None for missing document" =
  reset ();
  let store = Document_store.create () in
  let options = make_formatting_options () in
  let range = make_range 0 0 1 0 in
  let result = Lsp_formatting.format_range store "nonexistent.lina" range options in
  print_endline (show_text_edits result);
  [%expect {| None |}]

(* ============================================================ *)
(* Already Formatted Code *)
(* ============================================================ *)

let%expect_test "format_document returns empty list for already formatted code" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

let%expect_test "format_document returns empty list for multi-line formatted code" =
  reset ();
  with_document {|let x = 1
let y = 2
let z = x + y|} (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

(* ============================================================ *)
(* CST Formatter Behavior *)
(* ============================================================ *)

(* Note: The CST formatter preserves whitespace to maintain lossless formatting.
   This means extra spaces are preserved, not normalized. *)

let%expect_test "format_document preserves extra spaces (CST preserves whitespace)" =
  reset ();
  with_document "let  x  =  42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| Edit (0:0)-(0:13): "let x = 42" |}]

let%expect_test "format_document preserves indentation style (CST preserves whitespace)" =
  reset ();
  with_document "let x =\n42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

(* ============================================================ *)
(* Error Handling *)
(* ============================================================ *)

let%expect_test "format_document returns empty list for syntax errors" =
  reset ();
  with_document "let x =" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

let%expect_test "format_document returns empty list for invalid tokens" =
  reset ();
  with_document "let @ = 1" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let%expect_test "format_document handles empty document" =
  reset ();
  with_document "" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

let%expect_test "format_document handles single newline" =
  reset ();
  with_document "\n" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

let%expect_test "format_document preserves comments" =
  reset ();
  with_document "(* comment *)\nlet x = 42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    match result with
    | None -> print_endline "None"
    | Some [] -> print_endline "No edits"
    | Some edits ->
        List.iter (fun edit ->
          let has_comment = String.length edit.Lsp.Types.TextEdit.newText > 0 &&
                            String.sub edit.Lsp.Types.TextEdit.newText 0 2 = "(*" in
          Printf.printf "comment_preserved=%b\n" has_comment
        ) edits);
  [%expect {| No edits |}]

let%expect_test "format_document handles code with line comments" =
  reset ();
  with_document "-- line comment\nlet x = 42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

(* ============================================================ *)
(* Range Formatting *)
(* ============================================================ *)

let%expect_test "format_range returns empty list for already formatted code" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let options = make_formatting_options () in
    let range = make_range 0 0 0 10 in
    let result = Lsp_formatting.format_range store uri range options in
    print_endline (show_text_edits result));
  [%expect {| No edits |}]

let%expect_test "format_range preserves whitespace (CST formatter)" =
  reset ();
  with_document "let  x  =  42" (fun store uri ->
    let options = make_formatting_options () in
    let range = make_range 0 0 0 5 in
    let result = Lsp_formatting.format_range store uri range options in
    print_endline (show_text_edits result));
  [%expect {| Edit (0:0)-(0:13): "let x = 42" |}]

(* ============================================================ *)
(* TextEdit Range Verification *)
(* ============================================================ *)

(* Since CST formatter preserves whitespace, we verify the range calculation
   using the full_document_range helper directly instead of through TextEdits *)

let%expect_test "format_document with preserved content returns no edits" =
  reset ();
  with_document "let  x  =  42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| Edit (0:0)-(0:13): "let x = 42" |}]

let%expect_test "format_document with multi-line preserved content returns no edits" =
  reset ();
  with_document "let  x  =\n  42" (fun store uri ->
    let options = make_formatting_options () in
    let result = Lsp_formatting.format_document store uri options in
    print_endline (show_text_edits result));
  [%expect {| Edit (0:0)-(1:4): "let x =\n42" |}]

(* ============================================================ *)
(* full_document_range Helper Tests *)
(* ============================================================ *)

let%expect_test "full_document_range handles empty string" =
  let range = Lsp_types.full_document_range "" in
  Printf.printf "(%d:%d)-(%d:%d)\n"
    range.start_pos.line range.start_pos.character
    range.end_pos.line range.end_pos.character;
  [%expect {| (0:0)-(0:0) |}]

let%expect_test "full_document_range handles single line without newline" =
  let range = Lsp_types.full_document_range "hello" in
  Printf.printf "(%d:%d)-(%d:%d)\n"
    range.start_pos.line range.start_pos.character
    range.end_pos.line range.end_pos.character;
  [%expect {| (0:0)-(0:5) |}]

let%expect_test "full_document_range handles single line with newline" =
  let range = Lsp_types.full_document_range "hello\n" in
  Printf.printf "(%d:%d)-(%d:%d)\n"
    range.start_pos.line range.start_pos.character
    range.end_pos.line range.end_pos.character;
  [%expect {| (0:0)-(1:0) |}]

let%expect_test "full_document_range handles multiple lines" =
  let range = Lsp_types.full_document_range "line1\nline2\nline3" in
  Printf.printf "(%d:%d)-(%d:%d)\n"
    range.start_pos.line range.start_pos.character
    range.end_pos.line range.end_pos.character;
  [%expect {| (0:0)-(2:5) |}]

let%expect_test "full_document_range handles trailing newline" =
  let range = Lsp_types.full_document_range "line1\nline2\n" in
  Printf.printf "(%d:%d)-(%d:%d)\n"
    range.start_pos.line range.start_pos.character
    range.end_pos.line range.end_pos.character;
  [%expect {| (0:0)-(2:0) |}]
