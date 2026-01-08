(** Tests for LSP document store. *)

open Lina_lsp

(* ============================================================ *)
(* Document Lifecycle Tests *)
(* ============================================================ *)

let%expect_test "open_document - stores content" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  (match Document_store.document_content store "test.lina" with
   | Some content -> print_endline content
   | None -> print_endline "Not found");
  [%expect {| let x = 1 |}]

let%expect_test "open_document - get_document returns document" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:5;
  (match Document_store.get_document store "test.lina" with
   | Some doc -> Printf.printf "version=%d content_len=%d" doc.version (String.length doc.content)
   | None -> print_endline "Not found");
  [%expect {| version=5 content_len=9 |}]

let%expect_test "close_document - removes document" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  Document_store.close_document store ~uri:"test.lina";
  (match Document_store.get_document store "test.lina" with
   | Some _ -> print_endline "Still exists"
   | None -> print_endline "Removed");
  [%expect {| Removed |}]

let%expect_test "update_document - updates content and version" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  Document_store.update_document store ~uri:"test.lina" ~content:"let x = 2" ~version:2;
  (match Document_store.get_document store "test.lina" with
   | Some doc -> Printf.printf "version=%d content=%s" doc.version doc.content
   | None -> print_endline "Not found");
  [%expect {| version=2 content=let x = 2 |}]

let%expect_test "update_document - non-existent creates new" =
  let store = Document_store.create () in
  Document_store.update_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  (match Document_store.document_content store "test.lina" with
   | Some content -> print_endline content
   | None -> print_endline "Not found");
  [%expect {| let x = 1 |}]

let%expect_test "get_all_uris - returns all open documents" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"a.lina" ~content:"a" ~version:1;
  Document_store.open_document store ~uri:"b.lina" ~content:"b" ~version:1;
  Document_store.open_document store ~uri:"c.lina" ~content:"c" ~version:1;
  let uris = Document_store.get_all_uris store |> List.sort String.compare in
  List.iter print_endline uris;
  [%expect {|
    a.lina
    b.lina
    c.lina
    |}]

(* ============================================================ *)
(* Position Conversion Tests *)
(* ============================================================ *)

let%expect_test "position_to_offset - single line" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 42" ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (match Document_store.position_to_offset doc 0 4 with
        | Some offset -> Printf.printf "offset=%d char='%c'" offset doc.content.[offset]
        | None -> print_endline "Invalid position")
   | None -> print_endline "No document");
  [%expect {| offset=4 char='x' |}]

let%expect_test "position_to_offset - multi-line" =
  let store = Document_store.create () in
  let content = "let x = 1\nlet y = 2\nlet z = 3" in
  Document_store.open_document store ~uri:"test.lina" ~content ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (* Line 1 (second line), char 4 should be 'y' *)
       (match Document_store.position_to_offset doc 1 4 with
        | Some offset -> Printf.printf "line1:4 offset=%d char='%c'" offset doc.content.[offset]
        | None -> print_endline "Invalid position")
   | None -> print_endline "No document");
  [%expect {| line1:4 offset=14 char='y' |}]

let%expect_test "position_to_offset - line start" =
  let store = Document_store.create () in
  let content = "line1\nline2\nline3" in
  Document_store.open_document store ~uri:"test.lina" ~content ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (match Document_store.position_to_offset doc 2 0 with
        | Some offset -> Printf.printf "offset=%d char='%c'" offset doc.content.[offset]
        | None -> print_endline "Invalid position")
   | None -> print_endline "No document");
  [%expect {| offset=12 char='l' |}]

let%expect_test "position_to_offset - invalid line returns None" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (match Document_store.position_to_offset doc 5 0 with
        | Some _ -> print_endline "Got offset"
        | None -> print_endline "None")
   | None -> print_endline "No document");
  [%expect {| None |}]

let%expect_test "offset_to_position - single line" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 42" ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (match Document_store.offset_to_position doc 4 with
        | Some (line, char) -> Printf.printf "line=%d char=%d" line char
        | None -> print_endline "Invalid offset")
   | None -> print_endline "No document");
  [%expect {| line=0 char=4 |}]

let%expect_test "offset_to_position - multi-line" =
  let store = Document_store.create () in
  let content = "let x = 1\nlet y = 2" in
  Document_store.open_document store ~uri:"test.lina" ~content ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       (* Offset 14 should be 'y' on line 1 *)
       (match Document_store.offset_to_position doc 14 with
        | Some (line, char) -> Printf.printf "line=%d char=%d" line char
        | None -> print_endline "Invalid offset")
   | None -> print_endline "No document");
  [%expect {| line=1 char=4 |}]

let%expect_test "lsp_position_to_offset - uses Lsp_types.position" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 42" ~version:1;
  (match Document_store.get_document store "test.lina" with
   | Some doc ->
       let pos : Lsp_types.position = { line = 0; character = 4 } in
       (match Document_store.lsp_position_to_offset doc pos with
        | Some offset -> Printf.printf "offset=%d" offset
        | None -> print_endline "Invalid position")
   | None -> print_endline "No document");
  [%expect {| offset=4 |}]

(* ============================================================ *)
(* Cache Management Tests *)
(* ============================================================ *)

let%expect_test "parse_cache - set and get" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  let cache : Document_store.parse_cache = {
    parse_version = 1;
    ast = None;
    parse_errors = [];
  } in
  Document_store.set_parse_cache store ~uri:"test.lina" cache;
  (match Document_store.get_parse_cache store "test.lina" with
   | Some c -> Printf.printf "version=%d" c.parse_version
   | None -> print_endline "No cache");
  [%expect {| version=1 |}]

let%expect_test "parse_cache - stale version returns None" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  let cache : Document_store.parse_cache = {
    parse_version = 1;
    ast = None;
    parse_errors = [];
  } in
  Document_store.set_parse_cache store ~uri:"test.lina" cache;
  (* Update document to version 2 *)
  Document_store.update_document store ~uri:"test.lina" ~content:"let x = 2" ~version:2;
  (match Document_store.get_parse_cache store "test.lina" with
   | Some _ -> print_endline "Got cache"
   | None -> print_endline "None (stale)");
  [%expect {| None (stale) |}]

let%expect_test "typing_cache - set and get" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  let cache : Document_store.typing_cache = {
    typing_version = 1;
    typed_ast = None;
    environment = Typing.Environment.empty;
    type_errors = [];
    warnings = [];
  } in
  Document_store.set_typing_cache store ~uri:"test.lina" cache;
  (match Document_store.get_typing_cache store "test.lina" with
   | Some c -> Printf.printf "version=%d" c.typing_version
   | None -> print_endline "No cache");
  [%expect {| version=1 |}]

let%expect_test "invalidate_caches - clears both caches" =
  let store = Document_store.create () in
  Document_store.open_document store ~uri:"test.lina" ~content:"let x = 1" ~version:1;
  let parse_cache : Document_store.parse_cache = {
    parse_version = 1;
    ast = None;
    parse_errors = [];
  } in
  let typing_cache : Document_store.typing_cache = {
    typing_version = 1;
    typed_ast = None;
    environment = Typing.Environment.empty;
    type_errors = [];
    warnings = [];
  } in
  Document_store.set_parse_cache store ~uri:"test.lina" parse_cache;
  Document_store.set_typing_cache store ~uri:"test.lina" typing_cache;
  Document_store.invalidate_caches store ~uri:"test.lina";
  let has_parse = Option.is_some (Document_store.get_parse_cache store "test.lina") in
  let has_typing = Option.is_some (Document_store.get_typing_cache store "test.lina") in
  Printf.printf "parse=%b typing=%b" has_parse has_typing;
  [%expect {| parse=false typing=false |}]

(* ============================================================ *)
(* URI Handling Tests *)
(* ============================================================ *)

let%expect_test "filename_of_uri - extracts path from file:// URI" =
  let path = Document_store.filename_of_uri "file:///home/user/test.lina" in
  print_endline path;
  [%expect {| /home/user/test.lina |}]

let%expect_test "filename_of_uri - returns non-file URI as-is" =
  let path = Document_store.filename_of_uri "/home/user/test.lina" in
  print_endline path;
  [%expect {| /home/user/test.lina |}]

let%expect_test "uri_of_filename - creates file:// URI" =
  let uri = Document_store.uri_of_filename "/home/user/test.lina" in
  print_endline uri;
  [%expect {| file:///home/user/test.lina |}]
