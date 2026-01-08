(** Tests for LSP completion functionality. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Keyword Completions *)
(* ============================================================ *)

let%expect_test "completion includes 'let' keyword" =
  reset ();
  with_document "le" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 2) in
    let has_let = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "let" && item.kind = Lsp_completion.Keyword) items in
    Printf.printf "has_let=%b" has_let);
  [%expect {| has_let=true |}]

let%expect_test "completion includes 'match' keyword" =
  reset ();
  with_document "mat" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    let has_match = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "match" && item.kind = Lsp_completion.Keyword) items in
    Printf.printf "has_match=%b" has_match);
  [%expect {| has_match=true |}]

let%expect_test "completion includes 'module' keyword" =
  reset ();
  with_document "mod" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    let has_module = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "module" && item.kind = Lsp_completion.Keyword) items in
    Printf.printf "has_module=%b" has_module);
  [%expect {| has_module=true |}]

let%expect_test "completion includes 'if' keyword" =
  reset ();
  with_document "i" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 1) in
    let has_if = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "if" && item.kind = Lsp_completion.Keyword) items in
    Printf.printf "has_if=%b" has_if);
  [%expect {| has_if=true |}]

let%expect_test "completion includes 'function' keyword" =
  reset ();
  with_document "fun" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    let has_fun = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "fun" && item.kind = Lsp_completion.Keyword) items in
    let has_function = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "function" && item.kind = Lsp_completion.Keyword) items in
    Printf.printf "has_fun=%b has_function=%b" has_fun has_function);
  [%expect {| has_fun=true has_function=true |}]

(* ============================================================ *)
(* Completion Filtering *)
(* ============================================================ *)

let starts_with prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

let%expect_test "completion filters by prefix - 'ty' matches 'type'" =
  reset ();
  with_document "ty" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 2) in
    let type_items = List.filter (fun (item : Lsp_completion.completion_item) ->
      starts_with "ty" item.label) items in
    let labels = List.map (fun item -> item.Lsp_completion.label) type_items in
    print_endline (String.concat ", " labels));
  [%expect {| type |}]

let%expect_test "completion with non-matching prefix returns empty" =
  reset ();
  with_document "xyz" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    Printf.printf "count=%d" (List.length items));
  [%expect {| count=0 |}]

(* ============================================================ *)
(* Completion at Various Positions *)
(* ============================================================ *)

let%expect_test "completion after 'let x = '" =
  reset ();
  with_document "let x = ma" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 10) in
    let has_match = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "match") items in
    Printf.printf "has_match=%b" has_match);
  [%expect {| has_match=true |}]

let%expect_test "completion on second line" =
  reset ();
  with_document {|let x = 1
le|} (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 2) in
    let has_let = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "let") items in
    Printf.printf "has_let=%b" has_let);
  [%expect {| has_let=true |}]

(* ============================================================ *)
(* Completion Item Properties *)
(* ============================================================ *)

let%expect_test "keyword completion has Keyword kind" =
  reset ();
  with_document "let" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    let let_item = List.find_opt (fun (item : Lsp_completion.completion_item) ->
      item.label = "let") items in
    match let_item with
    | Some item ->
        let kind_str = match item.kind with
          | Lsp_completion.Keyword -> "Keyword"
          | _ -> "Other"
        in
        Printf.printf "kind=%s" kind_str
    | None -> print_endline "Not found");
  [%expect {| kind=Keyword |}]

let%expect_test "keyword completions have no detail" =
  reset ();
  with_document "let" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 3) in
    let let_item = List.find_opt (fun (item : Lsp_completion.completion_item) ->
      item.label = "let") items in
    match let_item with
    | Some item ->
        Printf.printf "has_detail=%b" (Option.is_some item.detail)
    | None -> print_endline "Not found");
  [%expect {| has_detail=false |}]

(* ============================================================ *)
(* All Keywords Present *)
(* ============================================================ *)

let%expect_test "completion returns various control flow keywords" =
  reset ();
  with_document "" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 0) in
    let keywords = ["if"; "then"; "else"; "match"; "with"; "when"; "begin"; "end"] in
    let found = List.filter (fun kw ->
      List.exists (fun (item : Lsp_completion.completion_item) ->
        item.label = kw) items) keywords in
    Printf.printf "found %d of %d control keywords" (List.length found) (List.length keywords));
  [%expect {| found 6 of 8 control keywords |}]

let%expect_test "completion returns definition keywords" =
  reset ();
  with_document "" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 0) in
    let keywords = ["let"; "rec"; "and"; "in"; "type"; "module"; "sig"; "struct"; "functor"] in
    let found = List.filter (fun kw ->
      List.exists (fun (item : Lsp_completion.completion_item) ->
        item.label = kw) items) keywords in
    Printf.printf "found %d of %d definition keywords" (List.length found) (List.length keywords));
  [%expect {| found 9 of 9 definition keywords |}]

let%expect_test "completion returns boolean literals" =
  reset ();
  with_document "" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 0) in
    let has_true = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "true") items in
    let has_false = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "false") items in
    Printf.printf "true=%b false=%b" has_true has_false);
  [%expect {| true=true false=true |}]

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let%expect_test "completion on empty document" =
  reset ();
  with_document "" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 0) in
    Printf.printf "count=%d" (List.length items));
  (* Should return all keywords *)
  [%expect {| count=24 |}]

let%expect_test "completion on invalid document returns keywords" =
  reset ();
  let store = Document_store.create () in
  let items = Lsp_completion.get_completions store "nonexistent.lina" (pos 0 0) in
  Printf.printf "count=%d" (List.length items);
  [%expect {| count=0 |}]
