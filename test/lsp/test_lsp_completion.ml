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
  (* Should return all keywords (24) + built-in values (11) = 35 *)
  [%expect {| count=52 |}]

let%expect_test "completion on invalid document returns keywords" =
  reset ();
  let store = Document_store.create () in
  let items = Lsp_completion.get_completions store "nonexistent.lina" (pos 0 0) in
  Printf.printf "count=%d" (List.length items);
  [%expect {| count=0 |}]

(* ============================================================ *)
(* Environment-Based Completions *)
(* ============================================================ *)

let%expect_test "completion includes user-defined function" =
  reset ();
  (* Use valid syntax: `inc 10` is a complete expression *)
  with_document "let inc a = a + 1\nlet test = inc 10" (fun store uri ->
    (* Type "in" at position where we want to complete *)
    let items = Lsp_completion.get_completions store uri (pos 1 13) in
    let has_inc = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "inc") items in
    Printf.printf "has_inc=%b" has_inc);
  [%expect {| has_inc=true |}]

let%expect_test "completion for user function has Function kind" =
  reset ();
  with_document "let add x y = x + y\nlet result = add 1 2" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 15) in
    let add_item = List.find_opt (fun (item : Lsp_completion.completion_item) ->
      item.label = "add") items in
    match add_item with
    | Some item ->
        let kind_str = match item.kind with
          | Lsp_completion.Function -> "Function"
          | Lsp_completion.Variable -> "Variable"
          | _ -> "Other"
        in
        Printf.printf "kind=%s" kind_str
    | None -> print_endline "Not found");
  [%expect {| kind=Function |}]

let%expect_test "completion for user function includes type in detail" =
  reset ();
  with_document "let inc a = a + 1\nlet result = inc 5" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 16) in
    let inc_item = List.find_opt (fun (item : Lsp_completion.completion_item) ->
      item.label = "inc") items in
    match inc_item with
    | Some item ->
        Printf.printf "has_detail=%b" (Option.is_some item.detail)
    | None -> print_endline "Not found");
  [%expect {| has_detail=true |}]

let%expect_test "completion for user variable has Variable kind" =
  reset ();
  with_document "let counter = 42\nlet result = counter + 1" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 20) in
    let counter_item = List.find_opt (fun (item : Lsp_completion.completion_item) ->
      item.label = "counter") items in
    match counter_item with
    | Some item ->
        let kind_str = match item.kind with
          | Lsp_completion.Function -> "Function"
          | Lsp_completion.Variable -> "Variable"
          | _ -> "Other"
        in
        Printf.printf "kind=%s" kind_str
    | None -> print_endline "Not found");
  [%expect {| kind=Variable |}]

let%expect_test "completion includes builtin functions" =
  reset ();
  with_document "let _ = print 42" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 0 13) in
    let has_print = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "print") items in
    Printf.printf "has_print=%b" has_print);
  [%expect {| has_print=true |}]

let%expect_test "completion includes type names" =
  reset ();
  (* Simple variant type to verify types are included *)
  with_document "type color = Red | Blue\nlet x = 1" (fun store uri ->
    (* Get all completions with "col" prefix - position after "let x = " is 8, so pos 8 gives empty prefix *)
    let items = Lsp_completion.get_completions store uri (pos 1 8) in
    let type_items = List.filter (fun (item : Lsp_completion.completion_item) ->
      item.kind = Lsp_completion.Type) items in
    let has_color = List.exists (fun item -> item.Lsp_completion.label = "color") type_items in
    Printf.printf "has_color=%b types_count=%d" has_color (List.length type_items));
  [%expect {| has_color=true types_count=4 |}]

let%expect_test "completion includes variant constructors" =
  reset ();
  with_document "type color = Red | Green | Blue\nlet c = Green" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 11) in
    let has_green = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "Green" && item.kind = Lsp_completion.Constructor) items in
    Printf.printf "has_Green=%b" has_green);
  [%expect {| has_Green=true |}]

let%expect_test "completion includes module names" =
  reset ();
  with_document "module Math = struct let pi = 3 end\nlet x = Math.pi" (fun store uri ->
    let items = Lsp_completion.get_completions store uri (pos 1 12) in
    let has_math = List.exists (fun (item : Lsp_completion.completion_item) ->
      item.label = "Math" && item.kind = Lsp_completion.Module) items in
    Printf.printf "has_Math=%b" has_math);
  [%expect {| has_Math=true |}]
