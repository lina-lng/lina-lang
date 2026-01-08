(** Tests for LSP document symbols. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Value Bindings *)
(* ============================================================ *)

let%expect_test "symbol for simple let binding" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| x (Variable) : int |}]

let%expect_test "symbol for function binding" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| f (Function) : (int -> int) |}]

let%expect_test "symbol for multi-arg function" =
  reset ();
  with_document "let add x y = x + y" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| add (Function) : (int -> (int -> int)) |}]

let%expect_test "symbol for polymorphic function" =
  reset ();
  with_document "let id x = x" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    match symbols with
    | [sym] ->
        Printf.printf "name=%s kind=%s has_type_var=%b"
          sym.name
          (match sym.kind with Lsp_symbols.Function -> "Function" | _ -> "Other")
          (match sym.detail with Some d -> String.contains d '\'' | None -> false)
    | _ -> Printf.printf "unexpected count: %d" (List.length symbols));
  [%expect {| name=id kind=Function has_type_var=true |}]

let%expect_test "symbol for recursive function" =
  reset ();
  with_document "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| fact (Function) : (int -> int) |}]

(* ============================================================ *)
(* Multiple Bindings *)
(* ============================================================ *)

let%expect_test "symbols for multiple let bindings" =
  reset ();
  with_document {|let x = 1
let y = 2
let z = 3|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    x (Variable) : int
    y (Variable) : int
    z (Variable) : int
    |}]

let%expect_test "symbols for mixed bindings" =
  reset ();
  with_document {|let x = 42
let f a = a + 1
let s = "hello"|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    x (Variable) : int
    f (Function) : (int -> int)
    s (Variable) : string
    |}]

(* ============================================================ *)
(* Type Definitions *)
(* ============================================================ *)

let%expect_test "symbol for variant type" =
  reset ();
  with_document "type color = Red | Green | Blue" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| color (Type) |}]

let%expect_test "symbol for record type" =
  reset ();
  with_document "type point = { x : int; y : int }" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| point (Type) |}]

let%expect_test "symbol for type alias" =
  reset ();
  with_document "type int_pair = int * int" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| int_pair (Type) |}]

let%expect_test "symbol for parameterized type" =
  reset ();
  with_document "type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| tree (Type) |}]

(* ============================================================ *)
(* Module Definitions *)
(* ============================================================ *)

let%expect_test "symbol for simple module" =
  reset ();
  with_document {|module M = struct
  let value = 42
end|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    M (Module)
      value (Variable) : int
    |}]

let%expect_test "symbol for module with function" =
  reset ();
  with_document {|module Math = struct
  let add x y = x + y
end|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    Math (Module)
      add (Function) : (int -> (int -> int))
    |}]

let%expect_test "symbol for module with type" =
  reset ();
  with_document {|module Point = struct
  type t = { x : int; y : int }
  let origin = { x = 0; y = 0 }
end|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    Point (Module)
      t (Type)
      origin (Variable) : { x : int; y : int }
    |}]

let%expect_test "symbol for empty module" =
  reset ();
  with_document {|module Empty = struct
end|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| Empty (Module) |}]

(* ============================================================ *)
(* Mixed Definitions *)
(* ============================================================ *)

let%expect_test "symbols for types and values" =
  reset ();
  with_document {|type t = int
let x : t = 42|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| No symbols |}]

let%expect_test "symbols for values and modules" =
  reset ();
  with_document {|let x = 1
module M = struct
  let y = 2
end
let z = M.y|} (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {|
    x (Variable) : int
    M (Module)
      y (Variable) : int
    z (Variable) : int
    |}]

(* ============================================================ *)
(* Symbol Properties *)
(* ============================================================ *)

let%expect_test "symbol has correct range" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    match symbols with
    | [sym] ->
        Printf.printf "start_line=%d start_char=%d"
          sym.range.start_pos.line
          sym.range.start_pos.character
    | _ -> print_endline "unexpected");
  [%expect {| start_line=0 start_char=3 |}]

let contains_substring substring s =
  let len_sub = String.length substring in
  let len_s = String.length s in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = substring then true
      else check (i + 1)
    in
    check 0

let%expect_test "function symbol has detail with type" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    match symbols with
    | [sym] ->
        Printf.printf "has_detail=%b detail_contains_arrow=%b"
          (Option.is_some sym.detail)
          (match sym.detail with Some d -> contains_substring "->" d | None -> false)
    | _ -> print_endline "unexpected");
  [%expect {| has_detail=true detail_contains_arrow=true |}]

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let%expect_test "symbols for empty document" =
  reset ();
  with_document "" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| No symbols |}]

let%expect_test "symbols for invalid document returns empty" =
  reset ();
  let store = Document_store.create () in
  let symbols = Lsp_symbols.get_document_symbols store "nonexistent.lina" in
  print_endline (show_symbols symbols);
  [%expect {| No symbols |}]

let%expect_test "symbols for document with parse error" =
  reset ();
  with_document "let x =" (fun store uri ->
    let symbols = Lsp_symbols.get_document_symbols store uri in
    print_endline (show_symbols symbols));
  [%expect {| No symbols |}]
