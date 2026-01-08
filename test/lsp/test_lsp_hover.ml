(** Tests for LSP hover functionality. *)

open Lina_lsp
open Lsp_test_helpers

(* ============================================================ *)
(* Simple Expressions *)
(* ============================================================ *)

let%expect_test "hover on integer literal" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    (* Hover on 42 at position (0, 8) *)
    let result = Lsp_hover.get_hover store uri (pos 0 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on string literal" =
  reset ();
  with_document {|let s = "hello"|} (fun store uri ->
    (* Hover on "hello" at position (0, 9) *)
    let result = Lsp_hover.get_hover store uri (pos 0 9) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "hover on boolean" =
  reset ();
  with_document "let b = true" (fun store uri ->
    (* Hover on true at position (0, 8) *)
    let result = Lsp_hover.get_hover store uri (pos 0 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    bool
    ```
    |}]

(* ============================================================ *)
(* Function Types *)
(* ============================================================ *)

let%expect_test "hover on simple function" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    (* Hover on function body (x + 1) at position (0, 10) *)
    let result = Lsp_hover.get_hover store uri (pos 0 10) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    (int -> (int -> int))
    ```
    |}]

let%expect_test "hover on multi-arg function body" =
  reset ();
  with_document "let add x y = x + y" (fun store uri ->
    (* Hover on x + y at position (0, 14) *)
    let result = Lsp_hover.get_hover store uri (pos 0 14) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    (int -> (int -> int))
    ```
    |}]

let%expect_test "hover on polymorphic identity function" =
  reset ();
  with_document "let id x = x" (fun store uri ->
    (* Hover on x at position (0, 11) *)
    let result = Lsp_hover.get_hover store uri (pos 0 11) in
    match result with
    | Some r ->
        (* Should be a type variable like 'a or '_a *)
        let has_var = String.contains r.contents '\'' in
        Printf.printf "has_type_var=%b" has_var
    | None -> print_endline "None");
  [%expect {| has_type_var=true |}]

(* ============================================================ *)
(* Complex Expressions *)
(* ============================================================ *)

let%expect_test "hover on tuple" =
  reset ();
  with_document "let t = (1, true)" (fun store uri ->
    (* Hover on the tuple at position (0, 9) *)
    let result = Lsp_hover.get_hover store uri (pos 0 9) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on constructor application" =
  reset ();
  with_document "let opt = Some 42" (fun store uri ->
    (* Hover on Some 42 at position (0, 10) *)
    let result = Lsp_hover.get_hover store uri (pos 0 10) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "hover on nested expression" =
  reset ();
  with_document "let x = (1 + 2) * 3" (fun store uri ->
    (* Hover on (1 + 2) at position (0, 9) - the inner addition *)
    let result = Lsp_hover.get_hover store uri (pos 0 9) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    (int -> (int -> int))
    ```
    |}]

(* ============================================================ *)
(* Position Edge Cases *)
(* ============================================================ *)

let%expect_test "hover on whitespace returns None" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    (* Hover on space between 'let' and 'x' at position (0, 3) *)
    let result = Lsp_hover.get_hover store uri (pos 0 3) in
    match result with
    | Some _ -> print_endline "Got result"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "hover past end of line returns None" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    (* Hover past end at position (0, 100) *)
    let result = Lsp_hover.get_hover store uri (pos 0 100) in
    match result with
    | Some _ -> print_endline "Got result"
    | None -> print_endline "None");
  [%expect {| None |}]

let%expect_test "hover on invalid document returns None" =
  reset ();
  let store = Document_store.create () in
  let result = Lsp_hover.get_hover store "nonexistent.lina" (pos 0 0) in
  (match result with
   | Some _ -> print_endline "Got result"
   | None -> print_endline "None");
  [%expect {| None |}]

(* ============================================================ *)
(* Multi-line Documents *)
(* ============================================================ *)

let%expect_test "hover on second line" =
  reset ();
  with_document {|let x = 1
let y = x + 1|} (fun store uri ->
    (* Hover on 'x + 1' at line 1, position 8 *)
    let result = Lsp_hover.get_hover store uri (pos 1 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    (int -> (int -> int))
    ```
    |}]

let%expect_test "hover in function body on different line" =
  reset ();
  with_document {|let f x =
  x + 1|} (fun store uri ->
    (* Hover on 'x + 1' at line 1, position 2 *)
    let result = Lsp_hover.get_hover store uri (pos 1 2) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    (int -> (int -> int))
    ```
    |}]

(* ============================================================ *)
(* Hover on Various AST Nodes *)
(* ============================================================ *)

let%expect_test "hover on if expression" =
  reset ();
  with_document "let x = if true then 1 else 2" (fun store uri ->
    (* Hover on the if expression at position (0, 8) *)
    let result = Lsp_hover.get_hover store uri (pos 0 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on match expression result" =
  reset ();
  with_document {|let f x = match x with
  | Some n -> n
  | None -> 0|} (fun store uri ->
    (* Hover on 'n' at line 1 *)
    let result = Lsp_hover.get_hover store uri (pos 1 14) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {| None |}]

(* ============================================================ *)
(* Hover Range *)
(* ============================================================ *)

let%expect_test "hover includes range" =
  reset ();
  with_document "let x = 42" (fun store uri ->
    let result = Lsp_hover.get_hover store uri (pos 0 8) in
    match result with
    | Some r ->
        (match r.range with
         | Some range -> Printf.printf "has_range=true start_line=%d" range.start_pos.line
         | None -> print_endline "no range")
    | None -> print_endline "None");
  [%expect {| has_range=true start_line=0 |}]
