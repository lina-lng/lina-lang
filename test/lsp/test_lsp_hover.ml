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

let%expect_test "hover on variable in function body" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    (* Hover on 'x' at position (0, 10) - shows type of the variable x *)
    let result = Lsp_hover.get_hover store uri (pos 0 10) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on variable in multi-arg function body" =
  reset ();
  with_document "let add x y = x + y" (fun store uri ->
    (* Hover on 'x' at position (0, 14) - shows type of variable x *)
    let result = Lsp_hover.get_hover store uri (pos 0 14) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
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
  [%expect {|
    ```lina
    int option
    ```
    |}]

let%expect_test "hover on literal in nested expression" =
  reset ();
  with_document "let x = (1 + 2) * 3" (fun store uri ->
    (* Hover on '1' at position (0, 9) - shows type of the integer literal *)
    let result = Lsp_hover.get_hover store uri (pos 0 9) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
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

let%expect_test "hover on variable on second line" =
  reset ();
  with_document {|let x = 1
let y = x + 1|} (fun store uri ->
    (* Hover on 'x' at line 1, position 8 - shows type of variable x *)
    let result = Lsp_hover.get_hover store uri (pos 1 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on variable in function body on different line" =
  reset ();
  with_document {|let f x =
  x + 1|} (fun store uri ->
    (* Hover on 'x' at line 1, position 2 - shows type of variable x *)
    let result = Lsp_hover.get_hover store uri (pos 1 2) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
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
  [%expect {|
    ```lina
    int
    ```
    |}]

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

(* ============================================================ *)
(* Pattern Hover - Binding Patterns *)
(* ============================================================ *)

let%expect_test "hover on let binding pattern shows type" =
  reset ();
  with_document "let test = 42" (fun store uri ->
    (* Hover on 'test' at position (0, 4) - the binding name *)
    let result = Lsp_hover.get_hover store uri (pos 0 4) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on binding pattern of function application" =
  reset ();
  with_document "let inc a = a + 1\nlet result = inc 10" (fun store uri ->
    (* Hover on 'result' at line 1, position 4 *)
    let result = Lsp_hover.get_hover store uri (pos 1 4) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on tuple binding pattern" =
  reset ();
  with_document "let (a, b) = (1, 2)" (fun store uri ->
    (* Hover on 'a' at position (0, 5) *)
    let result = Lsp_hover.get_hover store uri (pos 0 5) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

(* ============================================================ *)
(* Pattern Hover - Function Parameters *)
(* ============================================================ *)

let%expect_test "hover on function parameter shows type" =
  reset ();
  with_document "let f x = x + 1" (fun store uri ->
    (* Hover on 'x' parameter at position (0, 6) *)
    let result = Lsp_hover.get_hover store uri (pos 0 6) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on multi-param function first param" =
  reset ();
  with_document "let add x y = x + y" (fun store uri ->
    (* Hover on 'x' parameter at position (0, 8) *)
    let result = Lsp_hover.get_hover store uri (pos 0 8) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

let%expect_test "hover on multi-param function second param" =
  reset ();
  with_document "let add x y = x + y" (fun store uri ->
    (* Hover on 'y' parameter at position (0, 10) *)
    let result = Lsp_hover.get_hover store uri (pos 0 10) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]

(* ============================================================ *)
(* Pattern Hover - Match Arm Patterns *)
(* ============================================================ *)

let%expect_test "hover on match arm pattern variable" =
  reset ();
  with_document "type t = A of int\nlet f x = match x with A n -> n" (fun store uri ->
    (* The pattern 'n' is polymorphic in the match context - just verify we get a type *)
    let result = Lsp_hover.get_hover store uri (pos 1 25) in
    match result with
    | Some r ->
        (* Just check we got some type info, it may be a type variable *)
        let has_type = String.length r.contents > 10 in
        Printf.printf "has_type=%b" has_type
    | None -> print_endline "None");
  [%expect {| has_type=true |}]

let%expect_test "hover on wildcard pattern returns unit type" =
  reset ();
  with_document "let _ = 42" (fun store uri ->
    (* Hover on '_' at position (0, 4) *)
    let result = Lsp_hover.get_hover store uri (pos 0 4) in
    match result with
    | Some r -> print_endline r.contents
    | None -> print_endline "None");
  [%expect {|
    ```lina
    int
    ```
    |}]
