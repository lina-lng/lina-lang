(** Tests for LSP semantic tokens functionality.

    Tests verify that semantic tokens are correctly collected from typed AST
    and encoded in LSP delta format. *)

open Lina_lsp
open Lsp_test_helpers

(** Format a single semantic token for testing. *)
let show_token (tok : Lsp_semantic_tokens.semantic_token) =
  let type_str =
    match tok.token_type with
    | Lsp_semantic_tokens.TokenNamespace -> "namespace"
    | Lsp_semantic_tokens.TokenType -> "type"
    | Lsp_semantic_tokens.TokenTypeParameter -> "typeParameter"
    | Lsp_semantic_tokens.TokenParameter -> "parameter"
    | Lsp_semantic_tokens.TokenVariable -> "variable"
    | Lsp_semantic_tokens.TokenProperty -> "property"
    | Lsp_semantic_tokens.TokenEnumMember -> "enumMember"
    | Lsp_semantic_tokens.TokenFunction -> "function"
    | Lsp_semantic_tokens.TokenKeyword -> "keyword"
    | Lsp_semantic_tokens.TokenString -> "string"
    | Lsp_semantic_tokens.TokenNumber -> "number"
    | Lsp_semantic_tokens.TokenOperator -> "operator"
  in
  let modifiers_str =
    tok.modifiers
    |> List.map (function
      | Lsp_semantic_tokens.ModDeclaration -> "decl"
      | Lsp_semantic_tokens.ModReadonly -> "ro"
      | Lsp_semantic_tokens.ModStatic -> "static")
    |> String.concat ","
  in
  Printf.sprintf "%d:%d len=%d %s [%s]"
    tok.line tok.start_char tok.length type_str modifiers_str

(** Format tokens for testing. *)
let show_tokens tokens =
  if tokens = [] then "No tokens"
  else
    tokens
    |> List.map show_token
    |> String.concat "\n"

(** Get semantic tokens from source code. *)
let get_tokens content =
  let store = Document_store.create () in
  let uri = "test.lina" in
  Document_store.open_document store ~uri ~content ~version:1;
  let typed_ast_opt, _, _ = Diagnostics.type_check_document store uri in
  match typed_ast_opt with
  | None -> []
  | Some typed_ast -> Lsp_semantic_tokens.collect_tokens typed_ast

(* ============================================================ *)
(* Basic Token Collection *)
(* ============================================================ *)

let%expect_test "semantic tokens for simple let binding" =
  reset ();
  let tokens = get_tokens "let x = 42" in
  print_endline (show_tokens tokens);
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:8 len=2 number []
  |}]

let%expect_test "semantic tokens for function definition" =
  reset ();
  let tokens = get_tokens "let f x = x + 1" in
  print_endline (show_tokens tokens);
  (* Now includes the + operator as a function token *)
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:6 len=1 parameter [decl,ro]
    0:10 len=1 variable [ro]
    0:12 len=1 function [ro]
    0:14 len=1 number []
    |}]

let%expect_test "semantic tokens for string literal" =
  reset ();
  let tokens = get_tokens {|let s = "hello"|} in
  print_endline (show_tokens tokens);
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:14 len=1 string []
    |}]

let%expect_test "semantic tokens for boolean" =
  reset ();
  let tokens = get_tokens "let b = true" in
  print_endline (show_tokens tokens);
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:8 len=4 keyword []
  |}]

(* ============================================================ *)
(* Constructors and Patterns *)
(* ============================================================ *)

let%expect_test "semantic tokens for constructor" =
  reset ();
  (* Need to define the option type first since it's not in the initial env *)
  let tokens = get_tokens {|type 'a option = None | Some of 'a
let x = Some 42|} in
  print_endline (show_tokens tokens);
  [%expect {|
    1:4 len=1 variable [static,decl,ro]
    1:8 len=4 enumMember []
    1:13 len=2 number []
  |}]

let%expect_test "semantic tokens for None" =
  reset ();
  let tokens = get_tokens {|type 'a option = None | Some of 'a
let x = None|} in
  print_endline (show_tokens tokens);
  [%expect {|
    1:4 len=1 variable [static,decl,ro]
    1:8 len=4 enumMember []
  |}]

let%expect_test "semantic tokens for tuple" =
  reset ();
  let tokens = get_tokens "let t = (1, 2, 3)" in
  print_endline (show_tokens tokens);
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:9 len=1 number []
    0:12 len=1 number []
    0:15 len=1 number []
  |}]

(* ============================================================ *)
(* Match Expressions *)
(* ============================================================ *)

let%expect_test "semantic tokens for match expression" =
  reset ();
  let tokens = get_tokens {|type 'a option = None | Some of 'a
let f x = match x with
  | Some n -> n
  | None -> 0|} in
  print_endline (show_tokens tokens);
  [%expect {|
    1:4 len=1 variable [static,decl,ro]
    1:6 len=1 parameter [decl,ro]
    1:16 len=1 variable [ro]
    2:4 len=4 enumMember []
    2:9 len=1 variable [decl,ro]
    2:14 len=1 variable [ro]
    3:4 len=4 enumMember []
    3:12 len=1 number []
  |}]

(* ============================================================ *)
(* Function Application *)
(* ============================================================ *)

let%expect_test "semantic tokens for function call" =
  reset ();
  let tokens = get_tokens {|let f x = x + 1
let y = f 42|} in
  print_endline (show_tokens tokens);
  (* Now includes the + operator as a function token *)
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    0:6 len=1 parameter [decl,ro]
    0:10 len=1 variable [ro]
    0:12 len=1 function [ro]
    0:14 len=1 number []
    1:4 len=1 variable [static,decl,ro]
    1:8 len=1 function [ro]
    1:10 len=2 number []
    |}]

(* ============================================================ *)
(* Modules *)
(* ============================================================ *)

let%expect_test "semantic tokens for module definition" =
  reset ();
  let tokens = get_tokens {|module M = struct
  let x = 42
end|} in
  print_endline (show_tokens tokens);
  [%expect {|
    0:0 len=1 namespace [decl,static]
    1:6 len=1 variable [static,decl,ro]
    1:10 len=2 number []
  |}]

(* ============================================================ *)
(* Nested Let *)
(* ============================================================ *)

let%expect_test "semantic tokens for nested let" =
  reset ();
  let tokens = get_tokens {|let x =
  let y = 1 in
  y + 2|} in
  print_endline (show_tokens tokens);
  (* Now includes the + operator as a function token *)
  [%expect {|
    0:4 len=1 variable [static,decl,ro]
    1:6 len=1 variable [decl,ro]
    1:10 len=1 number []
    2:2 len=1 variable [ro]
    2:4 len=1 function [ro]
    2:6 len=1 number []
    |}]

(* ============================================================ *)
(* Delta Encoding *)
(* ============================================================ *)

let%expect_test "delta encoding produces correct format" =
  reset ();
  let tokens = get_tokens "let x = 42" in
  let encoded = Lsp_semantic_tokens.encode_tokens tokens in
  (* Each token is 5 integers: deltaLine, deltaChar, length, tokenType, modifiers *)
  Printf.printf "Encoded: [%s]"
    (String.concat "; " (List.map string_of_int encoded));
  [%expect {| Encoded: [0; 4; 1; 4; 7; 0; 4; 2; 10; 0] |}]

let%expect_test "delta encoding across multiple lines" =
  reset ();
  let tokens = get_tokens {|let x = 1
let y = 2|} in
  let encoded = Lsp_semantic_tokens.encode_tokens tokens in
  Printf.printf "Encoded: [%s]"
    (String.concat "; " (List.map string_of_int encoded));
  (* First token: line 0, char 4, variable
     Second token: line 0, char 8, number
     Third token: line 1, char 4, variable (delta line=1, char=4)
     Fourth token: line 1, char 8, number (delta line=0, char=4) *)
  [%expect {| Encoded: [0; 4; 1; 4; 7; 0; 4; 1; 10; 0; 1; 4; 1; 4; 7; 0; 4; 1; 10; 0] |}]

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let%expect_test "semantic tokens for empty document" =
  reset ();
  let tokens = get_tokens "" in
  print_endline (show_tokens tokens);
  [%expect {| No tokens |}]

let%expect_test "semantic tokens for document with parse error" =
  reset ();
  let tokens = get_tokens "let x =" in
  print_endline (show_tokens tokens);
  [%expect {| No tokens |}]

let%expect_test "token legend has expected types" =
  let types = Lsp_semantic_tokens.token_type_names in
  Printf.printf "Token types: %s\n" (String.concat ", " types);
  [%expect {| Token types: namespace, type, typeParameter, parameter, variable, property, enumMember, function, keyword, string, number, operator |}]

let%expect_test "token legend has expected modifiers" =
  let modifiers = Lsp_semantic_tokens.token_modifier_names in
  Printf.printf "Token modifiers: %s\n" (String.concat ", " modifiers);
  [%expect {| Token modifiers: declaration, readonly, static |}]
