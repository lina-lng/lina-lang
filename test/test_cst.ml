(** CST (Concrete Syntax Tree) tests.

    Tests for the Red-Green tree infrastructure including:
    - Green tree construction and operations
    - Red tree navigation
    - CST lexer tokenization with trivia
    - CST parser
    - CST formatter with comment preservation *)

open Cst

(** {1 Syntax Kind Tests} *)

let%expect_test "syntax kind classification - tokens" =
  let open Syntax_kind in
  Printf.printf "TK_LET is_token: %b\n" (is_token TK_LET);
  Printf.printf "TK_LOWERCASE_IDENT is_token: %b\n" (is_token TK_LOWERCASE_IDENT);
  Printf.printf "NK_SOURCE_FILE is_token: %b\n" (is_token NK_SOURCE_FILE);
  [%expect {|
    TK_LET is_token: true
    TK_LOWERCASE_IDENT is_token: true
    NK_SOURCE_FILE is_token: false
  |}]

let%expect_test "syntax kind classification - nodes" =
  let open Syntax_kind in
  Printf.printf "NK_SOURCE_FILE is_node: %b\n" (is_node NK_SOURCE_FILE);
  Printf.printf "NK_BINDING is_node: %b\n" (is_node NK_BINDING);
  Printf.printf "TK_LET is_node: %b\n" (is_node TK_LET);
  [%expect {|
    NK_SOURCE_FILE is_node: true
    NK_BINDING is_node: true
    TK_LET is_node: false
  |}]

let%expect_test "syntax kind classification - trivia" =
  let open Syntax_kind in
  Printf.printf "TK_WHITESPACE is_trivia: %b\n" (is_trivia TK_WHITESPACE);
  Printf.printf "TK_LINE_COMMENT is_trivia: %b\n" (is_trivia TK_LINE_COMMENT);
  Printf.printf "TK_BLOCK_COMMENT is_trivia: %b\n" (is_trivia TK_BLOCK_COMMENT);
  Printf.printf "TK_NEWLINE is_trivia: %b\n" (is_trivia TK_NEWLINE);
  Printf.printf "TK_LET is_trivia: %b\n" (is_trivia TK_LET);
  [%expect {|
    TK_WHITESPACE is_trivia: true
    TK_LINE_COMMENT is_trivia: true
    TK_BLOCK_COMMENT is_trivia: true
    TK_NEWLINE is_trivia: true
    TK_LET is_trivia: false
  |}]

let%expect_test "syntax kind classification - keywords" =
  let open Syntax_kind in
  Printf.printf "TK_LET is_keyword: %b\n" (is_keyword TK_LET);
  Printf.printf "TK_IF is_keyword: %b\n" (is_keyword TK_IF);
  Printf.printf "TK_MODULE is_keyword: %b\n" (is_keyword TK_MODULE);
  Printf.printf "TK_LOWERCASE_IDENT is_keyword: %b\n" (is_keyword TK_LOWERCASE_IDENT);
  Printf.printf "TK_PLUS is_keyword: %b\n" (is_keyword TK_PLUS);
  [%expect {|
    TK_LET is_keyword: true
    TK_IF is_keyword: true
    TK_MODULE is_keyword: true
    TK_LOWERCASE_IDENT is_keyword: false
    TK_PLUS is_keyword: false
  |}]

(** {1 Green Tree Tests} *)

let%expect_test "green tree - token construction" =
  let open Green_tree in
  let token = make_token Syntax_kind.TK_LOWERCASE_IDENT "foo" in
  Printf.printf "kind: %s\n" (Syntax_kind.show token.kind);
  Printf.printf "text: %s\n" token.text;
  Printf.printf "leading_trivia count: %d\n" (List.length token.leading_trivia);
  Printf.printf "trailing_trivia count: %d\n" (List.length token.trailing_trivia);
  [%expect {|
    kind: Syntax_kind.TK_LOWERCASE_IDENT
    text: foo
    leading_trivia count: 0
    trailing_trivia count: 0
  |}]

let%expect_test "green tree - token with trivia" =
  let open Green_tree in
  let leading = [
    { trivia_kind = Syntax_kind.TK_WHITESPACE; trivia_text = "  " };
  ] in
  let trailing = [
    { trivia_kind = Syntax_kind.TK_LINE_COMMENT; trivia_text = "-- comment" };
  ] in
  let token = make_token_with_trivia Syntax_kind.TK_LOWERCASE_IDENT "x" ~leading ~trailing in
  Printf.printf "text: %s\n" token.text;
  Printf.printf "leading_trivia: %d pieces\n" (List.length token.leading_trivia);
  Printf.printf "trailing_trivia: %d pieces\n" (List.length token.trailing_trivia);
  Printf.printf "full text length: %d\n" (token_len token);
  [%expect {|
    text: x
    leading_trivia: 1 pieces
    trailing_trivia: 1 pieces
    full text length: 13
  |}]

let%expect_test "green tree - node construction" =
  let open Green_tree in
  let let_tok = make_token Syntax_kind.TK_LET "let" in
  let ident_tok = make_token Syntax_kind.TK_LOWERCASE_IDENT "x" in
  let eq_tok = make_token Syntax_kind.TK_EQUAL "=" in
  let num_tok = make_token Syntax_kind.TK_INTEGER "42" in
  let children = [
    GreenToken let_tok;
    GreenToken ident_tok;
    GreenToken eq_tok;
    GreenToken num_tok;
  ] in
  let node = make_node Syntax_kind.NK_BINDING children in
  Printf.printf "kind: %s\n" (Syntax_kind.show node.kind);
  Printf.printf "text_len: %d\n" node.text_len;
  Printf.printf "children count: %d\n" (List.length node.children);
  [%expect {|
    kind: Syntax_kind.NK_BINDING
    text_len: 7
    children count: 4
    |}]

let%expect_test "green tree - text extraction" =
  let open Green_tree in
  let let_tok = make_token Syntax_kind.TK_LET "let" in
  let ws1 = { trivia_kind = Syntax_kind.TK_WHITESPACE; trivia_text = " " } in
  let ident_tok = make_token_with_trivia Syntax_kind.TK_LOWERCASE_IDENT "x" ~leading:[ws1] ~trailing:[] in
  let ws2 = { trivia_kind = Syntax_kind.TK_WHITESPACE; trivia_text = " " } in
  let eq_tok = make_token_with_trivia Syntax_kind.TK_EQUAL "=" ~leading:[ws2] ~trailing:[] in
  let ws3 = { trivia_kind = Syntax_kind.TK_WHITESPACE; trivia_text = " " } in
  let num_tok = make_token_with_trivia Syntax_kind.TK_INTEGER "42" ~leading:[ws3] ~trailing:[] in
  let children = [
    GreenToken let_tok;
    GreenToken ident_tok;
    GreenToken eq_tok;
    GreenToken num_tok;
  ] in
  let node = make_node Syntax_kind.NK_BINDING children in
  Printf.printf "node text: |%s|\n" (green_node_text node);
  [%expect {|
    node text: |let x = 42|
  |}]

(** {1 Red Tree Tests} *)

let%expect_test "red tree - root creation" =
  let open Green_tree in
  let let_tok = make_token Syntax_kind.TK_LET "let" in
  let ident_tok = make_token Syntax_kind.TK_LOWERCASE_IDENT "x" in
  let eq_tok = make_token Syntax_kind.TK_EQUAL "=" in
  let num_tok = make_token Syntax_kind.TK_INTEGER "1" in
  let binding = make_node Syntax_kind.NK_BINDING [
    GreenToken let_tok;
    GreenToken ident_tok;
    GreenToken eq_tok;
    GreenToken num_tok;
  ] in
  let source = make_node Syntax_kind.NK_SOURCE_FILE [GreenNode binding] in
  let red = Red_tree.root source in
  Printf.printf "kind: %s\n" (Syntax_kind.show (Red_tree.kind red));
  Printf.printf "offset: %d\n" red.offset;
  Printf.printf "has parent: %b\n" (Option.is_some red.parent);
  [%expect {|
    kind: Syntax_kind.NK_SOURCE_FILE
    offset: 0
    has parent: false
  |}]

let%expect_test "red tree - children navigation" =
  let open Green_tree in
  let let_tok = make_token Syntax_kind.TK_LET "let" in
  let ident_tok = make_token Syntax_kind.TK_LOWERCASE_IDENT "x" in
  let eq_tok = make_token Syntax_kind.TK_EQUAL "=" in
  let num_tok = make_token Syntax_kind.TK_INTEGER "1" in
  let binding = make_node Syntax_kind.NK_BINDING [
    GreenToken let_tok;
    GreenToken ident_tok;
    GreenToken eq_tok;
    GreenToken num_tok;
  ] in
  let source = make_node Syntax_kind.NK_SOURCE_FILE [GreenNode binding] in
  let red = Red_tree.root source in
  let children = Red_tree.children red in
  Printf.printf "child count: %d\n" (List.length children);
  List.iter (function
    | Red_tree.SyntaxNode node ->
        Printf.printf "  node: %s at %d\n"
          (Syntax_kind.show (Red_tree.kind node))
          node.offset
    | Red_tree.SyntaxToken token ->
        Printf.printf "  token: %s '%s' at %d\n"
          (Syntax_kind.show token.green.kind)
          token.green.text
          token.offset
  ) children;
  [%expect {|
    child count: 1
      node: Syntax_kind.NK_BINDING at 0
  |}]

let%expect_test "red tree - span calculation" =
  let open Green_tree in
  let let_tok = make_token Syntax_kind.TK_LET "let" in
  let ident_tok = make_token Syntax_kind.TK_LOWERCASE_IDENT "foo" in
  let eq_tok = make_token Syntax_kind.TK_EQUAL "=" in
  let num_tok = make_token Syntax_kind.TK_INTEGER "42" in
  let binding = make_node Syntax_kind.NK_BINDING [
    GreenToken let_tok;
    GreenToken ident_tok;
    GreenToken eq_tok;
    GreenToken num_tok;
  ] in
  let red = Red_tree.root binding in
  let span = Red_tree.span red in
  Printf.printf "start: %d\n" span.start;
  Printf.printf "length: %d\n" span.length;
  [%expect {|
    start: 0
    length: 9
  |}]

(** {1 CST Lexer Tests} *)

let%expect_test "cst lexer - basic tokenization" =
  let tokens = Cst_lexer.tokenize "test" "let x = 1" in
  List.iter (fun (token : Cst_lexer.cst_token) ->
    Printf.printf "%s '%s'\n"
      (Syntax_kind.show token.green.kind)
      token.green.text
  ) tokens;
  [%expect {|
    Syntax_kind.TK_LET 'let'
    Syntax_kind.TK_LOWERCASE_IDENT 'x'
    Syntax_kind.TK_EQUAL '='
    Syntax_kind.TK_INTEGER '1'
    Syntax_kind.TK_EOF ''
  |}]

let%expect_test "cst lexer - trivia attachment" =
  let tokens = Cst_lexer.tokenize "test" "let  x = 1" in
  List.iter (fun (token : Cst_lexer.cst_token) ->
    let leading = List.length token.green.leading_trivia in
    let trailing = List.length token.green.trailing_trivia in
    Printf.printf "%s '%s' (leading: %d, trailing: %d)\n"
      (Syntax_kind.show token.green.kind)
      token.green.text
      leading trailing
  ) tokens;
  [%expect {|
    Syntax_kind.TK_LET 'let' (leading: 0, trailing: 1)
    Syntax_kind.TK_LOWERCASE_IDENT 'x' (leading: 0, trailing: 1)
    Syntax_kind.TK_EQUAL '=' (leading: 0, trailing: 1)
    Syntax_kind.TK_INTEGER '1' (leading: 0, trailing: 0)
    Syntax_kind.TK_EOF '' (leading: 0, trailing: 0)
    |}]

let%expect_test "cst lexer - line comment as trivia" =
  let tokens = Cst_lexer.tokenize "test" "let x = 1 -- comment" in
  List.iter (fun (token : Cst_lexer.cst_token) ->
    Printf.printf "%s '%s'\n" (Syntax_kind.show token.green.kind) token.green.text;
    List.iter (fun (trivia : Green_tree.trivia_piece) ->
      Printf.printf "  trailing: %s '%s'\n"
        (Syntax_kind.show trivia.trivia_kind)
        trivia.trivia_text
    ) token.green.trailing_trivia
  ) tokens;
  [%expect {|
    Syntax_kind.TK_LET 'let'
      trailing: Syntax_kind.TK_WHITESPACE ' '
    Syntax_kind.TK_LOWERCASE_IDENT 'x'
      trailing: Syntax_kind.TK_WHITESPACE ' '
    Syntax_kind.TK_EQUAL '='
      trailing: Syntax_kind.TK_WHITESPACE ' '
    Syntax_kind.TK_INTEGER '1'
      trailing: Syntax_kind.TK_WHITESPACE ' '
      trailing: Syntax_kind.TK_LINE_COMMENT '-- comment'
    Syntax_kind.TK_EOF ''
    |}]

let%expect_test "cst lexer - block comment" =
  let tokens = Cst_lexer.tokenize "test" "let (* comment *) x = 1" in
  List.iter (fun (token : Cst_lexer.cst_token) ->
    Printf.printf "%s '%s'\n" (Syntax_kind.show token.green.kind) token.green.text;
    List.iter (fun (trivia : Green_tree.trivia_piece) ->
      Printf.printf "  leading: %s '%s'\n"
        (Syntax_kind.show trivia.trivia_kind)
        trivia.trivia_text
    ) token.green.leading_trivia
  ) tokens;
  [%expect {|
    Syntax_kind.TK_LET 'let'
    Syntax_kind.TK_LOWERCASE_IDENT 'x'
    Syntax_kind.TK_EQUAL '='
    Syntax_kind.TK_INTEGER '1'
    Syntax_kind.TK_EOF ''
    |}]

(** {1 CST Parser Tests} *)

let%expect_test "cst parser - simple binding" =
  let root = Cst_parser.parse "test" "let x = 1" in
  Printf.printf "kind: %s\n" (Syntax_kind.show (Red_tree.kind root));
  Printf.printf "children: %d\n" (List.length (Red_tree.children root));
  [%expect {|
    kind: Syntax_kind.NK_SOURCE_FILE
    children: 2
    |}]

let%expect_test "cst parser - lossless text reconstruction" =
  let source = "let x = 1" in
  let root = Cst_parser.parse "test" source in
  let reconstructed = Red_tree.text root in
  Printf.printf "original:      |%s|\n" source;
  Printf.printf "reconstructed: |%s|\n" reconstructed;
  Printf.printf "match: %b\n" (source = reconstructed);
  [%expect {|
    original:      |let x = 1|
    reconstructed: |let x = 1|
    match: true
  |}]

let%expect_test "cst parser - preserves whitespace" =
  let source = "let   x   =   1" in
  let root = Cst_parser.parse "test" source in
  let reconstructed = Red_tree.text root in
  Printf.printf "match: %b\n" (source = reconstructed);
  [%expect {|
    match: true
  |}]

let%expect_test "cst parser - preserves comments" =
  let source = "let x = 1 -- comment" in
  let root = Cst_parser.parse "test" source in
  let reconstructed = Red_tree.text root in
  Printf.printf "original:      |%s|\n" source;
  Printf.printf "reconstructed: |%s|\n" reconstructed;
  Printf.printf "match: %b\n" (source = reconstructed);
  [%expect {|
    original:      |let x = 1 -- comment|
    reconstructed: |let x = 1 -- comment|
    match: true
  |}]

let%expect_test "cst parser - multiple definitions" =
  let source = "let x = 1\nlet y = 2" in
  let root = Cst_parser.parse "test" source in
  Printf.printf "children: %d\n" (List.length (Red_tree.children root));
  let reconstructed = Red_tree.text root in
  Printf.printf "match: %b\n" (source = reconstructed);
  [%expect {|
    children: 3
    match: true
    |}]

(** {1 CST Formatter Tests} *)

let%expect_test "cst formatter - simple formatting" =
  let result = Lina_format.Format_cst.format_string "let x = 1" in
  print_string result;
  [%expect {| let x = 1 |}]

let%expect_test "cst formatter - preserves line comments" =
  let result = Lina_format.Format_cst.format_string "let x = 1 -- comment" in
  print_string result;
  [%expect {| let x = 1 -- comment |}]

let%expect_test "cst formatter - preserves block comments" =
  let result = Lina_format.Format_cst.format_string "let (* comment *) x = 1" in
  print_string result;
  [%expect {| let (* comment *) x = 1 |}]

let%expect_test "cst formatter - preserves leading comments" =
  let result = Lina_format.Format_cst.format_string "-- header\nlet x = 1" in
  print_string result;
  [%expect {|
    -- header
    let x = 1
    |}]

let%expect_test "cst formatter - idempotent" =
  let source = "let x = 1 -- comment" in
  let first = Lina_format.Format_cst.format_string source in
  let second = Lina_format.Format_cst.format_string first in
  Printf.printf "idempotent: %b\n" (first = second);
  [%expect {| idempotent: true |}]

let%expect_test "cst formatter - multiple definitions with comments" =
  let source = "-- first\nlet x = 1\n-- second\nlet y = 2" in
  let result = Lina_format.Format_cst.format_string source in
  print_string result;
  [%expect {|
    -- first
    let x = 1
    -- second
    let y = 2
    |}]
