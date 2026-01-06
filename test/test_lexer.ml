open Test_helpers.Helpers

(* Basic token tests *)

let%expect_test "integer literals" =
  print_endline (show_tokens "0 42 999");
  [%expect
    {|
    (Lexer.INTEGER 0)
    (Lexer.INTEGER 42)
    (Lexer.INTEGER 999)
    Lexer.EOF
    |}]

let%expect_test "float literals" =
  print_endline (show_tokens "0.0 3.14 42.");
  [%expect
    {|
    (Lexer.FLOAT 0.)
    (Lexer.FLOAT 3.14)
    (Lexer.FLOAT 42.)
    Lexer.EOF
    |}]

let%expect_test "string literals" =
  print_endline (show_tokens {|"hello" "world\n"|});
  [%expect
    {|
    (Lexer.STRING "hello")
    (Lexer.STRING "world\n")
    Lexer.EOF
    |}]

let%expect_test "keywords" =
  print_endline (show_tokens "let rec in fun if then else type of and as true false");
  [%expect
    {|
    Lexer.LET
    Lexer.REC
    Lexer.IN
    Lexer.FUN
    Lexer.IF
    Lexer.THEN
    Lexer.ELSE
    Lexer.TYPE
    Lexer.OF
    Lexer.AND
    Lexer.AS
    Lexer.TRUE
    Lexer.FALSE
    Lexer.EOF
    |}]

let%expect_test "identifiers" =
  print_endline (show_tokens "foo bar x'");
  [%expect
    {|
    (Lexer.LOWERCASE_IDENTIFIER "foo")
    (Lexer.LOWERCASE_IDENTIFIER "bar")
    (Lexer.LOWERCASE_IDENTIFIER "x'")
    Lexer.EOF
    |}]

let%expect_test "uppercase identifiers (constructors)" =
  print_endline (show_tokens "Some None True");
  [%expect
    {|
    (Lexer.UPPERCASE_IDENTIFIER "Some")
    (Lexer.UPPERCASE_IDENTIFIER "None")
    (Lexer.UPPERCASE_IDENTIFIER "True")
    Lexer.EOF
    |}]

let%expect_test "type variables" =
  print_endline (show_tokens "'a 'b 'foo");
  [%expect
    {|
    (Lexer.TYPE_VARIABLE "a")
    (Lexer.TYPE_VARIABLE "b")
    (Lexer.TYPE_VARIABLE "foo")
    Lexer.EOF
    |}]

let%expect_test "operators" =
  print_endline (show_tokens "+ - * / < > <= >= == != -> =");
  [%expect
    {|
    Lexer.PLUS
    Lexer.MINUS
    Lexer.STAR
    Lexer.SLASH
    Lexer.LESS
    Lexer.GREATER
    Lexer.LESS_EQUAL
    Lexer.GREATER_EQUAL
    Lexer.EQUAL_EQUAL
    Lexer.NOT_EQUAL
    Lexer.ARROW
    Lexer.EQUAL
    Lexer.EOF
    |}]

let%expect_test "punctuation" =
  print_endline (show_tokens "( ) [ ] , ; : |");
  [%expect
    {|
    Lexer.LPAREN
    Lexer.RPAREN
    Lexer.LBRACKET
    Lexer.RBRACKET
    Lexer.COMMA
    Lexer.SEMICOLON
    Lexer.COLON
    Lexer.BAR
    Lexer.EOF
    |}]

(* Comments *)

let%expect_test "line comments" =
  print_endline (show_tokens "foo -- comment\nbar");
  [%expect
    {|
    (Lexer.LOWERCASE_IDENTIFIER "foo")
    (Lexer.LOWERCASE_IDENTIFIER "bar")
    Lexer.EOF
    |}]

let%expect_test "block comments" =
  print_endline (show_tokens "foo (* comment *) bar");
  [%expect
    {|
    (Lexer.LOWERCASE_IDENTIFIER "foo")
    (Lexer.LOWERCASE_IDENTIFIER "bar")
    Lexer.EOF
    |}]

let%expect_test "nested block comments" =
  print_endline (show_tokens "foo (* outer (* inner *) outer *) bar");
  [%expect
    {|
    (Lexer.LOWERCASE_IDENTIFIER "foo")
    (Lexer.LOWERCASE_IDENTIFIER "bar")
    Lexer.EOF
    |}]

(* Extended number literals *)

let%expect_test "hex integers" =
  print_endline (show_tokens "0x1A 0XFF 0xff");
  [%expect
    {|
    (Lexer.INTEGER 26)
    (Lexer.INTEGER 255)
    (Lexer.INTEGER 255)
    Lexer.EOF
    |}]

let%expect_test "binary integers" =
  print_endline (show_tokens "0b1010 0B1111");
  [%expect
    {|
    (Lexer.INTEGER 10)
    (Lexer.INTEGER 15)
    Lexer.EOF
    |}]

let%expect_test "integers with underscores" =
  print_endline (show_tokens "1_000_000 0xFF_FF 0b1010_1010");
  [%expect
    {|
    (Lexer.INTEGER 1000000)
    (Lexer.INTEGER 65535)
    (Lexer.INTEGER 170)
    Lexer.EOF
    |}]

let%expect_test "scientific notation" =
  print_endline (show_tokens "1e10 1E10 3.14e2 1.5e-3 2.5E+3");
  [%expect
    {|
    (Lexer.FLOAT 10000000000.)
    (Lexer.FLOAT 10000000000.)
    (Lexer.FLOAT 314.)
    (Lexer.FLOAT 0.0015)
    (Lexer.FLOAT 2500.)
    Lexer.EOF
    |}]

let%expect_test "floats with underscores" =
  print_endline (show_tokens "1_000.50 3.14_159");
  [%expect
    {|
    (Lexer.FLOAT 1000.5)
    (Lexer.FLOAT 3.14159)
    Lexer.EOF
    |}]

(* Extended string escapes *)

let%expect_test "basic string escapes" =
  print_endline (show_tokens {|"hello\nworld\t!"|});
  [%expect {|
    (Lexer.STRING "hello\nworld\t!")
    Lexer.EOF
    |}]

let%expect_test "null character escape" =
  print_endline (show_tokens {|"\0"|});
  [%expect {|
    (Lexer.STRING "\000")
    Lexer.EOF
    |}]

let%expect_test "hex escape" =
  print_endline (show_tokens {|"\x41\x42\x43"|});
  [%expect {|
    (Lexer.STRING "ABC")
    Lexer.EOF
    |}]

let%expect_test "unicode escape" =
  print_endline (show_tokens {|"\u{48}\u{65}\u{6C}\u{6C}\u{6F}"|});
  [%expect {|
    (Lexer.STRING "Hello")
    Lexer.EOF
    |}]

let%expect_test "unicode escape emoji" =
  (* U+1F600 = grinning face emoji, encoded as 4-byte UTF-8 *)
  print_endline (show_tokens {|"\u{1F600}"|});
  [%expect {|
    (Lexer.STRING "\240\159\152\128")
    Lexer.EOF
    |}]

(* Error cases *)

let%expect_test "unterminated string" =
  print_endline (show_tokens {|"hello|});
  [%expect {|
    File "<test>", line 1, characters 0-1:
    Lexer error: Unterminated string literal
    |}]

let%expect_test "unterminated block comment" =
  print_endline (show_tokens "(* comment");
  [%expect {|
    File "<test>", line 1, characters 0-2:
    Lexer error: Unterminated block comment
    |}]

let%expect_test "unknown escape sequence" =
  print_endline (show_tokens {|"\q"|});
  [%expect {|
    File "<test>", line 1, characters 0-1:
    Lexer error: Unknown escape sequence: \q
    |}]

let%expect_test "invalid hex escape" =
  print_endline (show_tokens {|"\xGG"|});
  [%expect
    {|
    File "<test>", line 1, characters 0-1:
    Lexer error: Invalid hex escape: expected \xHH (two hex digits)
    |}]

let%expect_test "trailing underscore in number" =
  print_endline (show_tokens "123_");
  [%expect
    {|
    File "<test>", line 1, characters 0-4:
    Lexer error: Number literal cannot have trailing underscore
    |}]

let%expect_test "consecutive underscores in number" =
  print_endline (show_tokens "1__0");
  [%expect
    {|
    File "<test>", line 1, characters 0-4:
    Lexer error: Number literal cannot have consecutive underscores
    |}]
