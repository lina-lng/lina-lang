(** Snapshot tests for human-oriented error messages.

    These tests verify that error messages are:
    - Written in conversational "we" voice
    - Include helpful suggestions where applicable
    - Provide context about what went wrong
    - Use proper formatting with backticks for code *)

(** Helper to capture error messages from compilation *)
let compile source =
  Typing.Types.reset_type_variable_id ();
  let test_options = Driver.Pipeline.{
    default_options with
    warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
  } in
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Ok _ -> "(no error)"
  | Error msg -> msg

let%expect_test "unbound variable - no suggestions" =
  print_endline (compile "let x = completely_unknown_name");
  [%expect {|
    error: Unbound Value --> <test>:1:9

       1 | let x = completely_unknown_name
                   ^^^^^^^^^^^^^^^^^^^^^^^

    We couldn't find a variable named `completely_unknown_name`
    |}]

let%expect_test "unbound variable - with suggestion" =
  print_endline (compile {|
let foo = 42
let x = fooo
|});
  [%expect {|
    error: Unbound Value --> <test>:3:9

       2 | let foo = 42
       3 | let x = fooo
                   ^^^^

    We couldn't find a variable named `fooo`

    Did you mean `foo`?
    |}]

let%expect_test "unbound constructor" =
  print_endline (compile "let x = Unknwn");
  [%expect {|
    error: Unbound Constructor --> <test>:1:9

       1 | let x = Unknwn
                   ^^^^^^

    We couldn't find a constructor named `Unknwn`
    |}]

let%expect_test "type mismatch - int vs string" =
  print_endline (compile {|let x = 1 + "hello"|});
  [%expect {|
    error: Type Mismatch --> <test>:1:13

       1 | let x = 1 + "hello"
                       ^^^^^^^
                       expected `int`, found `string`

    The `+` operator expects integers, but the right operand has type `string`.

    note: To convert a string to an int, use `int_of_string value`
    |}]

let%expect_test "type mismatch - function not applied" =
  print_endline (compile {|
let f x = x + 1
let x = f + 1
|});
  [%expect {|
    error: Type Mismatch --> <test>:3:9

       2 | let f x = x + 1
       3 | let x = f + 1
                   ^
                   expected `int`, found `int -> int`

    The `+` operator expects integers, but the left operand has type `int -> int`.

    This is a function. Did you forget to apply it to an argument?
    |}]

let%expect_test "type mismatch - in function argument" =
  print_endline (compile {|
let f (x : int) = x
let y = f "hello"
|});
  [%expect {|
    error: Type Mismatch --> <test>:3:11

       2 | let f (x : int) = x
       3 | let y = f "hello"
                     ^^^^^^^
                     expected `int`, found `string`

    This function's 1st argument expects type `int`, but you gave it `string`.

    note: To convert a string to an int, use `int_of_string value`
    |}]

let%expect_test "constructor with unexpected argument" =
  print_endline (compile {|
type t = A | B of int
let x = A 42
|});
  [%expect {|
    error: Arity Mismatch --> <test>:3:9

       2 | type t = A | B of int
       3 | let x = A 42
                   ^^^^

    The constructor `A` doesn't take any arguments, but we found one here.

    Use just `A` without an argument.
    |}]

let%expect_test "constructor missing argument" =
  print_endline (compile {|
type t = A | B of int
let x = B
|});
  [%expect {|
    error: Arity Mismatch --> <test>:3:9

       2 | type t = A | B of int
       3 | let x = B
                   ^

    The constructor `B` requires an argument, but none was provided.

    Use `B value` with an appropriate value.
    |}]

let%expect_test "or-pattern variable mismatch" =
  print_endline (compile {|
type t = A of int | B of int * int
let f x = match x with
  | A a | B (a, b) -> a
|});
  [%expect {|
    error: Type Mismatch --> <test>:4:5

       3 | let f x = match x with
       4 |   | A a | B (a, b) -> a
               ^^^^^^^^^^^^^^

    The variable `b` is bound on the right side of this or-pattern but not on the left side.

    Both sides of an or-pattern (p1 | p2) must bind exactly the same variables so that the code after the match can use them safely.
    |}]

let%expect_test "duplicate variable in pattern" =
  print_endline (compile "let f (x, x) = x");
  [%expect {|
    error: Type Mismatch --> <test>:1:11

       1 | let f (x, x) = x
                     ^

    The variable `x` is bound more than once in this pattern.

    It was first bound at line 1. Each variable can only be bound once in a pattern to avoid ambiguity about which value to use.
    |}]

let%expect_test "cannot open functor" =
  print_endline (compile {|
module F (X : sig end) = struct end
open F
|});
  [%expect {|
    error: Type Mismatch --> <test>:3:1

       2 | module F (X : sig end) = struct end
       3 | open F
           ^^^^^^

    We can't open a functor directly.

    Functors must be applied to an argument before their contents can be accessed. Try `open F(SomeModule)` instead.
    |}]

let%expect_test "apply non-functor" =
  print_endline (compile {|
module M = struct end
module N = M(struct end)
|});
  [%expect {|
    error: Type Mismatch --> <test>:3:12

       2 | module M = struct end
       3 | module N = M(struct end)
                      ^^^^^^^^^^^^^

    We can't apply this module because it's not a functor.

    Only functors can be applied with `F(Arg)` syntax. This module has a regular signature.
    |}]

let%expect_test "tuple arity mismatch" =
  print_endline (compile {|
let f (x : int * int) = x
let y = f (1, 2, 3)
|});
  [%expect {|
    error: Type Mismatch --> <test>:3:11

       2 | let f (x : int * int) = x
       3 | let y = f (1, 2, 3)
                     ^^^^^^^^^
                     expected `int * int`, found `int * int * int`

    This function's 1st argument expects type `int * int`, but you gave it `int * int * int`.
    |}]

let%expect_test "missing record field" =
  print_endline (compile {|
let f (r : { x : int }) = r.y
|});
  [%expect {|
    error: Type Mismatch --> <test>:2:27

       1 |
       2 | let f (r : { x : int }) = r.y
                                     ^^^

    This record doesn't have a field named `y`.

    Did you mean `x`?

    It has these accessible fields:
        .x
    |}]

let%expect_test "occurs check failure" =
  print_endline (compile "let rec f x = f");
  [%expect {|
    error: Type Mismatch --> <test>:1:9

       1 | let rec f x = f
                   ^^^^^^^
                   expected `'a`, found `'a`

    Infinite type: 'a occurs within 'a -> 'b

    This would create an infinite type. Consider using an explicit recursive type with a data constructor.
    |}]
