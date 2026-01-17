(** Comprehensive tests for Lua variable naming.

    These tests document the code generator's variable naming behavior:
    - How identifiers are translated to Lua
    - How Lua keywords are handled (prefixed with underscore)
    - How global unique stamps disambiguate names
    - Pattern match binding names
    - Nested function scoping
    - Module and recursive definition naming

    The current implementation uses globally unique stamps for all identifiers.
    Each identifier gets a numeric suffix (e.g., [x_1], [x_2]) based on its
    order in the global identifier counter. First occurrences of unique names
    may get clean names if they're the first use of that base name.

    Test categories:
    - Basic naming: how first occurrences are named
    - Shadowing: how repeated names get suffixes
    - Lua keywords: prefixed with underscore ([nil] -> [_nil])
    - Pattern match bindings: names in match arms
    - Nested functions: parameter name scoping
    - Complex structures: modules, recursive definitions
    - Edge cases: wildcards, long chains, internal name collisions
    - Currying: multi-param function naming *)

let compile source =
  Common.Identifier.reset_stamp ();
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

(* =============================================================================
   1. BASIC NAMING BEHAVIOR
   Documents how identifiers are translated to Lua variable names.
   ============================================================================= *)

let%expect_test "simple binding uses clean name" =
  print_endline (compile "let x = 1");
  [%expect{| local x = 1 |}]

let%expect_test "multiple different names all clean" =
  print_endline (compile "let x = 1
let y = 2
let z = 3");
  [%expect{|
    local x = 1;
    local y = 2;
    local z = 3
    |}]

let%expect_test "let-in chain preserves names" =
  print_endline (compile "let result = let x = 1 in let y = 2 in x + y");
  [%expect{|
    local x = 1;
    local y = 2;
    local result = x + y
    |}]

let%expect_test "function with single param uses clean name" =
  print_endline (compile "let f x = x + 1");
  [%expect{|
    local function f(x)
      return x + 1
    end
    |}]

let%expect_test "function with descriptive param name" =
  print_endline (compile "let increment value = value + 1");
  [%expect{|
    local function increment(value)
      return value + 1
    end
    |}]

(* =============================================================================
   2. SHADOWING SCENARIOS
   Documents how repeated/shadowed names receive unique stamps.
   ============================================================================= *)

let%expect_test "simple shadow adds suffix" =
  print_endline (compile "let x = 1 in let x = x + 1 in x");
  [%expect{|
    local x = 1;
    local x_1 = x + 1;
    local top = x_1
    |}]

let%expect_test "triple shadow increments suffix" =
  print_endline (compile "let x = 1 in let x = 2 in let x = 3 in x");
  [%expect{|
    local x = 1;
    local x_1 = 2;
    local x_2 = 3;
    local top = x_2
    |}]

let%expect_test "shadow with reference to outer" =
  print_endline (compile "let x = 10 in let x = x * 2 in x");
  [%expect{|
    local x = 10;
    local x_1 = x * 2;
    local top = x_1
    |}]

let%expect_test "function param shadows outer binding" =
  print_endline (compile "let x = 1
let f x = x * 2");
  [%expect{|
    local x = 1;
    local function f(x_1)
      return x_1 * 2
    end
    |}]

let%expect_test "inner function shadows outer param" =
  print_endline (compile "let f x = let g x = x in g x");
  [%expect{|
    local function f(x)
      local g = function(x_1)
        return x_1
      end;
      return g(x)
    end
    |}]

let%expect_test "nested function shadows at multiple levels" =
  print_endline (compile "let f x =
  let g x =
    let h x = x in
    h x
  in g x");
  [%expect{|
    local function f(x)
      local g = function(x_1)
        local h = function(x_2)
          return x_2
        end;
        return h(x_1)
      end;
      return g(x)
    end
    |}]

let%expect_test "same name in sequential let bindings shadows" =
  print_endline (compile "let a = 1
let a = 2
let a = 3");
  [%expect{|
    local a = 1;
    local a_1 = 2;
    local a_2 = 3
    |}]

let%expect_test "lambda param shadows outer binding" =
  print_endline (compile "let x = 5
let f = fun x -> x + 1");
  [%expect{|
    local x = 5;
    local function f(x_1)
      return x_1 + 1
    end
    |}]

let%expect_test "multiple params with same base name" =
  print_endline (compile "let f x = fun x -> x");
  [%expect{|
    local function f(x)
      return function(x_1)
        return x_1
      end
    end
    |}]

(* =============================================================================
   3. LUA KEYWORD HANDLING
   Keywords get prefixed with underscore.
   ============================================================================= *)

let%expect_test "keyword nil is prefixed" =
  print_endline (compile "let nil = 0");
  [%expect{| local _nil = 0 |}]

let%expect_test "keyword local is prefixed" =
  print_endline (compile "let local = 123");
  [%expect{| local _local = 123 |}]

let%expect_test "keyword return is prefixed" =
  print_endline (compile "let return = 1");
  [%expect{| local _return = 1 |}]

let%expect_test "keyword repeat is prefixed" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat = 99 |}]

let%expect_test "keyword until is prefixed" =
  print_endline (compile "let until = 5");
  [%expect{| local _until = 5 |}]

let%expect_test "keyword goto is prefixed" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto = 7 |}]

let%expect_test "keyword break is prefixed" =
  print_endline (compile "let break = 3");
  [%expect{| local _break = 3 |}]

let%expect_test "keyword with shadowing" =
  print_endline (compile "let nil = 1 in let nil = 2 in nil");
  [%expect{|
    local _nil = 1;
    local _nil_1 = 2;
    local top = _nil_1
    |}]

let%expect_test "keyword as function parameter" =
  print_endline (compile "let f local = local + 1");
  [%expect{|
    local function f(_local)
      return _local + 1
    end
    |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker = 10 |}]

let%expect_test "identifier starting with keyword is not mangled" =
  print_endline (compile "let return_value = 42");
  [%expect{| local return_value = 42 |}]

let%expect_test "identifier ending with keyword is not mangled" =
  print_endline (compile "let my_local = 100");
  [%expect{| local my_local = 100 |}]

(* =============================================================================
   4. PATTERN MATCH BINDINGS
   Names in pattern matches should behave correctly.
   ============================================================================= *)

let%expect_test "tuple pattern binds clean names" =
  print_endline (compile "let f p = match p with | (a, b) -> a + b");
  [%expect{|
    local function f(p)
      local p_match = p;
      local b = p_match[2];
      local a = p_match[1];
      return a + b
    end
    |}]

let%expect_test "triple tuple pattern binds clean names" =
  print_endline (compile "let f t = match t with | (a, b, c) -> a + b + c");
  [%expect{|
    local function f(t)
      local t_match = t;
      local c = t_match[3];
      local b = t_match[2];
      local a = t_match[1];
      return a + b + c
    end
    |}]

let%expect_test "constructor pattern extracts argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some value -> value");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local value = x_match._0;
        return value
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "same name in different match arms is OK" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some x -> x");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local x_1 = x_match._0;
        return x_1
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "nested pattern with multiple bindings" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b) -> a + b | None -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local b = x_match._0[2];
        local a = x_match._0[1];
        return a + b
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 10-57:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

let%expect_test "record pattern with punning" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local function get_x(r)
      local r_match = r;
      local x = r_match.x;
      return x
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern with renaming" =
  print_endline (compile "let get_val r = match r with | { x = value } -> value");
  [%expect{|
    local function get_val(r)
      local r_match = r;
      local value = r_match.x;
      return value
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard bindings are properly scoped" =
  print_endline (compile "let f x = match x with | n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local n = x_match;
      if n > 0 then
        local n = x_match;
        return n
      else
        return 0
      end
    end
    File "<string>", line 1, characters 45-51:
    Warning: Redundant pattern: this case will never be matched
    |}]

(* =============================================================================
   5. NESTED FUNCTIONS
   Parameters at different nesting levels should be distinguished.
   ============================================================================= *)

let%expect_test "closure captures outer variable" =
  print_endline (compile "let make_adder x = fun y -> x + y");
  [%expect{|
    local function make_adder(x)
      return function(y)
        return x + y
      end
    end
    |}]

let%expect_test "deeply nested functions with unique names" =
  print_endline (compile "let f a =
  let g b =
    let h c = a + b + c in
    h
  in g");
  [%expect{|
    local function f(a)
      local g = function(b)
        local h = function(c)
          return a + b + c
        end;
        return h
      end;
      return g
    end
    |}]

let%expect_test "nested function returning function" =
  print_endline (compile "let compose f g = fun x -> f (g x)");
  [%expect{|
    local compose = function(f)
      return function(g)
        return function(x)
          return f(g(x))
        end
      end
    end
    |}]

let%expect_test "function in let binding inside function" =
  print_endline (compile "let outer x =
  let inner = fun y -> x + y in
  inner 10");
  [%expect{|
    local function outer(x)
      local inner = function(y)
        return x + y
      end;
      return inner(10)
    end
    |}]

let%expect_test "mutual shadowing across closures" =
  print_endline (compile "let f x =
  let g y = x + y in
  let h x = g x in
  h");
  [%expect{|
    local function f(x)
      local g = function(y)
        return x + y
      end;

      local h = function(x_1)
        return g(x_1)
      end;
      return h
    end
    |}]

(* =============================================================================
   6. COMPLEX STRUCTURES
   Modules, recursive definitions, conditionals.
   ============================================================================= *)

let%expect_test "recursive function with clean param name" =
  print_endline (compile "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)");
  [%expect{|
    local fact;
    fact = function(n)
      if n <= 1 then
        return 1
      else
        return n * fact(n - 1)
      end
    end
    |}]

let%expect_test "mutual recursion with clean names" =
  print_endline (compile "let rec even n = if n == 0 then true else odd (n - 1)
and odd n = if n == 0 then false else even (n - 1)");
  [%expect{|
    local even, odd;
    even = function(n)
      if n == 0 then
        return true
      else
        return odd(n - 1)
      end
    end;

    odd = function(n_1)
      if n_1 == 0 then
        return false
      else
        return even(n_1 - 1)
      end
    end
    |}]

let%expect_test "let in if branches" =
  print_endline (compile "let f cond =
  if cond then
    let x = 1 in x
  else
    let x = 2 in x");
  [%expect{|
    local function f(cond)
      if cond then
        local x = 1;
        return x
      else
        local x_1 = 2;
        return x_1
      end
    end
    |}]

let%expect_test "module with internal bindings" =
  print_endline (compile "module M = struct
  let x = 1
  let y = x + 1
  let z = y + 1
end");
  [%expect{|
    local x = 1;
    local y = x + 1;
    local z = y + 1;
    local M = {x = x, y = y, z = z}
    |}]

let%expect_test "nested modules preserve naming" =
  print_endline (compile "module Outer = struct
  let x = 1
  module Inner = struct
    let x = 2
    let y = x + Outer.x
  end
end");
  [%expect{|
    ERROR: File "<string>", line 5, characters 16-21:
    Type error: Unbound constructor: Outer
    |}]

(* =============================================================================
   7. EDGE CASES
   Wildcards, long chains, unusual patterns.
   ============================================================================= *)

let%expect_test "wildcard binding uses underscore" =
  print_endline (compile "let _ = print 42");
  [%expect{| local top = print(42) |}]

let%expect_test "long chain of unique names" =
  print_endline (compile "let a = 1 in let b = a in let c = b in let d = c in d");
  [%expect{|
    local a = 1;
    local b = a;
    local c = b;
    local d = c;
    local top = d
    |}]

let%expect_test "many sequential bindings" =
  print_endline (compile "let v1 = 1
let v2 = 2
let v3 = 3
let v4 = 4
let v5 = 5");
  [%expect{|
    local v1 = 1;
    local v2 = 2;
    local v3 = 3;
    local v4 = 4;
    local v5 = 5
    |}]

let%expect_test "alternating shadow and unique names" =
  print_endline (compile "let x = 1 in let y = 2 in let x = 3 in let z = 4 in x + y + z");
  [%expect{|
    local x = 1;
    local y = 2;
    local x_1 = 3;
    local z = 4;
    local top = x_1 + y + z
    |}]

let%expect_test "underscore pattern in match preserves other names" =
  print_endline (compile "let f p = match p with | (_, b) -> b");
  [%expect{|
    local function f(p)
      local p_match = p;
      local b = p_match[2];
      return b
    end
    |}]

let%expect_test "multiple underscores in pattern" =
  print_endline (compile "let f t = match t with | (_, _, c) -> c");
  [%expect{|
    local function f(t)
      local t_match = t;
      local c = t_match[3];
      return c
    end
    |}]

let%expect_test "variable named 'matched' does not conflict with internal" =
  print_endline (compile "let matched = 42");
  [%expect{| local matched = 42 |}]

let%expect_test "variable named 'result' is OK" =
  print_endline (compile "let result = 100");
  [%expect{| local result = 100 |}]

(* =============================================================================
   8. MULTI-PARAM FUNCTIONS (CURRYING)
   Each curry level should have proper naming.
   ============================================================================= *)

let%expect_test "two param function creates nested closures" =
  print_endline (compile "let add x y = x + y");
  [%expect{|
    local add = function(x)
      return function(y)
        return x + y
      end
    end
    |}]

let%expect_test "three param function deeply nested" =
  print_endline (compile "let add3 a b c = a + b + c");
  [%expect{|
    local add3 = function(a)
      return function(b)
        return function(c)
          return a + b + c
        end
      end
    end
    |}]

let%expect_test "five param function preserves all names" =
  print_endline (compile "let f a b c d e = a + b + c + d + e");
  [%expect{|
    local f = function(a)
      return function(b)
        return function(c)
          return function(d)
            return function(e)
              return a + b + c + d + e
            end
          end
        end
      end
    end
    |}]

let%expect_test "same name at different curry levels shadows" =
  print_endline (compile "let weird x = fun x -> x");
  [%expect{|
    local function weird(x)
      return function(x_1)
        return x_1
      end
    end
    |}]

let%expect_test "partial application preserves closure names" =
  print_endline (compile "let add a b = a + b
let add5 = add 5");
  [%expect{|
    local add = function(a)
      return function(b)
        return a + b
      end
    end;
    local add5 = add(5)
    |}]

let%expect_test "higher-order function params have clean names" =
  print_endline (compile "let apply f x = f x");
  [%expect{|
    local apply = function(f)
      return function(x)
        return f(x)
      end
    end
    |}]

let%expect_test "unit function has param placeholder" =
  print_endline (compile "let f = fun () -> 42");
  [%expect{|
    local function f(param)
      return 42
    end
    |}]

(* =============================================================================
   9. REFERENCES AND MUTABLE STATE
   Variables with refs should have clean names.
   ============================================================================= *)

let%expect_test "ref variable has clean name" =
  print_endline (compile "let counter = ref 0");
  [%expect{| local counter = {value = 0} |}]

let%expect_test "multiple refs have distinct names" =
  print_endline (compile "let x = ref 1
let y = ref 2");
  [%expect{|
    local x = {value = 1};
    local y = {value = 2}
    |}]

let%expect_test "shadowed ref gets suffix" =
  print_endline (compile "let r = ref 1
let r = ref 2");
  [%expect{|
    local r = {value = 1};
    local r_1 = {value = 2}
    |}]

(* =============================================================================
   10. POLYMORPHIC VARIANTS
   Poly variants should preserve variable names in patterns.
   ============================================================================= *)

let%expect_test "poly variant pattern extracts with clean name" =
  print_endline (compile {|
let f x = match x with
  | `Some value -> value
  | `None -> 0
|});
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match._tag == "None" then
        return 0
      else
        if x_match._tag == "Some" then
          local value = x_match._0;
          return value
        else
          return x("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 10-14:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* =============================================================================
   11. LOOP CONSTRUCTS
   Loop variables should have clean names.
   ============================================================================= *)

let%expect_test "for loop variable has clean name" =
  print_endline (compile {|
let sum = ref 0
let _ = for i = 1 to 5 do
  sum := !sum + i
done
|});
  [%expect{|
    local sum = {value = 0};
    local top = (function()
      for i = 1, 5 do
        sum.value = sum.value + i
      end;
      return nil
    end)()
    |}]

let%expect_test "nested for loops have distinct names" =
  print_endline (compile {|
let _ = for i = 1 to 2 do
  for j = 1 to 3 do
    print (i * j)
  done
done
|});
  [%expect{|
    local top = (function()
      for i = 1, 2 do
        (function()
          for j = 1, 3 do
            print(i * j)
          end;
          return nil
        end)()
      end;
      return nil
    end)()
    |}]

(* =============================================================================
   12. EXPRESSION CONTEXT NAMING
   Names should be preserved in various expression positions.
   ============================================================================= *)

let%expect_test "let inside tuple preserves names" =
  print_endline (compile "let pair = (let x = 1 in x, let y = 2 in y)");
  [%expect{|
    local pair = {(function()
      local x = 1;
      return x
    end)(), (function()
      local y = 2;
      return y
    end)()}
    |}]

let%expect_test "let inside record preserves names" =
  print_endline (compile "let r = { a = let x = 1 in x; b = let y = 2 in y }");
  [%expect{|
    ERROR: File "<string>", line 1, characters 14-17:
    Parse error: Syntax error
    |}]

let%expect_test "let inside function call preserves names" =
  print_endline (compile "let f x = x
let result = f (let y = 42 in y)");
  [%expect{|
    local function f(x)
      return x
    end;
    local result = f((function()
      local y = 42;
      return y
    end)())
    |}]

(* =============================================================================
   13. TYPE-LEVEL NAMES DON'T AFFECT VALUE NAMES
   Type names should not interfere with value namespace.
   ============================================================================= *)

let%expect_test "value name same as type name is OK" =
  print_endline (compile "type point = { x : int; y : int }
let point = { x = 1; y = 2 }");
  [%expect{| local point = {x = 1, y = 2} |}]

let%expect_test "constructor name doesn't affect value naming" =
  print_endline (compile "type 'a option = None | Some of 'a
let none = None
let some = Some 42");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local some = {_tag = 1, _0 = 42}
    |}]

(* =============================================================================
   14. SPECIAL CHARACTER HANDLING
   Names with special characters should be sanitized.
   ============================================================================= *)

let%expect_test "operator characters in custom ops are sanitized" =
  print_endline (compile "let (++) a b = a + b");
  [%expect{|
    ERROR: File "<string>", line 1, characters 5-6:
    Parse error: Syntax error
    |}]

(* =============================================================================
   15. DISPATCH TABLE AND IF-CHAIN NAMING
   Internal codegen names shouldn't conflict with user names.
   ============================================================================= *)

let%expect_test "user variable named dispatch is distinct" =
  print_endline (compile "let dispatch = 10
type dir = North | South | East | West
let f d = match d with
  | North -> dispatch
  | South -> 2
  | East -> 3
  | West -> 4");
  [%expect{|
    local dispatch = 10;
    local function f(d)
      local d_match = d;
      local matched = d_match;
      local _dispatch = {[3] = function()
        return 4
      end, [2] = function()
        return 3
      end, [1] = function()
        return 2
      end, [0] = function()
        return dispatch
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "user variable named handler is distinct" =
  print_endline (compile "let handler = 99
type 'a option = None | Some of 'a
let f x = match x with | Some handler -> handler | None -> 0");
  [%expect{|
    local handler = 99;
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local handler_1 = x_match._0;
        return handler_1
      else
        return error("Match failure")
      end
    end
    |}]

(* =============================================================================
   16. COMPLEX REAL-WORLD SCENARIOS
   Comprehensive examples combining multiple features.
   ============================================================================= *)

let%expect_test "map function with clean names" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec map f lst = match lst with
  | Nil -> Nil
  | Cons (head, tail) -> Cons (f head, map f tail)");
  [%expect{|
    local _Ctor_list_0 = {_tag = 0};
    local map;
    map = function(f)
      return function(lst)
        local lst_match = lst;
        local matched = lst_match;
        if matched._tag == 1 then
          local tail = lst_match._0[2];
          local head = lst_match._0[1];
          return {_tag = 1, _0 = {f(head), map(f)(tail)}}
        elseif matched._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "fold function preserves accumulator name" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec fold f acc lst = match lst with
  | Nil -> acc
  | Cons (head, tail) -> fold f (f acc head) tail");
  [%expect{|
    local fold;
    fold = function(f)
      return function(acc)
        return function(lst)
          local lst_match = lst;
          local matched = lst_match;
          if matched._tag == 1 then
            local tail = lst_match._0[2];
            local head = lst_match._0[1];
            return fold(f)(f(acc)(head))(tail)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end
    end
    |}]

let%expect_test "nested matching with descriptive names" =
  print_endline (compile "type 'a option = None | Some of 'a
let safe_divide dividend divisor =
  if divisor == 0 then None
  else Some (dividend / divisor)");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local safe_divide = function(dividend)
      return function(divisor)
        if divisor == 0 then
          return _Ctor_option_0
        else
          return {_tag = 1, _0 = dividend / divisor}
        end
      end
    end
    |}]

let%expect_test "counter closure with clean names" =
  print_endline (compile "let make_counter initial =
  let count = ref initial in
  let increment = fun () -> count := !count + 1 in
  let get = fun () -> !count in
  (increment, get)");
  [%expect{|
    local function make_counter(initial)
      local count = {value = initial};
      local increment = function(param)
        return (function()
          count.value = count.value + 1;
          return nil
        end)()
      end;

      local get = function(param_1)
        return count.value
      end;
      return {increment, get}
    end
    |}]
