let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "record literal generates lua table" =
  print_endline (compile "let r = { x = 1; y = 2 }");
  [%expect{| local r = {x = 1, y = 2} |}]

let%expect_test "record field access generates dot notation" =
  print_endline (compile "let r = { x = 42 }
let v = r.x");
  [%expect{|
    local r = {x = 42};
    local v = r.x
    |}]

let%expect_test "record update generates shallow copy" =
  print_endline (compile "let r = { x = 1; y = 2 }
let r2 = { r with x = 10 }");
  [%expect{|
    local r = {x = 1, y = 2};
    local r2 = (function()
      local _result = {};
      for _k, _v in pairs(r) do
        _result[_k] = _v
      end;
      _result.x = 10;
      return _result
    end)()
    |}]

let%expect_test "record punning expands to variable" =
  print_endline (compile "let x = 1
let y = 2
let r = { x; y }");
  [%expect{|
    local x = 1;
    local y = 2;
    local r = {x = x, y = y}
    |}]

let%expect_test "nested record access" =
  print_endline (compile "let outer = { inner = { value = 42 } }
let v = outer.inner.value");
  [%expect{|
    local outer = {inner = {value = 42}};
    local v = outer.inner.value
    |}]

let%expect_test "match with integer patterns" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 1 -> 2 | n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match == 1 then
        return 2
      else
        if x_match == 0 then
          return 1
        else
          local n = x_match;
          return n
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local function sum_pair(p)
      local p_match = p;
      local b = p_match[2];
      local a = p_match[1];
      return a + b
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local function abs(n)
      local n_match = n;
      local x = n_match;
      if x < 0 then
        local x = n_match;
        return 0 - x
      else
        local x_1 = n_match;
        return x_1
      end
    end
    File "<string>", line 1, characters 51-57:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 1 then
        return true
      elseif matched._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "match extracts constructor argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_or_default opt default = match opt with | None -> default | Some x -> x");
  [%expect{|
    local get_or_default = function(opt)
      return function(default)
        local opt_match = opt;
        local matched = opt_match;
        if matched._tag == 1 then
          local x = opt_match._0;
          return x
        elseif matched._tag == 0 then
          return default
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "constructor expression generates tagged table" =
  print_endline (compile "type 'a option = None | Some of 'a
let none = None
let some_val = Some 42");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local some_val = {_tag = 1, _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local function get_x(r)
      return r.x
    end;
    local p1 = {x = 1, y = 2};
    local p2 = {x = 10, z = 20};
    local v1 = get_x(p1);
    local v2 = get_x(p2)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r = {a = 1, b = 2, c = 3};
    local r2 = (function()
      local _result = {};
      for _k, _v in pairs(r) do
        _result[_k] = _v
      end;
      _result.a = 10;
      _result.c = 30;
      return _result
    end)()
    |}]

let%expect_test "function returning record" =
  print_endline (compile "let make_point x y = { x = x; y = y }");
  [%expect{|
    local make_point = function(x)
      return function(y)
        return {x = x, y = y}
      end
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local function f(p)
      local tuple = p;
      local a = tuple[1];
      local b = tuple[2];
      return a + b
    end
    |}]

let%expect_test "nested match" =
  print_endline (compile "type 'a option = None | Some of 'a
let f opt = match opt with
  | None -> 0
  | Some x -> match x with
    | 0 -> 1
    | n -> n + 1");
  [%expect{|
    local function f(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 1 then
        local x = opt_match._0;
        local x_match = x;
        if x_match == 0 then
          return 1
        else
          local n = x_match;
          return n + 1
        end
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x = 1;
    local r = {a = x + 1, b = x * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local function f(x)
      local x_match = x;
      return 42
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local function classify(n)
      local n_match = n;
      local x = n_match;
      if x < 0 then
        local x = n_match;
        return 0 - 1
      else
        local x_1 = n_match;
        if x_1 == 0 then
          local x_1 = n_match;
          return 0
        else
          local x_2 = n_match;
          return 1
        end
      end
    end
    File "<string>", line 3, characters 4-22:
    Warning: Redundant pattern: this case will never be matched
    File "<string>", line 4, characters 4-10:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "type error: record field type mismatch" =
  print_endline (compile "let r = { x = 1 }
let s = { r with x = true }");
  [%expect{|
    ERROR: File "<string>", line 2, characters 8-27:
    Type error: Type mismatch: expected bool, got int
    Expected: bool
    Actual: int
    |}]

let%expect_test "type error: accessing nonexistent field on closed record" =
  print_endline (compile "let r = { x = 1 }
let v = r.y");
  [%expect{|
    ERROR: File "<string>", line 2, characters 8-11:
    Type error: Missing field 'y' in closed record
    Expected: {}
    Actual: { y : 't1 }
    |}]

let%expect_test "type error: match arm type mismatch" =
  print_endline (compile "let f x = match x with | 0 -> 1 | n -> true");
  [%expect{|
    ERROR: File "<string>", line 1, characters 34-43:
    Type error: Type mismatch: expected int, got bool
    Expected: int
    Actual: bool
    |}]

let%expect_test "type error: guard must be bool" =
  print_endline (compile "let f x = match x with | n when n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      local n = x_match;
      if n then
        local n = x_match;
        return n
      else
        return error("Match failure")
      end
    end
    File "<string>", line 1, characters 10-38:
    Warning: Non-exhaustive pattern matching, missing case: true
    |}]

(* === Pattern Matching: Exhaustiveness Tests === *)

let%expect_test "exhaustive match on option - no warning" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local n = x_match._0;
        return n
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "non-exhaustive match - missing None" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local n = x_match._0;
        return n
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 10-36:
    Warning: Non-exhaustive pattern matching, missing case: None
    |}]

let%expect_test "non-exhaustive match - missing Some" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 10-34:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

let%expect_test "exhaustive match on bool - no warning" =
  print_endline (compile "let f b = match b with | true -> 1 | false -> 0");
  [%expect{|
    local function f(b)
      local b_match = b;
      if b_match == false then
        return 0
      else
        if b_match == true then
          return 1
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "non-exhaustive match on bool - missing false" =
  print_endline (compile "let f b = match b with | true -> 1");
  [%expect{|
    local function f(b)
      local b_match = b;
      if b_match == true then
        return 1
      else
        return error("Match failure")
      end
    end
    File "<string>", line 1, characters 10-34:
    Warning: Non-exhaustive pattern matching, missing case: false
    |}]

let%expect_test "wildcard makes match exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n -> n | _ -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local n = x_match._0;
        return n
      else
        return 0
      end
    end
    |}]

(* === Pattern Matching: Redundancy Tests === *)

let%expect_test "redundant pattern after wildcard" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | _ -> 0 | None -> 1");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      else
        return 0
      end
    end
    File "<string>", line 2, characters 34-43:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "redundant pattern - duplicate constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some n -> n | None -> 2");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local n = x_match._0;
        return n
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 51-60:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "redundant integer pattern" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 0 -> 2 | n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match == 0 then
        return 1
      else
        local n = x_match;
        return n
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local function to_num(c)
      local c_match = c;
      local matched = c_match;
      if matched._tag == 2 then
        return 2
      elseif matched._tag == 1 then
        return 1
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "nested tuple patterns" =
  print_endline (compile "let f p = match p with | ((a, b), c) -> a + b + c");
  [%expect{|
    local function f(p)
      local p_match = p;
      local c = p_match[2];
      local b = p_match[1][2];
      local a = p_match[1][1];
      return a + b + c
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local function sum(p)
      local p_match = p;
      local matched = p_match;
      if matched._tag == 0 then
        local b = p_match._0[2];
        local a = p_match._0[1];
        return a + b
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "match on record pattern" =
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

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local function sum_xy(r)
      local r_match = r;
      local y = r_match.y;
      local x = r_match.x;
      return x + y
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local n = x_match._0;
        if n > 0 then
          local n = x_match._0;
          return n
        else
          return 0
        end
      else
        return 0
      end
    end
    |}]

let%expect_test "exhaustive with guarded patterns conservatively warns" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | None -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local n = x_match._0;
        if n > 0 then
          local n = x_match._0;
          return n
        else
          return error("Match failure")
        end
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 10-59:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

(* === Pattern Matching: Complex Scenarios === *)

let%expect_test "list-like ADT with recursive match" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let head l = match l with | Nil -> 0 | Cons (h, t) -> h");
  [%expect{|
    local function head(l)
      local l_match = l;
      local matched = l_match;
      if matched._tag == 1 then
        local t = l_match._0[2];
        local h = l_match._0[1];
        return h
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "either type with two type params" =
  print_endline (compile "type ('a, 'b) either = Left of 'a | Right of 'b
let is_left e = match e with | Left _ -> true | Right _ -> false");
  [%expect{|
    local function is_left(e)
      local e_match = e;
      local matched = e_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end
    |}]

(* === Pattern Matching: Constant Patterns === *)

let%expect_test "match with string patterns" =
  print_endline (compile {|let greet name = match name with | "Alice" -> "Hi Alice" | "Bob" -> "Hey Bob" | _ -> "Hello"|});
  [%expect{|
    local function greet(name)
      local name_match = name;
      if name_match == "Bob" then
        return "Hey Bob"
      else
        if name_match == "Alice" then
          return "Hi Alice"
        else
          return "Hello"
        end
      end
    end
    |}]

let%expect_test "match with float patterns" =
  print_endline (compile "let classify f = match f with | 0.0 -> 0 | 1.0 -> 1 | _ -> 2");
  [%expect{|
    ERROR: File "<string>", line 1, characters 32-35:
    Parse error: Syntax error
    |}]

let%expect_test "match with boolean literal patterns" =
  print_endline (compile "let negate b = match b with | true -> false | false -> true");
  [%expect{|
    local function negate(b)
      local b_match = b;
      if b_match == false then
        return true
      else
        if b_match == true then
          return false
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "match with unit pattern" =
  print_endline (compile "let f u = match u with | () -> 42");
  [%expect{|
    local function f(u)
      local u_match = u;
      if u_match == nil then
        return 42
      else
        return error("Match failure")
      end
    end
    File "<string>", line 1, characters 10-33:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Tuple Patterns === *)

let%expect_test "match with 3-element tuple" =
  print_endline (compile "let sum3 t = match t with | (a, b, c) -> a + b + c");
  [%expect{|
    local function sum3(t)
      local t_match = t;
      local c = t_match[3];
      local b = t_match[2];
      local a = t_match[1];
      return a + b + c
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local function sum4(t)
      local t_match = t;
      local d = t_match[4];
      local c = t_match[3];
      local b = t_match[2];
      local a = t_match[1];
      return a + b + c + d
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local function first(t)
      local t_match = t;
      local a = t_match[1];
      return a
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local function check(t)
      local t_match = t;
      if t_match[1] == 1 then
        local y = t_match[2];
        return y + 1
      else
        if t_match[1] == 0 then
          local x = t_match[2];
          return x
        else
          local z = t_match[2];
          return z + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    local function deep(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local matched = x_match._0;
        if matched._tag == 1 then
          local matched = x_match._0._0;
          if matched._tag == 1 then
            local n = x_match._0._0._0;
            return n
          else
            return 0
          end
        else
          return 0
        end
      else
        return 0
      end
    end
    |}]

let%expect_test "mixed nesting - tuple in constructor in tuple" =
  print_endline (compile "type 'a option = None | Some of 'a
let extract t = match t with | (Some (a, b), c) -> a + b + c | (None, d) -> d");
  [%expect{|
    local function extract(t)
      local t_match = t;
      local matched = t_match[1];
      if matched._tag == 0 then
        local d = t_match[2];
        return d
      elseif matched._tag == 1 then
        local c = t_match[2];
        local b = t_match[1]._0[2];
        local a = t_match[1]._0[1];
        return a + b + c
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 16-77:
    Warning: Non-exhaustive pattern matching, missing case: (_, _)
    |}]

let%expect_test "constructor containing tuple containing constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
type wrapper = Wrap of ('a option * int)
let unwrap w = match w with | Wrap (Some x, n) -> x + n | Wrap (None, n) -> n");
  [%expect{|
    local function unwrap(w)
      local w_match = w;
      local matched = w_match;
      if matched._tag == 0 then
        local matched = w_match._0[1];
        if matched._tag == 0 then
          local n = w_match._0[2];
          return n
        elseif matched._tag == 1 then
          local n_1 = w_match._0[2];
          local x = w_match._0[1]._0;
          return x + n_1
        else
          return error("Match failure")
        end
      else
        return error("Match failure")
      end
    end
    |}]

(* === Pattern Matching: Multiple Variants === *)

let%expect_test "four constructor variant - exhaustive" =
  print_endline (compile "type dir = North | South | East | West
let to_num d = match d with | North -> 0 | South -> 1 | East -> 2 | West -> 3");
  [%expect{|
    local function to_num(d)
      local d_match = d;
      local matched = d_match;
      local _dispatch = {[3] = function()
        return 3
      end, [2] = function()
        return 2
      end, [1] = function()
        return 1
      end, [0] = function()
        return 0
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "four constructor variant - missing one" =
  print_endline (compile "type dir = North | South | East | West
let to_num d = match d with | North -> 0 | South -> 1 | East -> 2");
  [%expect{|
    local function to_num(d)
      local d_match = d;
      local matched = d_match;
      if matched._tag == 2 then
        return 2
      elseif matched._tag == 1 then
        return 1
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 15-65:
    Warning: Non-exhaustive pattern matching, missing case: West
    |}]

let%expect_test "five constructor variant with wildcard" =
  print_endline (compile "type day = Mon | Tue | Wed | Thu | Fri
let is_monday d = match d with | Mon -> true | _ -> false");
  [%expect{|
    local function is_monday(d)
      local d_match = d;
      local matched = d_match;
      if matched._tag == 0 then
        return true
      else
        return false
      end
    end
    |}]

let%expect_test "variant with mix of nullary and unary constructors" =
  print_endline (compile "type expr = Zero | One | Add of (expr * expr)
let is_value e = match e with | Zero -> true | One -> true | Add _ -> false");
  [%expect{|
    local function is_value(e)
      local e_match = e;
      local matched = e_match;
      if matched._tag == 2 then
        return false
      elseif matched._tag == 1 then
        return true
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end
    |}]

(* === Pattern Matching: Variable Bindings === *)

let%expect_test "same variable name in different arms" =
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

let%expect_test "binding entire constructor argument" =
  print_endline (compile "type pair = Pair of (int * int)
let get_pair p = match p with | Pair x -> x");
  [%expect{|
    local function get_pair(p)
      local p_match = p;
      local matched = p_match;
      if matched._tag == 0 then
        local x = p_match._0;
        return x
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local c = x_match._0[3];
        local b = x_match._0[2];
        local a = x_match._0[1];
        return a + b + c
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 10-64:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

(* === Pattern Matching: Guards === *)

let%expect_test "guard accessing multiple bound variables" =
  print_endline (compile "let check p = match p with | (a, b) when a > b -> a | (a, b) -> b");
  [%expect{|
    local function check(p)
      local p_match = p;
      local b = p_match[2];
      local a = p_match[1];
      if a > b then
        local b = p_match[2];
        local a = p_match[1];
        return a
      else
        local b_1 = p_match[2];
        local a_1 = p_match[1];
        return b_1
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range(n)
      local n_match = n;
      local x = n_match;
      if x > 0 then
        local x = n_match;
        if x < 10 then
          return 1
        else
          return 2
        end
      else
        return 0
      end
    end
    File "<string>", line 1, characters 73-79:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard on constructor with argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let positive opt = match opt with | Some n when n > 0 -> true | Some _ -> false | None -> false");
  [%expect{|
    local function positive(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        local n = opt_match._0;
        if n > 0 then
          local n = opt_match._0;
          return true
        else
          return false
        end
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple guards same pattern" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x > 100 -> 2
  | x when x > 50 -> 1
  | _ -> 0");
  [%expect{|
    local function classify(n)
      local n_match = n;
      local x = n_match;
      if x < 0 then
        local x = n_match;
        return 0 - 1
      else
        local x_1 = n_match;
        if x_1 > 100 then
          local x_1 = n_match;
          return 2
        else
          local x_2 = n_match;
          if x_2 > 50 then
            local x_2 = n_match;
            return 1
          else
            return 0
          end
        end
      end
    end
    File "<string>", line 3, characters 4-23:
    Warning: Redundant pattern: this case will never be matched
    File "<string>", line 4, characters 4-22:
    Warning: Redundant pattern: this case will never be matched
    File "<string>", line 5, characters 4-10:
    Warning: Redundant pattern: this case will never be matched
    |}]

(* === Pattern Matching: Recursive Functions === *)

let%expect_test "recursive function with pattern matching" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec length l = match l with | Nil -> 0 | Cons (_, t) -> 1 + length t");
  [%expect{|
    local length;
    length = function(l)
      local l_match = l;
      local matched = l_match;
      if matched._tag == 1 then
        local t = l_match._0[2];
        return 1 + length(t)
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "recursive function with multiple pattern arms" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum l = match l with | Nil -> 0 | Cons (h, t) -> h + sum t");
  [%expect{|
    local sum;
    sum = function(l)
      local l_match = l;
      local matched = l_match;
      if matched._tag == 1 then
        local t = l_match._0[2];
        local h = l_match._0[1];
        return h + sum(t)
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "tail recursive with accumulator" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum_acc acc l = match l with | Nil -> acc | Cons (h, t) -> sum_acc (acc + h) t");
  [%expect{|
    local sum_acc;
    sum_acc = function(acc)
      return function(l)
        local l_match = l;
        local matched = l_match;
        if matched._tag == 1 then
          local t = l_match._0[2];
          local h = l_match._0[1];
          return sum_acc(acc + h)(t)
        elseif matched._tag == 0 then
          return acc
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "mutual recursion with pattern matching" =
  print_endline (compile "let rec even n = match n with | 0 -> true | n -> odd (n - 1)
and odd n = match n with | 0 -> false | n -> even (n - 1)");
  [%expect{|
    local even, odd;
    even = function(n)
      local n_match = n;
      if n_match == 0 then
        return true
      else
        local n_1 = n_match;
        return odd(n_1 - 1)
      end
    end;

    odd = function(n_2)
      local n_match_1 = n_2;
      if n_match_1 == 0 then
        return false
      else
        local n_3 = n_match_1;
        return even(n_3 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id(x)
      local x_match = x;
      local y = x_match;
      return y
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap(x)
      local x_match = x;
      local matched = x_match;
      if matched._tag == 1 then
        local y = x_match._0;
        return y
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 15-41:
    Warning: Non-exhaustive pattern matching, missing case: None
    |}]

let%expect_test "all wildcards match" =
  print_endline (compile "let first3 t = match t with | (x, _, _) -> x | _ -> 0");
  [%expect{|
    local function first3(t)
      local t_match = t;
      local x = t_match[1];
      return x
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match == 1 then
        return 200
      else
        if x_match == 0 then
          return 100
        else
          local n = x_match;
          return n
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f(x)
      local x_match = x;
      return 1
    end
    File "<string>", line 1, characters 34-40:
    Warning: Redundant pattern: this case will never be matched
    File "<string>", line 1, characters 43-49:
    Warning: Redundant pattern: this case will never be matched
    |}]

(* === Pattern Matching: Type Errors === *)

let%expect_test "type error: pattern type mismatch with scrutinee" =
  print_endline (compile "let f x = match x with | (a, b) -> a + b | 0 -> 0");
  [%expect{|
    ERROR: File "<string>", line 1, characters 43-49:
    Type error: Type mismatch: expected (int * int), got int
    Expected: (int * int)
    Actual: int
    |}]

let%expect_test "type error: constructor from wrong type" =
  print_endline (compile "type 'a option = None | Some of 'a
type 'a result = Ok of 'a | Err
let f x = match x with | Some n -> n | Err -> 0");
  [%expect{|
    ERROR: File "<string>", line 3, characters 39-47:
    Type error: Type mismatch: expected 't5 option, got 't6 result
    Expected: 't5 option
    Actual: 't6 result
    |}]

let%expect_test "type error: inconsistent tuple sizes" =
  print_endline (compile "let f x = match x with | (a, b) -> a | (a, b, c) -> a");
  [%expect{|
    ERROR: File "<string>", line 1, characters 39-53:
    Type error: Tuple size mismatch: expected 2 elements, got 3
    Expected: ('t2 * 't3)
    Actual: ('t4 * 't5 * 't6)
    |}]

let%expect_test "type error: wrong constructor argument type" =
  print_endline (compile "type intopt = None | Some of int
let f x = match x with | Some true -> 1 | _ -> 0");
  [%expect{|
    ERROR: File "<string>", line 2, characters 25-34:
    Type error: Type mismatch: expected int, got bool
    Expected: int
    Actual: bool
    |}]

(* === Pattern Matching: Record Patterns === *)

let%expect_test "record pattern with all fields" =
  print_endline (compile "type point = { x : int; y : int }
let get_x p = match p with | { x; y } -> x");
  [%expect{|
    local function get_x(p)
      local p_match = p;
      local y = p_match.y;
      local x = p_match.x;
      return x
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local function project(p)
      local p_match = p;
      local y = p_match.y;
      local x = p_match.x;
      return {x, y}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local x = opt_match._0.x;
        return x
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 16-60:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

let%expect_test "record pattern with renamed binding" =
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

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local apply_pair = function(f)
      return function(p)
        local p_match = p;
        local b = p_match[2];
        local a = p_match[1];
        return f(a)(b)
      end
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local function double_unwrap(x)
      local x_match = x;
      local inner = (function()
        local matched = x_match;
        if matched._tag == 0 then
          return _Ctor_option_0
        elseif matched._tag == 1 then
          local y = x_match._0;
          return y
        else
          return error("Match failure")
        end
      end)();
      local inner_match = inner;
      local matched = inner_match;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local z = inner_match._0;
        return z
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "pattern match inside if-then-else" =
  print_endline (compile "type 'a option = None | Some of 'a
let safe_div a b = if b == 0 then None else Some (match (a, b) with | (x, y) -> x / y)");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local safe_div = function(a)
      return function(b)
        if b == 0 then
          return _Ctor_option_0
        else
          return {_tag = 1, _0 = (function()
            local matched = {a, b};
            local y = matched[2];
            local x = matched[1];
            return x / y
          end)()}
        end
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id(x)
      return x
    end;

    local function f(opt)
      local matched = id(opt);
      local matched = matched;
      if matched._tag == 0 then
        return 0
      elseif matched._tag == 1 then
        local n = matched._0;
        return n
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "complex real-world example - tree traversal" =
  print_endline (compile "type tree = Leaf of int | Node of (tree * tree)
let rec sum t = match t with | Leaf n -> n | Node (l, r) -> sum l + sum r");
  [%expect{|
    local sum;
    sum = function(t)
      local t_match = t;
      local matched = t_match;
      if matched._tag == 1 then
        local r = t_match._0[2];
        local l = t_match._0[1];
        return sum(l) + sum(r)
      elseif matched._tag == 0 then
        local n = t_match._0;
        return n
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "complex real-world example - expression evaluator" =
  print_endline (compile "type expr = Num of int | Add of (expr * expr) | Mul of (expr * expr)
let rec eval e = match e with
  | Num n -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b");
  [%expect{|
    local eval;
    eval = function(e)
      local e_match = e;
      local matched = e_match;
      if matched._tag == 2 then
        local b = e_match._0[2];
        local a = e_match._0[1];
        return eval(a) * eval(b)
      elseif matched._tag == 1 then
        local b_1 = e_match._0[2];
        local a_1 = e_match._0[1];
        return eval(a_1) + eval(b_1)
      elseif matched._tag == 0 then
        local n = e_match._0;
        return n
      else
        return error("Match failure")
      end
    end
    |}]

(* ============================================================
   COMPREHENSIVE TESTS FOR LUA CODEGEN IMPROVEMENTS
   ============================================================ *)

(* --- Reserved Keyword Handling Tests --- *)

let%expect_test "lua keyword 'end' used as identifier is mangled" =
  print_endline (compile "let end = 42");
  [%expect{|
    ERROR: File "<string>", line 1, characters 4-7:
    Parse error: Syntax error
    |}]

let%expect_test "lua keyword 'nil' used as identifier is mangled" =
  print_endline (compile "let nil = 0");
  [%expect{| local _nil = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    ERROR: File "<string>", line 1, characters 10-12:
    Parse error: Syntax error
    |}]

let%expect_test "lua keyword 'local' used as identifier is mangled" =
  print_endline (compile "let local = 123");
  [%expect{| local _local = 123 |}]

let%expect_test "lua keyword 'goto' used as identifier is mangled" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto = 7 |}]

let%expect_test "lua keyword 'break' used as identifier is mangled" =
  print_endline (compile "let break = 3");
  [%expect{| local _break = 3 |}]

let%expect_test "lua keyword 'return' used as identifier is mangled" =
  print_endline (compile "let return = 1");
  [%expect{| local _return = 1 |}]

let%expect_test "lua keyword 'while' used as identifier is mangled" =
  print_endline (compile "let while = 8");
  [%expect{|
    ERROR: File "<string>", line 1, characters 4-9:
    Parse error: Syntax error
    |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none_val = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2};
    local _Ctor_color_1 = {_tag = 1};
    local _Ctor_color_0 = {_tag = 0};
    local red = _Ctor_color_0;
    local green = _Ctor_color_1;
    local blue = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 1 then
        return true
      elseif matched._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end
    |}]

(* --- Nullary Constructor Singletons Tests --- *)

let%expect_test "nullary constructor generates singleton preamble" =
  print_endline (compile "type 'a option = None | Some of 'a
let a = None
let b = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local a = _Ctor_option_0;
    local b = _Ctor_option_0
    |}]

let%expect_test "multiple nullary constructors each get singleton" =
  print_endline (compile "type traffic_light = Red | Yellow | Green
let stop = Red
let caution = Yellow
let go = Green");
  [%expect{|
    local _Ctor_traffic_light_2 = {_tag = 2};
    local _Ctor_traffic_light_1 = {_tag = 1};
    local _Ctor_traffic_light_0 = {_tag = 0};
    local stop = _Ctor_traffic_light_0;
    local caution = _Ctor_traffic_light_1;
    local go = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x = {_tag = 1, _0 = 1};
    local y = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none1 = _Ctor_option_0;
    local some1 = {_tag = 1, _0 = 10};
    local none2 = _Ctor_option_0;
    local some2 = {_tag = 1, _0 = 20}
    |}]

(* --- Switch Dispatch Table Tests --- *)

let%expect_test "four or more constructors uses dispatch table" =
  print_endline (compile "type direction = North | South | East | West
let to_num dir = match dir with
  | North -> 0
  | South -> 1
  | East -> 2
  | West -> 3");
  [%expect{|
    local function to_num(dir)
      local dir_match = dir;
      local matched = dir_match;
      local _dispatch = {[3] = function()
        return 3
      end, [2] = function()
        return 2
      end, [1] = function()
        return 1
      end, [0] = function()
        return 0
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "five constructors uses dispatch table" =
  print_endline (compile "type weekday = Mon | Tue | Wed | Thu | Fri
let day_num day = match day with
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5");
  [%expect{|
    local function day_num(day)
      local day_match = day;
      local matched = day_match;
      local _dispatch = {[4] = function()
        return 5
      end, [3] = function()
        return 4
      end, [2] = function()
        return 3
      end, [1] = function()
        return 2
      end, [0] = function()
        return 1
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "three constructors uses if-chain not dispatch" =
  print_endline (compile "type rgb = R | G | B
let to_num color = match color with
  | R -> 0
  | G -> 1
  | B -> 2");
  [%expect{|
    local function to_num(color)
      local color_match = color;
      local matched = color_match;
      if matched._tag == 2 then
        return 2
      elseif matched._tag == 1 then
        return 1
      elseif matched._tag == 0 then
        return 0
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "dispatch table with constructor arguments" =
  print_endline (compile "type expr = Num of int | Add of int | Sub of int | Mul of int
let eval expression = match expression with
  | Num n -> n
  | Add n -> n + 1
  | Sub n -> n - 1
  | Mul n -> n * 2");
  [%expect{|
    local function eval(expression)
      local expression_match = expression;
      local matched = expression_match;
      local _dispatch = {[3] = function()
        local n = expression_match._0;
        return n * 2
      end, [2] = function()
        local n_1 = expression_match._0;
        return n_1 - 1
      end, [1] = function()
        local n_2 = expression_match._0;
        return n_2 + 1
      end, [0] = function()
        local n_3 = expression_match._0;
        return n_3
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

(* --- IIFE Optimization Tests --- *)

let%expect_test "nested let in value position is floated out" =
  print_endline (compile "let result = let x = 1 in x + 2");
  [%expect{|
    local x = 1;
    local result = x + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a = 1;
    local b = 2;
    local result = a + b
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result;
    if true then
      result = 1
    else
      result = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x = 5;
    local result;
    if x > 0 then
      result = 1
    else
      result = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result;
    if true then
      if false then
        result = 1
      else
        result = 2
      end
    else
      result = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag;
    if true then
      flag = 1
    else
      flag = 0
    end;
    local result = flag + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local _ = print(1);
    local result = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local function compute(x)
      local y = x + 1;
      return y * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local function abs(n)
      if n < 0 then
        return 0 - n
      else
        return n
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to;
    sum_to = function(n)
      return function(acc)
        if n <= 0 then
          return acc
        else
          return sum_to(n - 1)(acc + n)
        end
      end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown;
    countdown = function(n)
      if n <= 0 then
        return 0
      else
        return countdown(n - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero;
    find_zero = function(n)
      if n == 0 then
        return true
      else
        return find_zero(n - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even, is_odd;
    is_even = function(n)
      if n == 0 then
        return true
      else
        return is_odd(n - 1)
      end
    end;

    is_odd = function(n_1)
      if n_1 == 0 then
        return false
      else
        return is_even(n_1 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial;
    factorial = function(n)
      if n <= 1 then
        return 1
      else
        return n * factorial(n - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc;
    factorial_acc = function(n)
      return function(acc)
        if n <= 1 then
          return acc
        else
          return factorial_acc(n - 1)(n * acc)
        end
      end
    end
    |}]

(* --- Combined Feature Tests --- *)

let%expect_test "reserved keyword in pattern match with dispatch" =
  print_endline (compile "type cmd = And | Or | Not | Xor
let eval_cmd and_val = match and_val with
  | And -> 1
  | Or -> 2
  | Not -> 3
  | Xor -> 4");
  [%expect{|
    local function eval_cmd(and_val)
      local and_val_match = and_val;
      local matched = and_val_match;
      local _dispatch = {[3] = function()
        return 4
      end, [2] = function()
        return 3
      end, [1] = function()
        return 2
      end, [0] = function()
        return 1
      end};
      local _handler = _dispatch[matched._tag];
      if _handler then
        return _handler()
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "singleton with IIFE optimization" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_or_default opt default_val =
  let result = if true then opt else None in
  match result with
  | None -> default_val
  | Some x -> x");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local get_or_default = function(opt)
      return function(default_val)
        local result;
        if true then
          result = opt
        else
          result = _Ctor_option_0
        end;
        local result_match = result;
        local matched = result_match;
        if matched._tag == 1 then
          local x = result_match._0;
          return x
        elseif matched._tag == 0 then
          return default_val
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "all features combined - complex example" =
  print_endline (compile "type 'a option = None | Some of 'a
type direction = North | South | East | West

let process_direction dir =
  let opt_value = if dir == 0 then None else Some dir in
  let result = match opt_value with
    | None -> 0
    | Some n -> n
  in
  result + 1");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local function process_direction(dir)
      local opt_value;
      if dir == 0 then
        opt_value = _Ctor_option_0
      else
        opt_value = {_tag = 1, _0 = dir}
      end;
      local opt_value_match = opt_value;
      local result = (function()
        local matched = opt_value_match;
        if matched._tag == 1 then
          local n = opt_value_match._0;
          return n
        elseif matched._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end)();
      return result + 1
    end
    |}]

(* Reference operations *)

let%expect_test "ref creates table with value field" =
  print_endline (compile "let r = ref 42");
  [%expect{| local r = {value = 42} |}]

let%expect_test "deref accesses value field" =
  print_endline (compile "let r = ref 42
let x = !r");
  [%expect{|
    local r = {value = 42};
    local x = r.value
    |}]

let%expect_test "assign modifies value field" =
  print_endline (compile "let r = ref 0
let _ = r := 1");
  [%expect{|
    local r = {value = 0};
    local top = (function()
      r.value = 1;
      return nil
    end)()
    |}]

let%expect_test "increment ref" =
  print_endline (compile "let r = ref 0
let _ = r := !r + 1");
  [%expect{|
    local r = {value = 0};
    local top = (function()
      r.value = r.value + 1;
      return nil
    end)()
    |}]

let%expect_test "ref in function" =
  print_endline (compile "let make_counter init =
  let c = ref init in
  let get = fun () -> !c in
  (get, c)");
  [%expect{|
    local function make_counter(init)
      local c = {value = init};
      local get = function(param)
        return c.value
      end;
      return {get, c}
    end
    |}]

let%expect_test "multiple refs" =
  print_endline (compile "let x = ref 10
let y = ref 20
let _ = x := !x + !y");
  [%expect{|
    local x = {value = 10};
    local y = {value = 20};
    local top = (function()
      x.value = x.value + y.value;
      return nil
    end)()
    |}]

let%expect_test "ref with string" =
  print_endline (compile {|let s = ref "hello"
let _ = s := "world"|});
  [%expect{|
    local s = {value = "hello"};
    local top = (function()
      s.value = "world";
      return nil
    end)()
    |}]

(* === Loop Construct Tests === *)

let%expect_test "while loop generates IIFE wrapper" =
  print_endline (compile {|
let counter = ref 0
let _ = while !counter < 3 do
  counter := !counter + 1
done
|});
  [%expect{|
    local counter = {value = 0};
    local top = (function()
      while counter.value < 3 do
        counter.value = counter.value + 1
      end;
      return nil
    end)()
    |}]

let%expect_test "while loop condition evaluates each iteration" =
  print_endline (compile {|
let count = ref 5
let sum = ref 0
let _ = while !count > 0 do
  sum := !sum + !count;
  count := !count - 1
done
|});
  [%expect{|
    local count = {value = 5};
    local sum = {value = 0};
    local top = (function()
      while count.value > 0 do
        sum.value = sum.value + count.value;
        count.value = count.value - 1
      end;
      return nil
    end)()
    |}]

let%expect_test "for loop upto generates numeric for" =
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

let%expect_test "for loop downto generates negative step" =
  print_endline (compile {|
let sum = ref 0
let _ = for i = 5 downto 1 do
  sum := !sum + i
done
|});
  [%expect{|
    local sum = {value = 0};
    local top = (function()
      for i = 5, 1, -1 do
        sum.value = sum.value + i
      end;
      return nil
    end)()
    |}]

let%expect_test "for loop variable scoping" =
  print_endline (compile {|
let _ = for i = 1 to 3 do
  print i
done
|});
  [%expect{|
    local top = (function()
      for i = 1, 3 do
        print(i)
      end;
      return nil
    end)()
    |}]

let%expect_test "nested for loops" =
  print_endline (compile {|
let sum = ref 0
let _ = for i = 1 to 2 do
  for j = 1 to 3 do
    sum := !sum + i * j
  done
done
|});
  [%expect{|
    local sum = {value = 0};
    local top = (function()
      for i = 1, 2 do
        (function()
          for j = 1, 3 do
            sum.value = sum.value + i * j
          end;
          return nil
        end)()
      end;
      return nil
    end)()
    |}]

(* === Polymorphic Variant Tests === *)

let%expect_test "poly variant nullary uses string tag" =
  print_endline (compile {|let x = `A|});
  [%expect{| local x = {_tag = "A"} |}]

let%expect_test "poly variant with argument" =
  print_endline (compile {|let x = `B 42|});
  [%expect{| local x = {_tag = "B", _0 = 42} |}]

let%expect_test "poly variant match uses string comparison" =
  print_endline (compile {|
let f x = match x with
  | `A -> 1
  | `B -> 2
  | _ -> 0
|});
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match._tag == "B" then
        return 2
      else
        if x_match._tag == "A" then
          return 1
        else
          return 0
        end
      end
    end
    |}]

let%expect_test "poly variant differs from regular variant" =
  print_endline (compile {|
type regular = A | B
let regular_a = A
let poly_a = `A
|});
  [%expect{|
    local _Ctor_regular_0 = {_tag = 0};
    local regular_a = _Ctor_regular_0;
    local poly_a = {_tag = "A"}
    |}]

let%expect_test "poly variant with extracted argument" =
  print_endline (compile {|
let f x = match x with
  | `Some n -> n
  | `None -> 0
|});
  [%expect{|
    local function f(x)
      local x_match = x;
      if x_match._tag == "None" then
        return 0
      else
        if x_match._tag == "Some" then
          local n = x_match._0;
          return n
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 10-14:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested poly variant" =
  print_endline (compile {|let x = `Outer (`Inner 5)|});
  [%expect{| local x = {_tag = "Outer", _0 = {_tag = "Inner", _0 = 5}} |}]

(* === Currying Edge Cases === *)

let%expect_test "unit function has unit param" =
  print_endline (compile {|let f = fun () -> 42|});
  [%expect{|
    local function f(param)
      return 42
    end
    |}]

let%expect_test "single param no extra nesting" =
  print_endline (compile {|let f = fun x -> x|});
  [%expect{|
    local function f(x)
      return x
    end
    |}]

let%expect_test "two params one nesting level" =
  print_endline (compile {|let f = fun x y -> x + y|});
  [%expect{|
    local f = function(x)
      return function(y)
        return x + y
      end
    end
    |}]

let%expect_test "many params deep nesting" =
  print_endline (compile {|let f = fun a b c d e -> a + b + c + d + e|});
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

let%expect_test "curried call chains calls" =
  print_endline (compile {|
let f a b c = a + b + c
let result = f 1 2 3
|});
  [%expect{|
    local f = function(a)
      return function(b)
        return function(c)
          return a + b + c
        end
      end
    end;
    local result = f(1)(2)(3)
    |}]

let%expect_test "partial application returns closure" =
  print_endline (compile {|
let add a b = a + b
let add5 = add 5
|});
  [%expect{|
    local add = function(a)
      return function(b)
        return a + b
      end
    end;
    local add5 = add(5)
    |}]

let%expect_test "higher order function with currying" =
  print_endline (compile {|
let apply f x = f x
let double n = n + n
let result = apply double 21
|});
  [%expect{|
    local apply = function(f)
      return function(x)
        return f(x)
      end
    end;

    local function double(n)
      return n + n
    end;
    local result = apply(double)(21)
    |}]
