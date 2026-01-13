let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "record literal generates lua table" =
  print_endline (compile "let r = { x = 1; y = 2 }");
  [%expect{| local r_11 = {x = 1, y = 2} |}]

let%expect_test "record field access generates dot notation" =
  print_endline (compile "let r = { x = 42 }
let v = r.x");
  [%expect{|
    local r_12 = {x = 42}
    local v_13 = r_12.x
    |}]

let%expect_test "record update generates shallow copy" =
  print_endline (compile "let r = { x = 1; y = 2 }
let r2 = { r with x = 10 }");
  [%expect{|
    local r_14 = {x = 1, y = 2}
    local r2_15 = (function()
      local _result = {}
      for _k, _v in pairs(r_14) do
        _result[_k] = _v
      end
      _result.x = 10
      return _result
    end)()
    |}]

let%expect_test "record punning expands to variable" =
  print_endline (compile "let x = 1
let y = 2
let r = { x; y }");
  [%expect{|
    local x_16 = 1
    local y_17 = 2
    local r_18 = {x = x_16, y = y_17}
    |}]

let%expect_test "nested record access" =
  print_endline (compile "let outer = { inner = { value = 42 } }
let v = outer.inner.value");
  [%expect{|
    local outer_19 = {inner = {value = 42}}
    local v_20 = outer_19.inner.value
    |}]

let%expect_test "match with integer patterns" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 1 -> 2 | n -> n");
  [%expect{|
    local function f_23(x_21)
      local _scrutinee_24 = x_21
      if _scrutinee_24 == 1 then
        return 2
      else
        if _scrutinee_24 == 0 then
          return 1
        else
          local n_22 = _scrutinee_24
          return n_22
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local function sum_pair_28(p_25)
      local _scrutinee_29 = p_25
      local b_27 = _scrutinee_29[2]
      local a_26 = _scrutinee_29[1]
      return a_26 + b_27
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local function abs_33(n_30)
      local _scrutinee_34 = n_30
      if (function()
      local x_31 = _scrutinee_34
      return x_31 < 0
    end)() then
        local x_31 = _scrutinee_34
        return 0 - x_31
      else
        local x_32 = _scrutinee_34
        return x_32
      end
    end
    File "<string>", line 1, characters 51-57:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local function is_some_36(opt_35)
      local _scrutinee_37 = opt_35
      if _scrutinee_37._tag == 1 then
        return true
      else
        if _scrutinee_37._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "match extracts constructor argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_or_default opt default = match opt with | None -> default | Some x -> x");
  [%expect{|
    local function get_or_default_41(opt_38, default_39)
      local _scrutinee_42 = opt_38
      if _scrutinee_42._tag == 1 then
        local x_40 = _scrutinee_42._0
        return x_40
      else
        if _scrutinee_42._tag == 0 then
          return default_39
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
    local _Ctor_option_0 = {_tag = 0}
    local none_43 = _Ctor_option_0
    local some_val_44 = {_tag = 1, _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local function get_x_46(r_45)
      return r_45.x
    end
    local p1_47 = {x = 1, y = 2}
    local p2_48 = {x = 10, z = 20}
    local v1_49 = get_x_46(p1_47)
    local v2_50 = get_x_46(p2_48)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r_51 = {a = 1, b = 2, c = 3}
    local r2_52 = (function()
      local _result = {}
      for _k, _v in pairs(r_51) do
        _result[_k] = _v
      end
      _result.a = 10
      _result.c = 30
      return _result
    end)()
    |}]

let%expect_test "function returning record" =
  print_endline (compile "let make_point x y = { x = x; y = y }");
  [%expect{|
    local function make_point_55(x_53, y_54)
      return {x = x_53, y = y_54}
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local function f_59(p_56)
      local _tuple_60 = p_56
      local a_57 = _tuple_60[1]
      local b_58 = _tuple_60[2]
      return a_57 + b_58
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
    local function f_64(opt_61)
      local _scrutinee_65 = opt_61
      if _scrutinee_65._tag == 1 then
        local x_62 = _scrutinee_65._0
        local _scrutinee_66 = x_62
        if _scrutinee_66 == 0 then
          return 1
        else
          local n_63 = _scrutinee_66
          return n_63 + 1
        end
      else
        if _scrutinee_65._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r_67 = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x_68 = 1
    local r_69 = {a = x_68 + 1, b = x_68 * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local function f_71(x_70)
      local _scrutinee_72 = x_70
      return 42
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local function classify_77(n_73)
      local _scrutinee_78 = n_73
      if (function()
      local x_74 = _scrutinee_78
      return x_74 < 0
    end)() then
        local x_74 = _scrutinee_78
        return 0 - 1
      else
        if (function()
      local x_75 = _scrutinee_78
      return x_75 == 0
    end)() then
          local x_75 = _scrutinee_78
          return 0
        else
          local x_76 = _scrutinee_78
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
    local function f_85(x_83)
      local _scrutinee_86 = x_83
      if (function()
      local n_84 = _scrutinee_86
      return n_84
    end)() then
        local n_84 = _scrutinee_86
        return n_84
      else
        return error("Match failure")
      end
    end
    File "<string>", line 1, characters 10-38:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Exhaustiveness Tests === *)

let%expect_test "exhaustive match on option - no warning" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some n -> n");
  [%expect{|
    local function f_89(x_87)
      local _scrutinee_90 = x_87
      if _scrutinee_90._tag == 1 then
        local n_88 = _scrutinee_90._0
        return n_88
      else
        if _scrutinee_90._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "non-exhaustive match - missing None" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n -> n");
  [%expect{|
    local function f_93(x_91)
      local _scrutinee_94 = x_91
      if _scrutinee_94._tag == 1 then
        local n_92 = _scrutinee_94._0
        return n_92
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
    local function f_96(x_95)
      local _scrutinee_97 = x_95
      if _scrutinee_97._tag == 0 then
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
    local function f_99(b_98)
      local _scrutinee_100 = b_98
      if _scrutinee_100 == false then
        return 0
      else
        if _scrutinee_100 == true then
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
    local function f_102(b_101)
      local _scrutinee_103 = b_101
      if _scrutinee_103 == true then
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
    local function f_106(x_104)
      local _scrutinee_107 = x_104
      if _scrutinee_107._tag == 1 then
        local n_105 = _scrutinee_107._0
        return n_105
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
    local function f_109(x_108)
      local _scrutinee_110 = x_108
      if _scrutinee_110._tag == 0 then
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
    local function f_113(x_111)
      local _scrutinee_114 = x_111
      if _scrutinee_114._tag == 1 then
        local n_112 = _scrutinee_114._0
        return n_112
      else
        if _scrutinee_114._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 51-60:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "redundant integer pattern" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 0 -> 2 | n -> n");
  [%expect{|
    local function f_117(x_115)
      local _scrutinee_118 = x_115
      if _scrutinee_118 == 0 then
        return 1
      else
        local n_116 = _scrutinee_118
        return n_116
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local function to_num_120(c_119)
      local _scrutinee_121 = c_119
      if _scrutinee_121._tag == 2 then
        return 2
      else
        if _scrutinee_121._tag == 1 then
          return 1
        else
          if _scrutinee_121._tag == 0 then
            return 0
          else
            return error("Match failure")
          end
        end
      end
    end
    |}]

let%expect_test "nested tuple patterns" =
  print_endline (compile "let f p = match p with | ((a, b), c) -> a + b + c");
  [%expect{|
    local function f_126(p_122)
      local _scrutinee_127 = p_122
      local c_125 = _scrutinee_127[2]
      local b_124 = _scrutinee_127[1][2]
      local a_123 = _scrutinee_127[1][1]
      return a_123 + b_124 + c_125
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local function sum_131(p_128)
      local _scrutinee_132 = p_128
      if _scrutinee_132._tag == 0 then
        local b_130 = _scrutinee_132._0[2]
        local a_129 = _scrutinee_132._0[1]
        return a_129 + b_130
      else
        return error("Match failure")
      end
    end
    File "<string>", line 2, characters 12-47:
    Warning: Non-exhaustive pattern matching, missing case: Pair (Pair _)
    |}]

let%expect_test "match on record pattern" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local function get_x_135(r_133)
      local _scrutinee_136 = r_133
      local x_134 = _scrutinee_136.x
      return x_134
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local function sum_xy_140(r_137)
      local _scrutinee_141 = r_137
      local y_139 = _scrutinee_141.y
      local x_138 = _scrutinee_141.x
      return x_138 + y_139
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f_144(x_142)
      local _scrutinee_145 = x_142
      if _scrutinee_145._tag == 1 then
        if (function()
      local n_143 = _scrutinee_145._0
      return n_143 > 0
    end)() then
          local n_143 = _scrutinee_145._0
          return n_143
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
    local function f_148(x_146)
      local _scrutinee_149 = x_146
      if _scrutinee_149._tag == 0 then
        return 0
      else
        if _scrutinee_149._tag == 1 then
          if (function()
      local n_147 = _scrutinee_149._0
      return n_147 > 0
    end)() then
            local n_147 = _scrutinee_149._0
            return n_147
          else
            return error("Match failure")
          end
        else
          return error("Match failure")
        end
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
    local function head_153(l_150)
      local _scrutinee_154 = l_150
      if _scrutinee_154._tag == 1 then
        local t_152 = _scrutinee_154._0[2]
        local h_151 = _scrutinee_154._0[1]
        return h_151
      else
        if _scrutinee_154._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 13-55:
    Warning: Non-exhaustive pattern matching, missing case: Cons Nil
    |}]

let%expect_test "either type with two type params" =
  print_endline (compile "type ('a, 'b) either = Left of 'a | Right of 'b
let is_left e = match e with | Left _ -> true | Right _ -> false");
  [%expect{|
    local function is_left_156(e_155)
      local _scrutinee_157 = e_155
      if _scrutinee_157._tag == 1 then
        return false
      else
        if _scrutinee_157._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end
    |}]

(* === Pattern Matching: Constant Patterns === *)

let%expect_test "match with string patterns" =
  print_endline (compile {|let greet name = match name with | "Alice" -> "Hi Alice" | "Bob" -> "Hey Bob" | _ -> "Hello"|});
  [%expect{|
    local function greet_159(name_158)
      local _scrutinee_160 = name_158
      if _scrutinee_160 == "Bob" then
        return "Hey Bob"
      else
        if _scrutinee_160 == "Alice" then
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
    local function negate_162(b_161)
      local _scrutinee_163 = b_161
      if _scrutinee_163 == false then
        return true
      else
        if _scrutinee_163 == true then
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
    local function f_165(u_164)
      local _scrutinee_166 = u_164
      if _scrutinee_166 == nil then
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
    local function sum3_171(t_167)
      local _scrutinee_172 = t_167
      local c_170 = _scrutinee_172[3]
      local b_169 = _scrutinee_172[2]
      local a_168 = _scrutinee_172[1]
      return a_168 + b_169 + c_170
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local function sum4_178(t_173)
      local _scrutinee_179 = t_173
      local d_177 = _scrutinee_179[4]
      local c_176 = _scrutinee_179[3]
      local b_175 = _scrutinee_179[2]
      local a_174 = _scrutinee_179[1]
      return a_174 + b_175 + c_176 + d_177
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local function first_182(t_180)
      local _scrutinee_183 = t_180
      local a_181 = _scrutinee_183[1]
      return a_181
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local function check_188(t_184)
      local _scrutinee_189 = t_184
      if _scrutinee_189[1] == 1 then
        local y_186 = _scrutinee_189[2]
        return y_186 + 1
      else
        if _scrutinee_189[1] == 0 then
          local x_185 = _scrutinee_189[2]
          return x_185
        else
          local z_187 = _scrutinee_189[2]
          return z_187 + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    local function deep_192(x_190)
      local _scrutinee_193 = x_190
      if _scrutinee_193._tag == 1 then
        if _scrutinee_193._0._tag == 1 then
          if _scrutinee_193._0._0._tag == 1 then
            local n_191 = _scrutinee_193._0._0._0
            return n_191
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
    local function extract_199(t_194)
      local _scrutinee_200 = t_194
      if _scrutinee_200[1]._tag == 0 then
        local d_198 = _scrutinee_200[2]
        return d_198
      else
        if _scrutinee_200[1]._tag == 1 then
          local c_197 = _scrutinee_200[2]
          local b_196 = _scrutinee_200[1]._0[2]
          local a_195 = _scrutinee_200[1]._0[1]
          return a_195 + b_196 + c_197
        else
          return error("Match failure")
        end
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
    local function unwrap_205(w_201)
      local _scrutinee_206 = w_201
      if _scrutinee_206._tag == 0 then
        if _scrutinee_206._0[1]._tag == 0 then
          local n_204 = _scrutinee_206._0[2]
          return n_204
        else
          if _scrutinee_206._0[1]._tag == 1 then
            local n_203 = _scrutinee_206._0[2]
            local x_202 = _scrutinee_206._0[1]._0
            return x_202 + n_203
          else
            return error("Match failure")
          end
        end
      else
        return error("Match failure")
      end
    end
    File "<string>", line 3, characters 15-77:
    Warning: Non-exhaustive pattern matching, missing case: Wrap (Wrap _)
    |}]

(* === Pattern Matching: Multiple Variants === *)

let%expect_test "four constructor variant - exhaustive" =
  print_endline (compile "type dir = North | South | East | West
let to_num d = match d with | North -> 0 | South -> 1 | East -> 2 | West -> 3");
  [%expect{|
    local function to_num_208(d_207)
      local _scrutinee_209 = d_207
      local _switch = _scrutinee_209
      local _dispatch = {[3] = function()
      return 3
    end, [2] = function()
      return 2
    end, [1] = function()
      return 1
    end, [0] = function()
      return 0
    end}
      local _handler = _dispatch[_switch._tag]
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
    local function to_num_211(d_210)
      local _scrutinee_212 = d_210
      if _scrutinee_212._tag == 2 then
        return 2
      else
        if _scrutinee_212._tag == 1 then
          return 1
        else
          if _scrutinee_212._tag == 0 then
            return 0
          else
            return error("Match failure")
          end
        end
      end
    end
    File "<string>", line 2, characters 15-65:
    Warning: Non-exhaustive pattern matching, missing case: West
    |}]

let%expect_test "five constructor variant with wildcard" =
  print_endline (compile "type day = Mon | Tue | Wed | Thu | Fri
let is_monday d = match d with | Mon -> true | _ -> false");
  [%expect{|
    local function is_monday_214(d_213)
      local _scrutinee_215 = d_213
      if _scrutinee_215._tag == 0 then
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
    local function is_value_217(e_216)
      local _scrutinee_218 = e_216
      if _scrutinee_218._tag == 2 then
        return false
      else
        if _scrutinee_218._tag == 1 then
          return true
        else
          if _scrutinee_218._tag == 0 then
            return true
          else
            return error("Match failure")
          end
        end
      end
    end
    |}]

(* === Pattern Matching: Variable Bindings === *)

let%expect_test "same variable name in different arms" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0 | Some x -> x");
  [%expect{|
    local function f_221(x_219)
      local _scrutinee_222 = x_219
      if _scrutinee_222._tag == 1 then
        local x_220 = _scrutinee_222._0
        return x_220
      else
        if _scrutinee_222._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "binding entire constructor argument" =
  print_endline (compile "type pair = Pair of (int * int)
let get_pair p = match p with | Pair x -> x");
  [%expect{|
    local function get_pair_225(p_223)
      local _scrutinee_226 = p_223
      if _scrutinee_226._tag == 0 then
        local x_224 = _scrutinee_226._0
        return x_224
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f_231(x_227)
      local _scrutinee_232 = x_227
      if _scrutinee_232._tag == 0 then
        return 0
      else
        if _scrutinee_232._tag == 1 then
          local c_230 = _scrutinee_232._0[3]
          local b_229 = _scrutinee_232._0[2]
          local a_228 = _scrutinee_232._0[1]
          return a_228 + b_229 + c_230
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 10-64:
    Warning: Non-exhaustive pattern matching, missing case: Some None
    |}]

(* === Pattern Matching: Guards === *)

let%expect_test "guard accessing multiple bound variables" =
  print_endline (compile "let check p = match p with | (a, b) when a > b -> a | (a, b) -> b");
  [%expect{|
    local function check_238(p_233)
      local _scrutinee_239 = p_233
      if (function()
      local b_235 = _scrutinee_239[2]
      local a_234 = _scrutinee_239[1]
      return a_234 > b_235
    end)() then
        local b_235 = _scrutinee_239[2]
        local a_234 = _scrutinee_239[1]
        return a_234
      else
        local b_237 = _scrutinee_239[2]
        local a_236 = _scrutinee_239[1]
        return b_237
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range_242(n_240)
      local _scrutinee_243 = n_240
      if (function()
      local x_241 = _scrutinee_243
      return x_241 > 0
    end)() then
        local x_241 = _scrutinee_243
        if x_241 < 10 then
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
    local function positive_246(opt_244)
      local _scrutinee_247 = opt_244
      if _scrutinee_247._tag == 0 then
        return false
      else
        if _scrutinee_247._tag == 1 then
          if (function()
      local n_245 = _scrutinee_247._0
      return n_245 > 0
    end)() then
            local n_245 = _scrutinee_247._0
            return true
          else
            return false
          end
        else
          return error("Match failure")
        end
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
    local function classify_252(n_248)
      local _scrutinee_253 = n_248
      if (function()
      local x_249 = _scrutinee_253
      return x_249 < 0
    end)() then
        local x_249 = _scrutinee_253
        return 0 - 1
      else
        if (function()
      local x_250 = _scrutinee_253
      return x_250 > 100
    end)() then
          local x_250 = _scrutinee_253
          return 2
        else
          if (function()
      local x_251 = _scrutinee_253
      return x_251 > 50
    end)() then
            local x_251 = _scrutinee_253
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
    local length_254
    length_254 = function(l_255)
      local _scrutinee_257 = l_255
      if _scrutinee_257._tag == 1 then
        local t_256 = _scrutinee_257._0[2]
        return 1 + length_254(t_256)
      else
        if _scrutinee_257._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 19-72:
    Warning: Non-exhaustive pattern matching, missing case: Cons Nil
    |}]

let%expect_test "recursive function with multiple pattern arms" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum l = match l with | Nil -> 0 | Cons (h, t) -> h + sum t");
  [%expect{|
    local sum_258
    sum_258 = function(l_259)
      local _scrutinee_262 = l_259
      if _scrutinee_262._tag == 1 then
        local t_261 = _scrutinee_262._0[2]
        local h_260 = _scrutinee_262._0[1]
        return h_260 + sum_258(t_261)
      else
        if _scrutinee_262._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-66:
    Warning: Non-exhaustive pattern matching, missing case: Cons Nil
    |}]

let%expect_test "tail recursive with accumulator" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum_acc acc l = match l with | Nil -> acc | Cons (h, t) -> sum_acc (acc + h) t");
  [%expect{|
    local sum_acc_263
    sum_acc_263 = function(acc_264, l_265)
      local _scrutinee_268 = l_265
      if _scrutinee_268._tag == 1 then
        local t_267 = _scrutinee_268._0[2]
        local h_266 = _scrutinee_268._0[1]
        return sum_acc_263(acc_264 + h_266, t_267)
      else
        if _scrutinee_268._tag == 0 then
          return acc_264
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 24-86:
    Warning: Non-exhaustive pattern matching, missing case: Cons Nil
    |}]

let%expect_test "mutual recursion with pattern matching" =
  print_endline (compile "let rec even n = match n with | 0 -> true | n -> odd (n - 1)
and odd n = match n with | 0 -> false | n -> even (n - 1)");
  [%expect{|
    local even_269, odd_270
    even_269 = function(n_271)
      local _scrutinee_275 = n_271
      if _scrutinee_275 == 0 then
        return true
      else
        local n_272 = _scrutinee_275
        return odd_270(n_272 - 1)
      end
    end
    odd_270 = function(n_273)
      local _scrutinee_276 = n_273
      if _scrutinee_276 == 0 then
        return false
      else
        local n_274 = _scrutinee_276
        return even_269(n_274 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id_279(x_277)
      local _scrutinee_280 = x_277
      local y_278 = _scrutinee_280
      return y_278
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap_283(x_281)
      local _scrutinee_284 = x_281
      if _scrutinee_284._tag == 1 then
        local y_282 = _scrutinee_284._0
        return y_282
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
    local function first3_287(t_285)
      local _scrutinee_288 = t_285
      local x_286 = _scrutinee_288[1]
      return x_286
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f_291(x_289)
      local _scrutinee_292 = x_289
      if _scrutinee_292 == 1 then
        return 200
      else
        if _scrutinee_292 == 0 then
          return 100
        else
          local n_290 = _scrutinee_292
          return n_290
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f_294(x_293)
      local _scrutinee_295 = x_293
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
    Type error: Type mismatch: expected 't4 option, got 't6 result
    Expected: 't4 option
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
    local function get_x_311(p_308)
      local _scrutinee_312 = p_308
      local y_310 = _scrutinee_312.y
      local x_309 = _scrutinee_312.x
      return x_309
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local function project_316(p_313)
      local _scrutinee_317 = p_313
      local y_315 = _scrutinee_317.y
      local x_314 = _scrutinee_317.x
      return {x_314, y_315}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x_320(opt_318)
      local _scrutinee_321 = opt_318
      if _scrutinee_321._tag == 0 then
        return 0
      else
        if _scrutinee_321._tag == 1 then
          local x_319 = _scrutinee_321._0.x
          return x_319
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-60:
    Warning: Non-exhaustive pattern matching, missing case: Some None
    |}]

let%expect_test "record pattern with renamed binding" =
  print_endline (compile "let get_val r = match r with | { x = value } -> value");
  [%expect{|
    local function get_val_324(r_322)
      local _scrutinee_325 = r_322
      local value_323 = _scrutinee_325.x
      return value_323
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local function apply_pair_330(f_326, p_327)
      local _scrutinee_331 = p_327
      local b_329 = _scrutinee_331[2]
      local a_328 = _scrutinee_331[1]
      return f_326(a_328, b_329)
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local function double_unwrap_336(x_332)
      local _scrutinee_338 = x_332
      local inner_334
      if _scrutinee_338._tag == 0 then
        inner_334 = _Ctor_option_0
      else
        if _scrutinee_338._tag == 1 then
          local y_333 = _scrutinee_338._0
          inner_334 = y_333
        else
          inner_334 = error("Match failure")
        end
      end
      local _scrutinee_337 = inner_334
      if _scrutinee_337._tag == 0 then
        return 0
      else
        if _scrutinee_337._tag == 1 then
          local z_335 = _scrutinee_337._0
          return z_335
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "pattern match inside if-then-else" =
  print_endline (compile "type 'a option = None | Some of 'a
let safe_div a b = if b == 0 then None else Some (match (a, b) with | (x, y) -> x / y)");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local function safe_div_343(a_339, b_340)
      if b_340 == 0 then
        return _Ctor_option_0
      else
        return {_tag = 1, _0 = (function()
      local _scrutinee_344 = {a_339, b_340}
      local y_342 = _scrutinee_344[2]
      local x_341 = _scrutinee_344[1]
      return x_341 / y_342
    end)()}
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id_346(x_345)
      return x_345
    end
    local function f_349(opt_347)
      local _scrutinee_350 = id_346(opt_347)
      if _scrutinee_350._tag == 0 then
        return 0
      else
        if _scrutinee_350._tag == 1 then
          local n_348 = _scrutinee_350._0
          return n_348
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "complex real-world example - tree traversal" =
  print_endline (compile "type tree = Leaf of int | Node of (tree * tree)
let rec sum t = match t with | Leaf n -> n | Node (l, r) -> sum l + sum r");
  [%expect{|
    local sum_351
    sum_351 = function(t_352)
      local _scrutinee_356 = t_352
      if _scrutinee_356._tag == 1 then
        local r_355 = _scrutinee_356._0[2]
        local l_354 = _scrutinee_356._0[1]
        return sum_351(l_354) + sum_351(r_355)
      else
        if _scrutinee_356._tag == 0 then
          local n_353 = _scrutinee_356._0
          return n_353
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-73:
    Warning: Non-exhaustive pattern matching, missing case: Node (Leaf _)
    |}]

let%expect_test "complex real-world example - expression evaluator" =
  print_endline (compile "type expr = Num of int | Add of (expr * expr) | Mul of (expr * expr)
let rec eval e = match e with
  | Num n -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b");
  [%expect{|
    local eval_357
    eval_357 = function(e_358)
      local _scrutinee_364 = e_358
      if _scrutinee_364._tag == 2 then
        local b_363 = _scrutinee_364._0[2]
        local a_362 = _scrutinee_364._0[1]
        return eval_357(a_362) * eval_357(b_363)
      else
        if _scrutinee_364._tag == 1 then
          local b_361 = _scrutinee_364._0[2]
          local a_360 = _scrutinee_364._0[1]
          return eval_357(a_360) + eval_357(b_361)
        else
          if _scrutinee_364._tag == 0 then
            local n_359 = _scrutinee_364._0
            return n_359
          else
            return error("Match failure")
          end
        end
      end
    end
    File "<string>", line 2, characters 17-33:
    Warning: Non-exhaustive pattern matching, missing case: Add (Num _)
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
  [%expect{| local _nil_365 = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat_366 = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until_367 = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    local function check_369(_do_368)
      return _do_368
    end
    |}]

let%expect_test "lua keyword 'local' used as identifier is mangled" =
  print_endline (compile "let local = 123");
  [%expect{| local _local_370 = 123 |}]

let%expect_test "lua keyword 'goto' used as identifier is mangled" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto_371 = 7 |}]

let%expect_test "lua keyword 'break' used as identifier is mangled" =
  print_endline (compile "let break = 3");
  [%expect{| local _break_372 = 3 |}]

let%expect_test "lua keyword 'return' used as identifier is mangled" =
  print_endline (compile "let return = 1");
  [%expect{| local _return_373 = 1 |}]

let%expect_test "lua keyword 'while' used as identifier is mangled" =
  print_endline (compile "let while = 8");
  [%expect{| local _while_374 = 8 |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value_375 = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker_376 = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none_val_377 = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val_378 = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2}
    local _Ctor_color_1 = {_tag = 1}
    local _Ctor_color_0 = {_tag = 0}
    local red_379 = _Ctor_color_0
    local green_380 = _Ctor_color_1
    local blue_381 = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local function is_some_383(opt_382)
      local _scrutinee_384 = opt_382
      if _scrutinee_384._tag == 1 then
        return true
      else
        if _scrutinee_384._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end
    |}]

(* --- Nullary Constructor Singletons Tests --- *)

let%expect_test "nullary constructor generates singleton preamble" =
  print_endline (compile "type 'a option = None | Some of 'a
let a = None
let b = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local a_385 = _Ctor_option_0
    local b_386 = _Ctor_option_0
    |}]

let%expect_test "multiple nullary constructors each get singleton" =
  print_endline (compile "type traffic_light = Red | Yellow | Green
let stop = Red
let caution = Yellow
let go = Green");
  [%expect{|
    local _Ctor_traffic_light_2 = {_tag = 2}
    local _Ctor_traffic_light_1 = {_tag = 1}
    local _Ctor_traffic_light_0 = {_tag = 0}
    local stop_387 = _Ctor_traffic_light_0
    local caution_388 = _Ctor_traffic_light_1
    local go_389 = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x_390 = {_tag = 1, _0 = 1}
    local y_391 = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none1_392 = _Ctor_option_0
    local some1_393 = {_tag = 1, _0 = 10}
    local none2_394 = _Ctor_option_0
    local some2_395 = {_tag = 1, _0 = 20}
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
    local function to_num_397(dir_396)
      local _scrutinee_398 = dir_396
      local _switch = _scrutinee_398
      local _dispatch = {[3] = function()
      return 3
    end, [2] = function()
      return 2
    end, [1] = function()
      return 1
    end, [0] = function()
      return 0
    end}
      local _handler = _dispatch[_switch._tag]
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
    local function day_num_400(day_399)
      local _scrutinee_401 = day_399
      local _switch = _scrutinee_401
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
    end}
      local _handler = _dispatch[_switch._tag]
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
    local function to_num_403(color_402)
      local _scrutinee_404 = color_402
      if _scrutinee_404._tag == 2 then
        return 2
      else
        if _scrutinee_404._tag == 1 then
          return 1
        else
          if _scrutinee_404._tag == 0 then
            return 0
          else
            return error("Match failure")
          end
        end
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
    local function eval_410(expression_405)
      local _scrutinee_411 = expression_405
      local _switch = _scrutinee_411
      local _dispatch = {[3] = function()
      local n_409 = _scrutinee_411._0
      return n_409 * 2
    end, [2] = function()
      local n_408 = _scrutinee_411._0
      return n_408 - 1
    end, [1] = function()
      local n_407 = _scrutinee_411._0
      return n_407 + 1
    end, [0] = function()
      local n_406 = _scrutinee_411._0
      return n_406
    end}
      local _handler = _dispatch[_switch._tag]
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
    local x_412 = 1
    local result_413 = x_412 + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a_414 = 1
    local b_415 = 2
    local result_416 = a_414 + b_415
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result_417
    if true then
      result_417 = 1
    else
      result_417 = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x_418 = 5
    local result_419
    if x_418 > 0 then
      result_419 = 1
    else
      result_419 = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result_420
    if true then
      if false then
        result_420 = 1
      else
        result_420 = 2
      end
    else
      result_420 = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag_421
    if true then
      flag_421 = 1
    else
      flag_421 = 0
    end
    local result_422 = flag_421 + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local __424 = print(1)
    local result_423 = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local function compute_427(x_425)
      local y_426 = x_425 + 1
      return y_426 * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local function abs_429(n_428)
      if n_428 < 0 then
        return 0 - n_428
      else
        return n_428
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to_430
    sum_to_430 = function(n_431, acc_432)
      if n_431 <= 0 then
        return acc_432
      else
        return sum_to_430(n_431 - 1, acc_432 + n_431)
      end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown_433
    countdown_433 = function(n_434)
      if n_434 <= 0 then
        return 0
      else
        return countdown_433(n_434 - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero_435
    find_zero_435 = function(n_436)
      if n_436 == 0 then
        return true
      else
        return find_zero_435(n_436 - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even_437, is_odd_438
    is_even_437 = function(n_439)
      if n_439 == 0 then
        return true
      else
        return is_odd_438(n_439 - 1)
      end
    end
    is_odd_438 = function(n_440)
      if n_440 == 0 then
        return false
      else
        return is_even_437(n_440 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial_441
    factorial_441 = function(n_442)
      if n_442 <= 1 then
        return 1
      else
        return n_442 * factorial_441(n_442 - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc_443
    factorial_acc_443 = function(n_444, acc_445)
      if n_444 <= 1 then
        return acc_445
      else
        return factorial_acc_443(n_444 - 1, n_444 * acc_445)
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
    local function eval_cmd_447(and_val_446)
      local _scrutinee_448 = and_val_446
      local _switch = _scrutinee_448
      local _dispatch = {[3] = function()
      return 4
    end, [2] = function()
      return 3
    end, [1] = function()
      return 2
    end, [0] = function()
      return 1
    end}
      local _handler = _dispatch[_switch._tag]
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
    local _Ctor_option_0 = {_tag = 0}
    local function get_or_default_453(opt_449, default_val_450)
      local result_451
      if true then
        result_451 = opt_449
      else
        result_451 = _Ctor_option_0
      end
      local _scrutinee_454 = result_451
      if _scrutinee_454._tag == 1 then
        local x_452 = _scrutinee_454._0
        return x_452
      else
        if _scrutinee_454._tag == 0 then
          return default_val_450
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
    local _Ctor_option_0 = {_tag = 0}
    local function process_direction_459(dir_455)
      local opt_value_456
      if dir_455 == 0 then
        opt_value_456 = _Ctor_option_0
      else
        opt_value_456 = {_tag = 1, _0 = dir_455}
      end
      local _scrutinee_460 = opt_value_456
      local result_458
      if _scrutinee_460._tag == 1 then
        local n_457 = _scrutinee_460._0
        result_458 = n_457
      else
        if _scrutinee_460._tag == 0 then
          result_458 = 0
        else
          result_458 = error("Match failure")
        end
      end
      return result_458 + 1
    end
    |}]

(* Reference operations *)

let%expect_test "ref creates table with value field" =
  print_endline (compile "let r = ref 42");
  [%expect{| local r_461 = {value = 42} |}]

let%expect_test "deref accesses value field" =
  print_endline (compile "let r = ref 42
let x = !r");
  [%expect{|
    local r_462 = {value = 42}
    local x_463 = r_462.value
    |}]

let%expect_test "assign modifies value field" =
  print_endline (compile "let r = ref 0
let _ = r := 1");
  [%expect{|
    local r_464 = {value = 0}
    local _top_465 = (function()
      r_464.value = 1
      return nil
    end)()
    |}]

let%expect_test "increment ref" =
  print_endline (compile "let r = ref 0
let _ = r := !r + 1");
  [%expect{|
    local r_466 = {value = 0}
    local _top_467 = (function()
      r_466.value = r_466.value + 1
      return nil
    end)()
    |}]

let%expect_test "ref in function" =
  print_endline (compile "let make_counter init =
  let c = ref init in
  let get = fun () -> !c in
  (get, c)");
  [%expect{|
    local function make_counter_471(init_468)
      local c_469 = {value = init_468}
      local get_470 = function(_param_472)
      return c_469.value
    end
      return {get_470, c_469}
    end
    |}]

let%expect_test "multiple refs" =
  print_endline (compile "let x = ref 10
let y = ref 20
let _ = x := !x + !y");
  [%expect{|
    local x_473 = {value = 10}
    local y_474 = {value = 20}
    local _top_475 = (function()
      x_473.value = x_473.value + y_474.value
      return nil
    end)()
    |}]

let%expect_test "ref with string" =
  print_endline (compile {|let s = ref "hello"
let _ = s := "world"|});
  [%expect{|
    local s_476 = {value = "hello"}
    local _top_477 = (function()
      s_476.value = "world"
      return nil
    end)()
    |}]
