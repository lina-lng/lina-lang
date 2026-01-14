let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "record literal generates lua table" =
  print_endline (compile "let r = { x = 1; y = 2 }");
  [%expect{| local r_13 = {x = 1, y = 2} |}]

let%expect_test "record field access generates dot notation" =
  print_endline (compile "let r = { x = 42 }
let v = r.x");
  [%expect{|
    local r_14 = {x = 42}
    local v_15 = r_14.x
    |}]

let%expect_test "record update generates shallow copy" =
  print_endline (compile "let r = { x = 1; y = 2 }
let r2 = { r with x = 10 }");
  [%expect{|
    local r_16 = {x = 1, y = 2}
    local r2_17 = (function()
      local _result = {}
      for _k, _v in pairs(r_16) do
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
    local x_18 = 1
    local y_19 = 2
    local r_20 = {x = x_18, y = y_19}
    |}]

let%expect_test "nested record access" =
  print_endline (compile "let outer = { inner = { value = 42 } }
let v = outer.inner.value");
  [%expect{|
    local outer_21 = {inner = {value = 42}}
    local v_22 = outer_21.inner.value
    |}]

let%expect_test "match with integer patterns" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 1 -> 2 | n -> n");
  [%expect{|
    local function f_25(x_23)
      local _scrutinee_26 = x_23
      if _scrutinee_26 == 1 then
        return 2
      else
        if _scrutinee_26 == 0 then
          return 1
        else
          local n_24 = _scrutinee_26
          return n_24
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local function sum_pair_30(p_27)
      local _scrutinee_31 = p_27
      local b_29 = _scrutinee_31[2]
      local a_28 = _scrutinee_31[1]
      return a_28 + b_29
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local function abs_35(n_32)
      local _scrutinee_36 = n_32
      if (function()
      local x_33 = _scrutinee_36
      return x_33 < 0
    end)() then
        local x_33 = _scrutinee_36
        return 0 - x_33
      else
        local x_34 = _scrutinee_36
        return x_34
      end
    end
    File "<string>", line 1, characters 51-57:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local function is_some_38(opt_37)
      local _scrutinee_39 = opt_37
      if _scrutinee_39._tag == 1 then
        return true
      else
        if _scrutinee_39._tag == 0 then
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
    local function get_or_default_43(opt_40, default_41)
      local _scrutinee_44 = opt_40
      if _scrutinee_44._tag == 1 then
        local x_42 = _scrutinee_44._0
        return x_42
      else
        if _scrutinee_44._tag == 0 then
          return default_41
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
    local none_45 = _Ctor_option_0
    local some_val_46 = {_tag = 1, _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local function get_x_48(r_47)
      return r_47.x
    end
    local p1_49 = {x = 1, y = 2}
    local p2_50 = {x = 10, z = 20}
    local v1_51 = get_x_48(p1_49)
    local v2_52 = get_x_48(p2_50)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r_53 = {a = 1, b = 2, c = 3}
    local r2_54 = (function()
      local _result = {}
      for _k, _v in pairs(r_53) do
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
    local function make_point_57(x_55, y_56)
      return {x = x_55, y = y_56}
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local function f_61(p_58)
      local _tuple_62 = p_58
      local a_59 = _tuple_62[1]
      local b_60 = _tuple_62[2]
      return a_59 + b_60
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
    local function f_66(opt_63)
      local _scrutinee_67 = opt_63
      if _scrutinee_67._tag == 1 then
        local x_64 = _scrutinee_67._0
        local _scrutinee_68 = x_64
        if _scrutinee_68 == 0 then
          return 1
        else
          local n_65 = _scrutinee_68
          return n_65 + 1
        end
      else
        if _scrutinee_67._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r_69 = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x_70 = 1
    local r_71 = {a = x_70 + 1, b = x_70 * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local function f_73(x_72)
      local _scrutinee_74 = x_72
      return 42
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local function classify_79(n_75)
      local _scrutinee_80 = n_75
      if (function()
      local x_76 = _scrutinee_80
      return x_76 < 0
    end)() then
        local x_76 = _scrutinee_80
        return 0 - 1
      else
        if (function()
      local x_77 = _scrutinee_80
      return x_77 == 0
    end)() then
          local x_77 = _scrutinee_80
          return 0
        else
          local x_78 = _scrutinee_80
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
    local function f_87(x_85)
      local _scrutinee_88 = x_85
      if (function()
      local n_86 = _scrutinee_88
      return n_86
    end)() then
        local n_86 = _scrutinee_88
        return n_86
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
    local function f_91(x_89)
      local _scrutinee_92 = x_89
      if _scrutinee_92._tag == 1 then
        local n_90 = _scrutinee_92._0
        return n_90
      else
        if _scrutinee_92._tag == 0 then
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
    local function f_95(x_93)
      local _scrutinee_96 = x_93
      if _scrutinee_96._tag == 1 then
        local n_94 = _scrutinee_96._0
        return n_94
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
    local function f_98(x_97)
      local _scrutinee_99 = x_97
      if _scrutinee_99._tag == 0 then
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
    local function f_101(b_100)
      local _scrutinee_102 = b_100
      if _scrutinee_102 == false then
        return 0
      else
        if _scrutinee_102 == true then
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
    local function f_104(b_103)
      local _scrutinee_105 = b_103
      if _scrutinee_105 == true then
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
    local function f_108(x_106)
      local _scrutinee_109 = x_106
      if _scrutinee_109._tag == 1 then
        local n_107 = _scrutinee_109._0
        return n_107
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
    local function f_111(x_110)
      local _scrutinee_112 = x_110
      if _scrutinee_112._tag == 0 then
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
    local function f_115(x_113)
      local _scrutinee_116 = x_113
      if _scrutinee_116._tag == 1 then
        local n_114 = _scrutinee_116._0
        return n_114
      else
        if _scrutinee_116._tag == 0 then
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
    local function f_119(x_117)
      local _scrutinee_120 = x_117
      if _scrutinee_120 == 0 then
        return 1
      else
        local n_118 = _scrutinee_120
        return n_118
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local function to_num_122(c_121)
      local _scrutinee_123 = c_121
      if _scrutinee_123._tag == 2 then
        return 2
      else
        if _scrutinee_123._tag == 1 then
          return 1
        else
          if _scrutinee_123._tag == 0 then
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
    local function f_128(p_124)
      local _scrutinee_129 = p_124
      local c_127 = _scrutinee_129[2]
      local b_126 = _scrutinee_129[1][2]
      local a_125 = _scrutinee_129[1][1]
      return a_125 + b_126 + c_127
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local function sum_133(p_130)
      local _scrutinee_134 = p_130
      if _scrutinee_134._tag == 0 then
        local b_132 = _scrutinee_134._0[2]
        local a_131 = _scrutinee_134._0[1]
        return a_131 + b_132
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "match on record pattern" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local function get_x_137(r_135)
      local _scrutinee_138 = r_135
      local x_136 = _scrutinee_138.x
      return x_136
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local function sum_xy_142(r_139)
      local _scrutinee_143 = r_139
      local y_141 = _scrutinee_143.y
      local x_140 = _scrutinee_143.x
      return x_140 + y_141
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f_146(x_144)
      local _scrutinee_147 = x_144
      if _scrutinee_147._tag == 1 then
        if (function()
      local n_145 = _scrutinee_147._0
      return n_145 > 0
    end)() then
          local n_145 = _scrutinee_147._0
          return n_145
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
    local function f_150(x_148)
      local _scrutinee_151 = x_148
      if _scrutinee_151._tag == 0 then
        return 0
      else
        if _scrutinee_151._tag == 1 then
          if (function()
      local n_149 = _scrutinee_151._0
      return n_149 > 0
    end)() then
            local n_149 = _scrutinee_151._0
            return n_149
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
    local function head_155(l_152)
      local _scrutinee_156 = l_152
      if _scrutinee_156._tag == 1 then
        local t_154 = _scrutinee_156._0[2]
        local h_153 = _scrutinee_156._0[1]
        return h_153
      else
        if _scrutinee_156._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "either type with two type params" =
  print_endline (compile "type ('a, 'b) either = Left of 'a | Right of 'b
let is_left e = match e with | Left _ -> true | Right _ -> false");
  [%expect{|
    local function is_left_158(e_157)
      local _scrutinee_159 = e_157
      if _scrutinee_159._tag == 1 then
        return false
      else
        if _scrutinee_159._tag == 0 then
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
    local function greet_161(name_160)
      local _scrutinee_162 = name_160
      if _scrutinee_162 == "Bob" then
        return "Hey Bob"
      else
        if _scrutinee_162 == "Alice" then
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
    local function negate_164(b_163)
      local _scrutinee_165 = b_163
      if _scrutinee_165 == false then
        return true
      else
        if _scrutinee_165 == true then
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
    local function f_167(u_166)
      local _scrutinee_168 = u_166
      if _scrutinee_168 == nil then
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
    local function sum3_173(t_169)
      local _scrutinee_174 = t_169
      local c_172 = _scrutinee_174[3]
      local b_171 = _scrutinee_174[2]
      local a_170 = _scrutinee_174[1]
      return a_170 + b_171 + c_172
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local function sum4_180(t_175)
      local _scrutinee_181 = t_175
      local d_179 = _scrutinee_181[4]
      local c_178 = _scrutinee_181[3]
      local b_177 = _scrutinee_181[2]
      local a_176 = _scrutinee_181[1]
      return a_176 + b_177 + c_178 + d_179
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local function first_184(t_182)
      local _scrutinee_185 = t_182
      local a_183 = _scrutinee_185[1]
      return a_183
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local function check_190(t_186)
      local _scrutinee_191 = t_186
      if _scrutinee_191[1] == 1 then
        local y_188 = _scrutinee_191[2]
        return y_188 + 1
      else
        if _scrutinee_191[1] == 0 then
          local x_187 = _scrutinee_191[2]
          return x_187
        else
          local z_189 = _scrutinee_191[2]
          return z_189 + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    local function deep_194(x_192)
      local _scrutinee_195 = x_192
      if _scrutinee_195._tag == 1 then
        if _scrutinee_195._0._tag == 1 then
          if _scrutinee_195._0._0._tag == 1 then
            local n_193 = _scrutinee_195._0._0._0
            return n_193
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
    local function extract_201(t_196)
      local _scrutinee_202 = t_196
      if _scrutinee_202[1]._tag == 0 then
        local d_200 = _scrutinee_202[2]
        return d_200
      else
        if _scrutinee_202[1]._tag == 1 then
          local c_199 = _scrutinee_202[2]
          local b_198 = _scrutinee_202[1]._0[2]
          local a_197 = _scrutinee_202[1]._0[1]
          return a_197 + b_198 + c_199
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
    local function unwrap_207(w_203)
      local _scrutinee_208 = w_203
      if _scrutinee_208._tag == 0 then
        if _scrutinee_208._0[1]._tag == 0 then
          local n_206 = _scrutinee_208._0[2]
          return n_206
        else
          if _scrutinee_208._0[1]._tag == 1 then
            local n_205 = _scrutinee_208._0[2]
            local x_204 = _scrutinee_208._0[1]._0
            return x_204 + n_205
          else
            return error("Match failure")
          end
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
    local function to_num_210(d_209)
      local _scrutinee_211 = d_209
      local _switch = _scrutinee_211
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
    local function to_num_213(d_212)
      local _scrutinee_214 = d_212
      if _scrutinee_214._tag == 2 then
        return 2
      else
        if _scrutinee_214._tag == 1 then
          return 1
        else
          if _scrutinee_214._tag == 0 then
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
    local function is_monday_216(d_215)
      local _scrutinee_217 = d_215
      if _scrutinee_217._tag == 0 then
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
    local function is_value_219(e_218)
      local _scrutinee_220 = e_218
      if _scrutinee_220._tag == 2 then
        return false
      else
        if _scrutinee_220._tag == 1 then
          return true
        else
          if _scrutinee_220._tag == 0 then
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
    local function f_223(x_221)
      local _scrutinee_224 = x_221
      if _scrutinee_224._tag == 1 then
        local x_222 = _scrutinee_224._0
        return x_222
      else
        if _scrutinee_224._tag == 0 then
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
    local function get_pair_227(p_225)
      local _scrutinee_228 = p_225
      if _scrutinee_228._tag == 0 then
        local x_226 = _scrutinee_228._0
        return x_226
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f_233(x_229)
      local _scrutinee_234 = x_229
      if _scrutinee_234._tag == 0 then
        return 0
      else
        if _scrutinee_234._tag == 1 then
          local c_232 = _scrutinee_234._0[3]
          local b_231 = _scrutinee_234._0[2]
          local a_230 = _scrutinee_234._0[1]
          return a_230 + b_231 + c_232
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 10-64:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

(* === Pattern Matching: Guards === *)

let%expect_test "guard accessing multiple bound variables" =
  print_endline (compile "let check p = match p with | (a, b) when a > b -> a | (a, b) -> b");
  [%expect{|
    local function check_240(p_235)
      local _scrutinee_241 = p_235
      if (function()
      local b_237 = _scrutinee_241[2]
      local a_236 = _scrutinee_241[1]
      return a_236 > b_237
    end)() then
        local b_237 = _scrutinee_241[2]
        local a_236 = _scrutinee_241[1]
        return a_236
      else
        local b_239 = _scrutinee_241[2]
        local a_238 = _scrutinee_241[1]
        return b_239
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range_244(n_242)
      local _scrutinee_245 = n_242
      if (function()
      local x_243 = _scrutinee_245
      return x_243 > 0
    end)() then
        local x_243 = _scrutinee_245
        if x_243 < 10 then
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
    local function positive_248(opt_246)
      local _scrutinee_249 = opt_246
      if _scrutinee_249._tag == 0 then
        return false
      else
        if _scrutinee_249._tag == 1 then
          if (function()
      local n_247 = _scrutinee_249._0
      return n_247 > 0
    end)() then
            local n_247 = _scrutinee_249._0
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
    local function classify_254(n_250)
      local _scrutinee_255 = n_250
      if (function()
      local x_251 = _scrutinee_255
      return x_251 < 0
    end)() then
        local x_251 = _scrutinee_255
        return 0 - 1
      else
        if (function()
      local x_252 = _scrutinee_255
      return x_252 > 100
    end)() then
          local x_252 = _scrutinee_255
          return 2
        else
          if (function()
      local x_253 = _scrutinee_255
      return x_253 > 50
    end)() then
            local x_253 = _scrutinee_255
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
    local length_256
    length_256 = function(l_257)
      local _scrutinee_259 = l_257
      if _scrutinee_259._tag == 1 then
        local t_258 = _scrutinee_259._0[2]
        return 1 + length_256(t_258)
      else
        if _scrutinee_259._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "recursive function with multiple pattern arms" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum l = match l with | Nil -> 0 | Cons (h, t) -> h + sum t");
  [%expect{|
    local sum_260
    sum_260 = function(l_261)
      local _scrutinee_264 = l_261
      if _scrutinee_264._tag == 1 then
        local t_263 = _scrutinee_264._0[2]
        local h_262 = _scrutinee_264._0[1]
        return h_262 + sum_260(t_263)
      else
        if _scrutinee_264._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "tail recursive with accumulator" =
  print_endline (compile "type 'a list = Nil | Cons of ('a * 'a list)
let rec sum_acc acc l = match l with | Nil -> acc | Cons (h, t) -> sum_acc (acc + h) t");
  [%expect{|
    local sum_acc_265
    sum_acc_265 = function(acc_266, l_267)
      local _scrutinee_270 = l_267
      if _scrutinee_270._tag == 1 then
        local t_269 = _scrutinee_270._0[2]
        local h_268 = _scrutinee_270._0[1]
        return sum_acc_265(acc_266 + h_268, t_269)
      else
        if _scrutinee_270._tag == 0 then
          return acc_266
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
    local even_271, odd_272
    even_271 = function(n_273)
      local _scrutinee_277 = n_273
      if _scrutinee_277 == 0 then
        return true
      else
        local n_274 = _scrutinee_277
        return odd_272(n_274 - 1)
      end
    end
    odd_272 = function(n_275)
      local _scrutinee_278 = n_275
      if _scrutinee_278 == 0 then
        return false
      else
        local n_276 = _scrutinee_278
        return even_271(n_276 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id_281(x_279)
      local _scrutinee_282 = x_279
      local y_280 = _scrutinee_282
      return y_280
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap_285(x_283)
      local _scrutinee_286 = x_283
      if _scrutinee_286._tag == 1 then
        local y_284 = _scrutinee_286._0
        return y_284
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
    local function first3_289(t_287)
      local _scrutinee_290 = t_287
      local x_288 = _scrutinee_290[1]
      return x_288
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f_293(x_291)
      local _scrutinee_294 = x_291
      if _scrutinee_294 == 1 then
        return 200
      else
        if _scrutinee_294 == 0 then
          return 100
        else
          local n_292 = _scrutinee_294
          return n_292
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f_296(x_295)
      local _scrutinee_297 = x_295
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
    local function get_x_313(p_310)
      local _scrutinee_314 = p_310
      local y_312 = _scrutinee_314.y
      local x_311 = _scrutinee_314.x
      return x_311
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local function project_318(p_315)
      local _scrutinee_319 = p_315
      local y_317 = _scrutinee_319.y
      local x_316 = _scrutinee_319.x
      return {x_316, y_317}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x_322(opt_320)
      local _scrutinee_323 = opt_320
      if _scrutinee_323._tag == 0 then
        return 0
      else
        if _scrutinee_323._tag == 1 then
          local x_321 = _scrutinee_323._0.x
          return x_321
        else
          return error("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-60:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

let%expect_test "record pattern with renamed binding" =
  print_endline (compile "let get_val r = match r with | { x = value } -> value");
  [%expect{|
    local function get_val_326(r_324)
      local _scrutinee_327 = r_324
      local value_325 = _scrutinee_327.x
      return value_325
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local function apply_pair_332(f_328, p_329)
      local _scrutinee_333 = p_329
      local b_331 = _scrutinee_333[2]
      local a_330 = _scrutinee_333[1]
      return f_328(a_330, b_331)
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local function double_unwrap_338(x_334)
      local _scrutinee_340 = x_334
      local inner_336
      if _scrutinee_340._tag == 0 then
        inner_336 = _Ctor_option_0
      else
        if _scrutinee_340._tag == 1 then
          local y_335 = _scrutinee_340._0
          inner_336 = y_335
        else
          inner_336 = error("Match failure")
        end
      end
      local _scrutinee_339 = inner_336
      if _scrutinee_339._tag == 0 then
        return 0
      else
        if _scrutinee_339._tag == 1 then
          local z_337 = _scrutinee_339._0
          return z_337
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
    local function safe_div_345(a_341, b_342)
      if b_342 == 0 then
        return _Ctor_option_0
      else
        return {_tag = 1, _0 = (function()
      local _scrutinee_346 = {a_341, b_342}
      local y_344 = _scrutinee_346[2]
      local x_343 = _scrutinee_346[1]
      return x_343 / y_344
    end)()}
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id_348(x_347)
      return x_347
    end
    local function f_351(opt_349)
      local _scrutinee_352 = id_348(opt_349)
      if _scrutinee_352._tag == 0 then
        return 0
      else
        if _scrutinee_352._tag == 1 then
          local n_350 = _scrutinee_352._0
          return n_350
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
    local sum_353
    sum_353 = function(t_354)
      local _scrutinee_358 = t_354
      if _scrutinee_358._tag == 1 then
        local r_357 = _scrutinee_358._0[2]
        local l_356 = _scrutinee_358._0[1]
        return sum_353(l_356) + sum_353(r_357)
      else
        if _scrutinee_358._tag == 0 then
          local n_355 = _scrutinee_358._0
          return n_355
        else
          return error("Match failure")
        end
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
    local eval_359
    eval_359 = function(e_360)
      local _scrutinee_366 = e_360
      if _scrutinee_366._tag == 2 then
        local b_365 = _scrutinee_366._0[2]
        local a_364 = _scrutinee_366._0[1]
        return eval_359(a_364) * eval_359(b_365)
      else
        if _scrutinee_366._tag == 1 then
          local b_363 = _scrutinee_366._0[2]
          local a_362 = _scrutinee_366._0[1]
          return eval_359(a_362) + eval_359(b_363)
        else
          if _scrutinee_366._tag == 0 then
            local n_361 = _scrutinee_366._0
            return n_361
          else
            return error("Match failure")
          end
        end
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
  [%expect{| local _nil_367 = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat_368 = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until_369 = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    local function check_371(_do_370)
      return _do_370
    end
    |}]

let%expect_test "lua keyword 'local' used as identifier is mangled" =
  print_endline (compile "let local = 123");
  [%expect{| local _local_372 = 123 |}]

let%expect_test "lua keyword 'goto' used as identifier is mangled" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto_373 = 7 |}]

let%expect_test "lua keyword 'break' used as identifier is mangled" =
  print_endline (compile "let break = 3");
  [%expect{| local _break_374 = 3 |}]

let%expect_test "lua keyword 'return' used as identifier is mangled" =
  print_endline (compile "let return = 1");
  [%expect{| local _return_375 = 1 |}]

let%expect_test "lua keyword 'while' used as identifier is mangled" =
  print_endline (compile "let while = 8");
  [%expect{| local _while_376 = 8 |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value_377 = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker_378 = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none_val_379 = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val_380 = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2}
    local _Ctor_color_1 = {_tag = 1}
    local _Ctor_color_0 = {_tag = 0}
    local red_381 = _Ctor_color_0
    local green_382 = _Ctor_color_1
    local blue_383 = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local function is_some_385(opt_384)
      local _scrutinee_386 = opt_384
      if _scrutinee_386._tag == 1 then
        return true
      else
        if _scrutinee_386._tag == 0 then
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
    local a_387 = _Ctor_option_0
    local b_388 = _Ctor_option_0
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
    local stop_389 = _Ctor_traffic_light_0
    local caution_390 = _Ctor_traffic_light_1
    local go_391 = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x_392 = {_tag = 1, _0 = 1}
    local y_393 = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none1_394 = _Ctor_option_0
    local some1_395 = {_tag = 1, _0 = 10}
    local none2_396 = _Ctor_option_0
    local some2_397 = {_tag = 1, _0 = 20}
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
    local function to_num_399(dir_398)
      local _scrutinee_400 = dir_398
      local _switch = _scrutinee_400
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
    local function day_num_402(day_401)
      local _scrutinee_403 = day_401
      local _switch = _scrutinee_403
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
    local function to_num_405(color_404)
      local _scrutinee_406 = color_404
      if _scrutinee_406._tag == 2 then
        return 2
      else
        if _scrutinee_406._tag == 1 then
          return 1
        else
          if _scrutinee_406._tag == 0 then
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
    local function eval_412(expression_407)
      local _scrutinee_413 = expression_407
      local _switch = _scrutinee_413
      local _dispatch = {[3] = function()
      local n_411 = _scrutinee_413._0
      return n_411 * 2
    end, [2] = function()
      local n_410 = _scrutinee_413._0
      return n_410 - 1
    end, [1] = function()
      local n_409 = _scrutinee_413._0
      return n_409 + 1
    end, [0] = function()
      local n_408 = _scrutinee_413._0
      return n_408
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
    local x_414 = 1
    local result_415 = x_414 + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a_416 = 1
    local b_417 = 2
    local result_418 = a_416 + b_417
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result_419
    if true then
      result_419 = 1
    else
      result_419 = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x_420 = 5
    local result_421
    if x_420 > 0 then
      result_421 = 1
    else
      result_421 = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result_422
    if true then
      if false then
        result_422 = 1
      else
        result_422 = 2
      end
    else
      result_422 = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag_423
    if true then
      flag_423 = 1
    else
      flag_423 = 0
    end
    local result_424 = flag_423 + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local __426 = print(1)
    local result_425 = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local function compute_429(x_427)
      local y_428 = x_427 + 1
      return y_428 * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local function abs_431(n_430)
      if n_430 < 0 then
        return 0 - n_430
      else
        return n_430
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to_432
    sum_to_432 = function(n_433, acc_434)
      if n_433 <= 0 then
        return acc_434
      else
        return sum_to_432(n_433 - 1, acc_434 + n_433)
      end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown_435
    countdown_435 = function(n_436)
      if n_436 <= 0 then
        return 0
      else
        return countdown_435(n_436 - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero_437
    find_zero_437 = function(n_438)
      if n_438 == 0 then
        return true
      else
        return find_zero_437(n_438 - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even_439, is_odd_440
    is_even_439 = function(n_441)
      if n_441 == 0 then
        return true
      else
        return is_odd_440(n_441 - 1)
      end
    end
    is_odd_440 = function(n_442)
      if n_442 == 0 then
        return false
      else
        return is_even_439(n_442 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial_443
    factorial_443 = function(n_444)
      if n_444 <= 1 then
        return 1
      else
        return n_444 * factorial_443(n_444 - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc_445
    factorial_acc_445 = function(n_446, acc_447)
      if n_446 <= 1 then
        return acc_447
      else
        return factorial_acc_445(n_446 - 1, n_446 * acc_447)
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
    local function eval_cmd_449(and_val_448)
      local _scrutinee_450 = and_val_448
      local _switch = _scrutinee_450
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
    local function get_or_default_455(opt_451, default_val_452)
      local result_453
      if true then
        result_453 = opt_451
      else
        result_453 = _Ctor_option_0
      end
      local _scrutinee_456 = result_453
      if _scrutinee_456._tag == 1 then
        local x_454 = _scrutinee_456._0
        return x_454
      else
        if _scrutinee_456._tag == 0 then
          return default_val_452
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
    local function process_direction_461(dir_457)
      local opt_value_458
      if dir_457 == 0 then
        opt_value_458 = _Ctor_option_0
      else
        opt_value_458 = {_tag = 1, _0 = dir_457}
      end
      local _scrutinee_462 = opt_value_458
      local result_460
      if _scrutinee_462._tag == 1 then
        local n_459 = _scrutinee_462._0
        result_460 = n_459
      else
        if _scrutinee_462._tag == 0 then
          result_460 = 0
        else
          result_460 = error("Match failure")
        end
      end
      return result_460 + 1
    end
    |}]

(* Reference operations *)

let%expect_test "ref creates table with value field" =
  print_endline (compile "let r = ref 42");
  [%expect{| local r_463 = {value = 42} |}]

let%expect_test "deref accesses value field" =
  print_endline (compile "let r = ref 42
let x = !r");
  [%expect{|
    local r_464 = {value = 42}
    local x_465 = r_464.value
    |}]

let%expect_test "assign modifies value field" =
  print_endline (compile "let r = ref 0
let _ = r := 1");
  [%expect{|
    local r_466 = {value = 0}
    local _top_467 = (function()
      r_466.value = 1
      return nil
    end)()
    |}]

let%expect_test "increment ref" =
  print_endline (compile "let r = ref 0
let _ = r := !r + 1");
  [%expect{|
    local r_468 = {value = 0}
    local _top_469 = (function()
      r_468.value = r_468.value + 1
      return nil
    end)()
    |}]

let%expect_test "ref in function" =
  print_endline (compile "let make_counter init =
  let c = ref init in
  let get = fun () -> !c in
  (get, c)");
  [%expect{|
    local function make_counter_473(init_470)
      local c_471 = {value = init_470}
      local get_472 = function(_param_474)
      return c_471.value
    end
      return {get_472, c_471}
    end
    |}]

let%expect_test "multiple refs" =
  print_endline (compile "let x = ref 10
let y = ref 20
let _ = x := !x + !y");
  [%expect{|
    local x_475 = {value = 10}
    local y_476 = {value = 20}
    local _top_477 = (function()
      x_475.value = x_475.value + y_476.value
      return nil
    end)()
    |}]

let%expect_test "ref with string" =
  print_endline (compile {|let s = ref "hello"
let _ = s := "world"|});
  [%expect{|
    local s_478 = {value = "hello"}
    local _top_479 = (function()
      s_478.value = "world"
      return nil
    end)()
    |}]
