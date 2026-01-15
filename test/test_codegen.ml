let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "record literal generates lua table" =
  print_endline (compile "let r = { x = 1; y = 2 }");
  [%expect{| local r_14 = {x = 1, y = 2} |}]

let%expect_test "record field access generates dot notation" =
  print_endline (compile "let r = { x = 42 }
let v = r.x");
  [%expect{|
    local r_15 = {x = 42}
    local v_16 = r_15.x
    |}]

let%expect_test "record update generates shallow copy" =
  print_endline (compile "let r = { x = 1; y = 2 }
let r2 = { r with x = 10 }");
  [%expect{|
    local r_17 = {x = 1, y = 2}
    local r2_18 = (function()
      local _result = {}
      for _k, _v in pairs(r_17) do
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
    local x_19 = 1
    local y_20 = 2
    local r_21 = {x = x_19, y = y_20}
    |}]

let%expect_test "nested record access" =
  print_endline (compile "let outer = { inner = { value = 42 } }
let v = outer.inner.value");
  [%expect{|
    local outer_22 = {inner = {value = 42}}
    local v_23 = outer_22.inner.value
    |}]

let%expect_test "match with integer patterns" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 1 -> 2 | n -> n");
  [%expect{|
    local function f_26(x_24)
      local _scrutinee_27 = x_24
      if _scrutinee_27 == 1 then
        return 2
      else
        if _scrutinee_27 == 0 then
          return 1
        else
          local n_25 = _scrutinee_27
          return n_25
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local function sum_pair_31(p_28)
      local _scrutinee_32 = p_28
      local b_30 = _scrutinee_32[2]
      local a_29 = _scrutinee_32[1]
      return a_29 + b_30
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local function abs_36(n_33)
      local _scrutinee_37 = n_33
      if (function()
      local x_34 = _scrutinee_37
      return x_34 < 0
    end)() then
        local x_34 = _scrutinee_37
        return 0 - x_34
      else
        local x_35 = _scrutinee_37
        return x_35
      end
    end
    File "<string>", line 1, characters 51-57:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local function is_some_39(opt_38)
      local _scrutinee_40 = opt_38
      if _scrutinee_40._tag == 1 then
        return true
      else
        if _scrutinee_40._tag == 0 then
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
    local function get_or_default_44(opt_41, default_42)
      local _scrutinee_45 = opt_41
      if _scrutinee_45._tag == 1 then
        local x_43 = _scrutinee_45._0
        return x_43
      else
        if _scrutinee_45._tag == 0 then
          return default_42
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
    local none_46 = _Ctor_option_0
    local some_val_47 = {_tag = 1, _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local function get_x_49(r_48)
      return r_48.x
    end
    local p1_50 = {x = 1, y = 2}
    local p2_51 = {x = 10, z = 20}
    local v1_52 = get_x_49(p1_50)
    local v2_53 = get_x_49(p2_51)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r_54 = {a = 1, b = 2, c = 3}
    local r2_55 = (function()
      local _result = {}
      for _k, _v in pairs(r_54) do
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
    local function make_point_58(x_56, y_57)
      return {x = x_56, y = y_57}
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local function f_62(p_59)
      local _tuple_63 = p_59
      local a_60 = _tuple_63[1]
      local b_61 = _tuple_63[2]
      return a_60 + b_61
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
    local function f_67(opt_64)
      local _scrutinee_68 = opt_64
      if _scrutinee_68._tag == 1 then
        local x_65 = _scrutinee_68._0
        local _scrutinee_69 = x_65
        if _scrutinee_69 == 0 then
          return 1
        else
          local n_66 = _scrutinee_69
          return n_66 + 1
        end
      else
        if _scrutinee_68._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r_70 = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x_71 = 1
    local r_72 = {a = x_71 + 1, b = x_71 * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local function f_74(x_73)
      local _scrutinee_75 = x_73
      return 42
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local function classify_80(n_76)
      local _scrutinee_81 = n_76
      if (function()
      local x_77 = _scrutinee_81
      return x_77 < 0
    end)() then
        local x_77 = _scrutinee_81
        return 0 - 1
      else
        if (function()
      local x_78 = _scrutinee_81
      return x_78 == 0
    end)() then
          local x_78 = _scrutinee_81
          return 0
        else
          local x_79 = _scrutinee_81
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
    local function f_88(x_86)
      local _scrutinee_89 = x_86
      if (function()
      local n_87 = _scrutinee_89
      return n_87
    end)() then
        local n_87 = _scrutinee_89
        return n_87
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
    local function f_92(x_90)
      local _scrutinee_93 = x_90
      if _scrutinee_93._tag == 1 then
        local n_91 = _scrutinee_93._0
        return n_91
      else
        if _scrutinee_93._tag == 0 then
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
    local function f_96(x_94)
      local _scrutinee_97 = x_94
      if _scrutinee_97._tag == 1 then
        local n_95 = _scrutinee_97._0
        return n_95
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
    local function f_99(x_98)
      local _scrutinee_100 = x_98
      if _scrutinee_100._tag == 0 then
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
    local function f_102(b_101)
      local _scrutinee_103 = b_101
      if _scrutinee_103 == false then
        return 0
      else
        if _scrutinee_103 == true then
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
    local function f_105(b_104)
      local _scrutinee_106 = b_104
      if _scrutinee_106 == true then
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
    local function f_109(x_107)
      local _scrutinee_110 = x_107
      if _scrutinee_110._tag == 1 then
        local n_108 = _scrutinee_110._0
        return n_108
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
    local function f_112(x_111)
      local _scrutinee_113 = x_111
      if _scrutinee_113._tag == 0 then
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
    local function f_116(x_114)
      local _scrutinee_117 = x_114
      if _scrutinee_117._tag == 1 then
        local n_115 = _scrutinee_117._0
        return n_115
      else
        if _scrutinee_117._tag == 0 then
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
    local function f_120(x_118)
      local _scrutinee_121 = x_118
      if _scrutinee_121 == 0 then
        return 1
      else
        local n_119 = _scrutinee_121
        return n_119
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local function to_num_123(c_122)
      local _scrutinee_124 = c_122
      if _scrutinee_124._tag == 2 then
        return 2
      else
        if _scrutinee_124._tag == 1 then
          return 1
        else
          if _scrutinee_124._tag == 0 then
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
    local function f_129(p_125)
      local _scrutinee_130 = p_125
      local c_128 = _scrutinee_130[2]
      local b_127 = _scrutinee_130[1][2]
      local a_126 = _scrutinee_130[1][1]
      return a_126 + b_127 + c_128
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local function sum_134(p_131)
      local _scrutinee_135 = p_131
      if _scrutinee_135._tag == 0 then
        local b_133 = _scrutinee_135._0[2]
        local a_132 = _scrutinee_135._0[1]
        return a_132 + b_133
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "match on record pattern" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local function get_x_138(r_136)
      local _scrutinee_139 = r_136
      local x_137 = _scrutinee_139.x
      return x_137
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local function sum_xy_143(r_140)
      local _scrutinee_144 = r_140
      local y_142 = _scrutinee_144.y
      local x_141 = _scrutinee_144.x
      return x_141 + y_142
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f_147(x_145)
      local _scrutinee_148 = x_145
      if _scrutinee_148._tag == 1 then
        if (function()
      local n_146 = _scrutinee_148._0
      return n_146 > 0
    end)() then
          local n_146 = _scrutinee_148._0
          return n_146
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
    local function f_151(x_149)
      local _scrutinee_152 = x_149
      if _scrutinee_152._tag == 0 then
        return 0
      else
        if _scrutinee_152._tag == 1 then
          if (function()
      local n_150 = _scrutinee_152._0
      return n_150 > 0
    end)() then
            local n_150 = _scrutinee_152._0
            return n_150
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
    local function head_156(l_153)
      local _scrutinee_157 = l_153
      if _scrutinee_157._tag == 1 then
        local t_155 = _scrutinee_157._0[2]
        local h_154 = _scrutinee_157._0[1]
        return h_154
      else
        if _scrutinee_157._tag == 0 then
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
    local function is_left_159(e_158)
      local _scrutinee_160 = e_158
      if _scrutinee_160._tag == 1 then
        return false
      else
        if _scrutinee_160._tag == 0 then
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
    local function greet_162(name_161)
      local _scrutinee_163 = name_161
      if _scrutinee_163 == "Bob" then
        return "Hey Bob"
      else
        if _scrutinee_163 == "Alice" then
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
    local function negate_165(b_164)
      local _scrutinee_166 = b_164
      if _scrutinee_166 == false then
        return true
      else
        if _scrutinee_166 == true then
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
    local function f_168(u_167)
      local _scrutinee_169 = u_167
      if _scrutinee_169 == nil then
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
    local function sum3_174(t_170)
      local _scrutinee_175 = t_170
      local c_173 = _scrutinee_175[3]
      local b_172 = _scrutinee_175[2]
      local a_171 = _scrutinee_175[1]
      return a_171 + b_172 + c_173
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local function sum4_181(t_176)
      local _scrutinee_182 = t_176
      local d_180 = _scrutinee_182[4]
      local c_179 = _scrutinee_182[3]
      local b_178 = _scrutinee_182[2]
      local a_177 = _scrutinee_182[1]
      return a_177 + b_178 + c_179 + d_180
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local function first_185(t_183)
      local _scrutinee_186 = t_183
      local a_184 = _scrutinee_186[1]
      return a_184
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local function check_191(t_187)
      local _scrutinee_192 = t_187
      if _scrutinee_192[1] == 1 then
        local y_189 = _scrutinee_192[2]
        return y_189 + 1
      else
        if _scrutinee_192[1] == 0 then
          local x_188 = _scrutinee_192[2]
          return x_188
        else
          local z_190 = _scrutinee_192[2]
          return z_190 + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    local function deep_195(x_193)
      local _scrutinee_196 = x_193
      if _scrutinee_196._tag == 1 then
        if _scrutinee_196._0._tag == 1 then
          if _scrutinee_196._0._0._tag == 1 then
            local n_194 = _scrutinee_196._0._0._0
            return n_194
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
    local function extract_202(t_197)
      local _scrutinee_203 = t_197
      if _scrutinee_203[1]._tag == 0 then
        local d_201 = _scrutinee_203[2]
        return d_201
      else
        if _scrutinee_203[1]._tag == 1 then
          local c_200 = _scrutinee_203[2]
          local b_199 = _scrutinee_203[1]._0[2]
          local a_198 = _scrutinee_203[1]._0[1]
          return a_198 + b_199 + c_200
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
    local function unwrap_208(w_204)
      local _scrutinee_209 = w_204
      if _scrutinee_209._tag == 0 then
        if _scrutinee_209._0[1]._tag == 0 then
          local n_207 = _scrutinee_209._0[2]
          return n_207
        else
          if _scrutinee_209._0[1]._tag == 1 then
            local n_206 = _scrutinee_209._0[2]
            local x_205 = _scrutinee_209._0[1]._0
            return x_205 + n_206
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
    local function to_num_211(d_210)
      local _scrutinee_212 = d_210
      local _switch = _scrutinee_212
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
    local function to_num_214(d_213)
      local _scrutinee_215 = d_213
      if _scrutinee_215._tag == 2 then
        return 2
      else
        if _scrutinee_215._tag == 1 then
          return 1
        else
          if _scrutinee_215._tag == 0 then
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
    local function is_monday_217(d_216)
      local _scrutinee_218 = d_216
      if _scrutinee_218._tag == 0 then
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
    local function is_value_220(e_219)
      local _scrutinee_221 = e_219
      if _scrutinee_221._tag == 2 then
        return false
      else
        if _scrutinee_221._tag == 1 then
          return true
        else
          if _scrutinee_221._tag == 0 then
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
    local function f_224(x_222)
      local _scrutinee_225 = x_222
      if _scrutinee_225._tag == 1 then
        local x_223 = _scrutinee_225._0
        return x_223
      else
        if _scrutinee_225._tag == 0 then
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
    local function get_pair_228(p_226)
      local _scrutinee_229 = p_226
      if _scrutinee_229._tag == 0 then
        local x_227 = _scrutinee_229._0
        return x_227
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f_234(x_230)
      local _scrutinee_235 = x_230
      if _scrutinee_235._tag == 0 then
        return 0
      else
        if _scrutinee_235._tag == 1 then
          local c_233 = _scrutinee_235._0[3]
          local b_232 = _scrutinee_235._0[2]
          local a_231 = _scrutinee_235._0[1]
          return a_231 + b_232 + c_233
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
    local function check_241(p_236)
      local _scrutinee_242 = p_236
      if (function()
      local b_238 = _scrutinee_242[2]
      local a_237 = _scrutinee_242[1]
      return a_237 > b_238
    end)() then
        local b_238 = _scrutinee_242[2]
        local a_237 = _scrutinee_242[1]
        return a_237
      else
        local b_240 = _scrutinee_242[2]
        local a_239 = _scrutinee_242[1]
        return b_240
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range_245(n_243)
      local _scrutinee_246 = n_243
      if (function()
      local x_244 = _scrutinee_246
      return x_244 > 0
    end)() then
        local x_244 = _scrutinee_246
        if x_244 < 10 then
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
    local function positive_249(opt_247)
      local _scrutinee_250 = opt_247
      if _scrutinee_250._tag == 0 then
        return false
      else
        if _scrutinee_250._tag == 1 then
          if (function()
      local n_248 = _scrutinee_250._0
      return n_248 > 0
    end)() then
            local n_248 = _scrutinee_250._0
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
    local function classify_255(n_251)
      local _scrutinee_256 = n_251
      if (function()
      local x_252 = _scrutinee_256
      return x_252 < 0
    end)() then
        local x_252 = _scrutinee_256
        return 0 - 1
      else
        if (function()
      local x_253 = _scrutinee_256
      return x_253 > 100
    end)() then
          local x_253 = _scrutinee_256
          return 2
        else
          if (function()
      local x_254 = _scrutinee_256
      return x_254 > 50
    end)() then
            local x_254 = _scrutinee_256
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
    local length_257
    length_257 = function(l_258)
      local _scrutinee_260 = l_258
      if _scrutinee_260._tag == 1 then
        local t_259 = _scrutinee_260._0[2]
        return 1 + length_257(t_259)
      else
        if _scrutinee_260._tag == 0 then
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
    local sum_261
    sum_261 = function(l_262)
      local _scrutinee_265 = l_262
      if _scrutinee_265._tag == 1 then
        local t_264 = _scrutinee_265._0[2]
        local h_263 = _scrutinee_265._0[1]
        return h_263 + sum_261(t_264)
      else
        if _scrutinee_265._tag == 0 then
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
    local sum_acc_266
    sum_acc_266 = function(acc_267, l_268)
      local _scrutinee_271 = l_268
      if _scrutinee_271._tag == 1 then
        local t_270 = _scrutinee_271._0[2]
        local h_269 = _scrutinee_271._0[1]
        return sum_acc_266(acc_267 + h_269, t_270)
      else
        if _scrutinee_271._tag == 0 then
          return acc_267
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
    local even_272, odd_273
    even_272 = function(n_274)
      local _scrutinee_278 = n_274
      if _scrutinee_278 == 0 then
        return true
      else
        local n_275 = _scrutinee_278
        return odd_273(n_275 - 1)
      end
    end
    odd_273 = function(n_276)
      local _scrutinee_279 = n_276
      if _scrutinee_279 == 0 then
        return false
      else
        local n_277 = _scrutinee_279
        return even_272(n_277 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id_282(x_280)
      local _scrutinee_283 = x_280
      local y_281 = _scrutinee_283
      return y_281
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap_286(x_284)
      local _scrutinee_287 = x_284
      if _scrutinee_287._tag == 1 then
        local y_285 = _scrutinee_287._0
        return y_285
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
    local function first3_290(t_288)
      local _scrutinee_291 = t_288
      local x_289 = _scrutinee_291[1]
      return x_289
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f_294(x_292)
      local _scrutinee_295 = x_292
      if _scrutinee_295 == 1 then
        return 200
      else
        if _scrutinee_295 == 0 then
          return 100
        else
          local n_293 = _scrutinee_295
          return n_293
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f_297(x_296)
      local _scrutinee_298 = x_296
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
    local function get_x_314(p_311)
      local _scrutinee_315 = p_311
      local y_313 = _scrutinee_315.y
      local x_312 = _scrutinee_315.x
      return x_312
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local function project_319(p_316)
      local _scrutinee_320 = p_316
      local y_318 = _scrutinee_320.y
      local x_317 = _scrutinee_320.x
      return {x_317, y_318}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x_323(opt_321)
      local _scrutinee_324 = opt_321
      if _scrutinee_324._tag == 0 then
        return 0
      else
        if _scrutinee_324._tag == 1 then
          local x_322 = _scrutinee_324._0.x
          return x_322
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
    local function get_val_327(r_325)
      local _scrutinee_328 = r_325
      local value_326 = _scrutinee_328.x
      return value_326
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local function apply_pair_333(f_329, p_330)
      local _scrutinee_334 = p_330
      local b_332 = _scrutinee_334[2]
      local a_331 = _scrutinee_334[1]
      return f_329(a_331, b_332)
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local function double_unwrap_339(x_335)
      local _scrutinee_341 = x_335
      local inner_337
      if _scrutinee_341._tag == 0 then
        inner_337 = _Ctor_option_0
      else
        if _scrutinee_341._tag == 1 then
          local y_336 = _scrutinee_341._0
          inner_337 = y_336
        else
          inner_337 = error("Match failure")
        end
      end
      local _scrutinee_340 = inner_337
      if _scrutinee_340._tag == 0 then
        return 0
      else
        if _scrutinee_340._tag == 1 then
          local z_338 = _scrutinee_340._0
          return z_338
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
    local function safe_div_346(a_342, b_343)
      if b_343 == 0 then
        return _Ctor_option_0
      else
        return {_tag = 1, _0 = (function()
      local _scrutinee_347 = {a_342, b_343}
      local y_345 = _scrutinee_347[2]
      local x_344 = _scrutinee_347[1]
      return x_344 / y_345
    end)()}
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id_349(x_348)
      return x_348
    end
    local function f_352(opt_350)
      local _scrutinee_353 = id_349(opt_350)
      if _scrutinee_353._tag == 0 then
        return 0
      else
        if _scrutinee_353._tag == 1 then
          local n_351 = _scrutinee_353._0
          return n_351
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
    local sum_354
    sum_354 = function(t_355)
      local _scrutinee_359 = t_355
      if _scrutinee_359._tag == 1 then
        local r_358 = _scrutinee_359._0[2]
        local l_357 = _scrutinee_359._0[1]
        return sum_354(l_357) + sum_354(r_358)
      else
        if _scrutinee_359._tag == 0 then
          local n_356 = _scrutinee_359._0
          return n_356
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
    local eval_360
    eval_360 = function(e_361)
      local _scrutinee_367 = e_361
      if _scrutinee_367._tag == 2 then
        local b_366 = _scrutinee_367._0[2]
        local a_365 = _scrutinee_367._0[1]
        return eval_360(a_365) * eval_360(b_366)
      else
        if _scrutinee_367._tag == 1 then
          local b_364 = _scrutinee_367._0[2]
          local a_363 = _scrutinee_367._0[1]
          return eval_360(a_363) + eval_360(b_364)
        else
          if _scrutinee_367._tag == 0 then
            local n_362 = _scrutinee_367._0
            return n_362
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
  [%expect{| local _nil_368 = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat_369 = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until_370 = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    local function check_372(_do_371)
      return _do_371
    end
    |}]

let%expect_test "lua keyword 'local' used as identifier is mangled" =
  print_endline (compile "let local = 123");
  [%expect{| local _local_373 = 123 |}]

let%expect_test "lua keyword 'goto' used as identifier is mangled" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto_374 = 7 |}]

let%expect_test "lua keyword 'break' used as identifier is mangled" =
  print_endline (compile "let break = 3");
  [%expect{| local _break_375 = 3 |}]

let%expect_test "lua keyword 'return' used as identifier is mangled" =
  print_endline (compile "let return = 1");
  [%expect{| local _return_376 = 1 |}]

let%expect_test "lua keyword 'while' used as identifier is mangled" =
  print_endline (compile "let while = 8");
  [%expect{| local _while_377 = 8 |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value_378 = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker_379 = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none_val_380 = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val_381 = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2}
    local _Ctor_color_1 = {_tag = 1}
    local _Ctor_color_0 = {_tag = 0}
    local red_382 = _Ctor_color_0
    local green_383 = _Ctor_color_1
    local blue_384 = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local function is_some_386(opt_385)
      local _scrutinee_387 = opt_385
      if _scrutinee_387._tag == 1 then
        return true
      else
        if _scrutinee_387._tag == 0 then
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
    local a_388 = _Ctor_option_0
    local b_389 = _Ctor_option_0
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
    local stop_390 = _Ctor_traffic_light_0
    local caution_391 = _Ctor_traffic_light_1
    local go_392 = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x_393 = {_tag = 1, _0 = 1}
    local y_394 = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none1_395 = _Ctor_option_0
    local some1_396 = {_tag = 1, _0 = 10}
    local none2_397 = _Ctor_option_0
    local some2_398 = {_tag = 1, _0 = 20}
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
    local function to_num_400(dir_399)
      local _scrutinee_401 = dir_399
      local _switch = _scrutinee_401
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
    local function day_num_403(day_402)
      local _scrutinee_404 = day_402
      local _switch = _scrutinee_404
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
    local function to_num_406(color_405)
      local _scrutinee_407 = color_405
      if _scrutinee_407._tag == 2 then
        return 2
      else
        if _scrutinee_407._tag == 1 then
          return 1
        else
          if _scrutinee_407._tag == 0 then
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
    local function eval_413(expression_408)
      local _scrutinee_414 = expression_408
      local _switch = _scrutinee_414
      local _dispatch = {[3] = function()
      local n_412 = _scrutinee_414._0
      return n_412 * 2
    end, [2] = function()
      local n_411 = _scrutinee_414._0
      return n_411 - 1
    end, [1] = function()
      local n_410 = _scrutinee_414._0
      return n_410 + 1
    end, [0] = function()
      local n_409 = _scrutinee_414._0
      return n_409
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
    local x_415 = 1
    local result_416 = x_415 + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a_417 = 1
    local b_418 = 2
    local result_419 = a_417 + b_418
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result_420
    if true then
      result_420 = 1
    else
      result_420 = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x_421 = 5
    local result_422
    if x_421 > 0 then
      result_422 = 1
    else
      result_422 = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result_423
    if true then
      if false then
        result_423 = 1
      else
        result_423 = 2
      end
    else
      result_423 = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag_424
    if true then
      flag_424 = 1
    else
      flag_424 = 0
    end
    local result_425 = flag_424 + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local __427 = print(1)
    local result_426 = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local function compute_430(x_428)
      local y_429 = x_428 + 1
      return y_429 * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local function abs_432(n_431)
      if n_431 < 0 then
        return 0 - n_431
      else
        return n_431
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to_433
    sum_to_433 = function(n_434, acc_435)
      if n_434 <= 0 then
        return acc_435
      else
        return sum_to_433(n_434 - 1, acc_435 + n_434)
      end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown_436
    countdown_436 = function(n_437)
      if n_437 <= 0 then
        return 0
      else
        return countdown_436(n_437 - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero_438
    find_zero_438 = function(n_439)
      if n_439 == 0 then
        return true
      else
        return find_zero_438(n_439 - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even_440, is_odd_441
    is_even_440 = function(n_442)
      if n_442 == 0 then
        return true
      else
        return is_odd_441(n_442 - 1)
      end
    end
    is_odd_441 = function(n_443)
      if n_443 == 0 then
        return false
      else
        return is_even_440(n_443 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial_444
    factorial_444 = function(n_445)
      if n_445 <= 1 then
        return 1
      else
        return n_445 * factorial_444(n_445 - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc_446
    factorial_acc_446 = function(n_447, acc_448)
      if n_447 <= 1 then
        return acc_448
      else
        return factorial_acc_446(n_447 - 1, n_447 * acc_448)
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
    local function eval_cmd_450(and_val_449)
      local _scrutinee_451 = and_val_449
      local _switch = _scrutinee_451
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
    local function get_or_default_456(opt_452, default_val_453)
      local result_454
      if true then
        result_454 = opt_452
      else
        result_454 = _Ctor_option_0
      end
      local _scrutinee_457 = result_454
      if _scrutinee_457._tag == 1 then
        local x_455 = _scrutinee_457._0
        return x_455
      else
        if _scrutinee_457._tag == 0 then
          return default_val_453
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
    local function process_direction_462(dir_458)
      local opt_value_459
      if dir_458 == 0 then
        opt_value_459 = _Ctor_option_0
      else
        opt_value_459 = {_tag = 1, _0 = dir_458}
      end
      local _scrutinee_463 = opt_value_459
      local result_461
      if _scrutinee_463._tag == 1 then
        local n_460 = _scrutinee_463._0
        result_461 = n_460
      else
        if _scrutinee_463._tag == 0 then
          result_461 = 0
        else
          result_461 = error("Match failure")
        end
      end
      return result_461 + 1
    end
    |}]

(* Reference operations *)

let%expect_test "ref creates table with value field" =
  print_endline (compile "let r = ref 42");
  [%expect{| local r_464 = {value = 42} |}]

let%expect_test "deref accesses value field" =
  print_endline (compile "let r = ref 42
let x = !r");
  [%expect{|
    local r_465 = {value = 42}
    local x_466 = r_465.value
    |}]

let%expect_test "assign modifies value field" =
  print_endline (compile "let r = ref 0
let _ = r := 1");
  [%expect{|
    local r_467 = {value = 0}
    local _top_468 = (function()
      r_467.value = 1
      return nil
    end)()
    |}]

let%expect_test "increment ref" =
  print_endline (compile "let r = ref 0
let _ = r := !r + 1");
  [%expect{|
    local r_469 = {value = 0}
    local _top_470 = (function()
      r_469.value = r_469.value + 1
      return nil
    end)()
    |}]

let%expect_test "ref in function" =
  print_endline (compile "let make_counter init =
  let c = ref init in
  let get = fun () -> !c in
  (get, c)");
  [%expect{|
    local function make_counter_474(init_471)
      local c_472 = {value = init_471}
      local get_473 = function(_param_475)
      return c_472.value
    end
      return {get_473, c_472}
    end
    |}]

let%expect_test "multiple refs" =
  print_endline (compile "let x = ref 10
let y = ref 20
let _ = x := !x + !y");
  [%expect{|
    local x_476 = {value = 10}
    local y_477 = {value = 20}
    local _top_478 = (function()
      x_476.value = x_476.value + y_477.value
      return nil
    end)()
    |}]

let%expect_test "ref with string" =
  print_endline (compile {|let s = ref "hello"
let _ = s := "world"|});
  [%expect{|
    local s_479 = {value = "hello"}
    local _top_480 = (function()
      s_479.value = "world"
      return nil
    end)()
    |}]
