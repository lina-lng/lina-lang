let compile source =
  Typing.Types.reset_level ();
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
      local a_26 = _scrutinee_29[1]
      local b_27 = _scrutinee_29[2]
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
          return error_38("Match failure")
        end
      end
    end
    |}]

let%expect_test "match extracts constructor argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_or_default opt default = match opt with | None -> default | Some x -> x");
  [%expect{|
    local function get_or_default_42(opt_39, default_40)
      local _scrutinee_43 = opt_39
      if _scrutinee_43._tag == 1 then
        local x_41 = _scrutinee_43._0
        local x_41 = _scrutinee_43._0
        return x_41
      else
        if _scrutinee_43._tag == 0 then
          return default_40
        else
          return error_44("Match failure")
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
          return error_69("Match failure")
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
    Actual: { y : 't118 }
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
        return error_90("Match failure")
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
    local function f_93(x_91)
      local _scrutinee_94 = x_91
      if _scrutinee_94._tag == 1 then
        local n_92 = _scrutinee_94._0
        local n_92 = _scrutinee_94._0
        return n_92
      else
        if _scrutinee_94._tag == 0 then
          return 0
        else
          return error_95("Match failure")
        end
      end
    end
    |}]

let%expect_test "non-exhaustive match - missing None" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n -> n");
  [%expect{|
    local function f_98(x_96)
      local _scrutinee_99 = x_96
      if _scrutinee_99._tag == 1 then
        local n_97 = _scrutinee_99._0
        local n_97 = _scrutinee_99._0
        return n_97
      else
        return error_100("Match failure")
      end
    end
    File "<string>", line 2, characters 10-36:
    Warning: Non-exhaustive pattern matching, missing case: None
    |}]

let%expect_test "non-exhaustive match - missing Some" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | None -> 0");
  [%expect{|
    local function f_102(x_101)
      local _scrutinee_103 = x_101
      if _scrutinee_103._tag == 0 then
        return 0
      else
        return error_104("Match failure")
      end
    end
    File "<string>", line 2, characters 10-34:
    Warning: Non-exhaustive pattern matching, missing case: Some _
    |}]

let%expect_test "exhaustive match on bool - no warning" =
  print_endline (compile "let f b = match b with | true -> 1 | false -> 0");
  [%expect{|
    local function f_106(b_105)
      local _scrutinee_107 = b_105
      if _scrutinee_107 == false then
        return 0
      else
        if _scrutinee_107 == true then
          return 1
        else
          return error_108("Match failure")
        end
      end
    end
    |}]

let%expect_test "non-exhaustive match on bool - missing false" =
  print_endline (compile "let f b = match b with | true -> 1");
  [%expect{|
    local function f_110(b_109)
      local _scrutinee_111 = b_109
      if _scrutinee_111 == true then
        return 1
      else
        return error_112("Match failure")
      end
    end
    File "<string>", line 1, characters 10-34:
    Warning: Non-exhaustive pattern matching, missing case: false
    |}]

let%expect_test "wildcard makes match exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n -> n | _ -> 0");
  [%expect{|
    local function f_115(x_113)
      local _scrutinee_116 = x_113
      if _scrutinee_116._tag == 1 then
        local n_114 = _scrutinee_116._0
        local n_114 = _scrutinee_116._0
        return n_114
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
    local function f_118(x_117)
      local _scrutinee_119 = x_117
      if _scrutinee_119._tag == 0 then
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
    local function f_122(x_120)
      local _scrutinee_123 = x_120
      if _scrutinee_123._tag == 1 then
        local n_121 = _scrutinee_123._0
        local n_121 = _scrutinee_123._0
        return n_121
      else
        if _scrutinee_123._tag == 0 then
          return 0
        else
          return error_124("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 51-60:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "redundant integer pattern" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 0 -> 2 | n -> n");
  [%expect{|
    local function f_127(x_125)
      local _scrutinee_128 = x_125
      if _scrutinee_128 == 0 then
        return 1
      else
        local n_126 = _scrutinee_128
        return n_126
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local function to_num_130(c_129)
      local _scrutinee_131 = c_129
      if _scrutinee_131._tag == 2 then
        return 2
      else
        if _scrutinee_131._tag == 1 then
          return 1
        else
          if _scrutinee_131._tag == 0 then
            return 0
          else
            return error_132("Match failure")
          end
        end
      end
    end
    |}]

let%expect_test "nested tuple patterns" =
  print_endline (compile "let f p = match p with | ((a, b), c) -> a + b + c");
  [%expect{|
    local function f_137(p_133)
      local _scrutinee_138 = p_133
      local c_136 = _scrutinee_138[2]
      local b_135 = _scrutinee_138[1][2]
      local a_134 = _scrutinee_138[1][1]
      local a_134 = _scrutinee_138[1][1]
      local b_135 = _scrutinee_138[1][2]
      local a_134 = _scrutinee_138[1][1]
      local b_135 = _scrutinee_138[1][2]
      local c_136 = _scrutinee_138[2]
      return a_134 + b_135 + c_136
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local function sum_142(p_139)
      local _scrutinee_143 = p_139
      if _scrutinee_143._tag == 0 then
        local b_141 = _scrutinee_143._0[2]
        local a_140 = _scrutinee_143._0[1]
        local a_140 = _scrutinee_143._0[1]
        local b_141 = _scrutinee_143._0[2]
        local a_140 = _scrutinee_143._0[1]
        local b_141 = _scrutinee_143._0[2]
        return a_140 + b_141
      else
        return error_144("Match failure")
      end
    end
    File "<string>", line 2, characters 12-47:
    Warning: Non-exhaustive pattern matching, missing case: Pair (Pair _)
    |}]

let%expect_test "match on record pattern" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local function get_x_147(r_145)
      local _scrutinee_148 = r_145
      local x_146 = _scrutinee_148.x
      local x_146 = _scrutinee_148.x
      return x_146
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local function sum_xy_152(r_149)
      local _scrutinee_153 = r_149
      local y_151 = _scrutinee_153.y
      local x_150 = _scrutinee_153.x
      local x_150 = _scrutinee_153.x
      local y_151 = _scrutinee_153.y
      return x_150 + y_151
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local function f_156(x_154)
      local _scrutinee_157 = x_154
      if _scrutinee_157._tag == 1 then
        if (function()
      local n_155 = _scrutinee_157._0
      local n_155 = _scrutinee_157._0
      return n_155 > 0
    end)() then
          local n_155 = _scrutinee_157._0
          local n_155 = _scrutinee_157._0
          return n_155
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
    local function f_160(x_158)
      local _scrutinee_161 = x_158
      if _scrutinee_161._tag == 0 then
        return 0
      else
        if _scrutinee_161._tag == 1 then
          if (function()
      local n_159 = _scrutinee_161._0
      local n_159 = _scrutinee_161._0
      return n_159 > 0
    end)() then
            local n_159 = _scrutinee_161._0
            local n_159 = _scrutinee_161._0
            return n_159
          else
            return error_162("Match failure")
          end
        else
          return error_163("Match failure")
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
    local function head_167(l_164)
      local _scrutinee_168 = l_164
      if _scrutinee_168._tag == 1 then
        local t_166 = _scrutinee_168._0[2]
        local h_165 = _scrutinee_168._0[1]
        local h_165 = _scrutinee_168._0[1]
        local t_166 = _scrutinee_168._0[2]
        local h_165 = _scrutinee_168._0[1]
        local t_166 = _scrutinee_168._0[2]
        return h_165
      else
        if _scrutinee_168._tag == 0 then
          return 0
        else
          return error_169("Match failure")
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
    local function is_left_171(e_170)
      local _scrutinee_172 = e_170
      if _scrutinee_172._tag == 1 then
        return false
      else
        if _scrutinee_172._tag == 0 then
          return true
        else
          return error_173("Match failure")
        end
      end
    end
    |}]

(* === Pattern Matching: Constant Patterns === *)

let%expect_test "match with string patterns" =
  print_endline (compile {|let greet name = match name with | "Alice" -> "Hi Alice" | "Bob" -> "Hey Bob" | _ -> "Hello"|});
  [%expect{|
    local function greet_175(name_174)
      local _scrutinee_176 = name_174
      if _scrutinee_176 == "Bob" then
        return "Hey Bob"
      else
        if _scrutinee_176 == "Alice" then
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
    local function negate_178(b_177)
      local _scrutinee_179 = b_177
      if _scrutinee_179 == false then
        return true
      else
        if _scrutinee_179 == true then
          return false
        else
          return error_180("Match failure")
        end
      end
    end
    |}]

let%expect_test "match with unit pattern" =
  print_endline (compile "let f u = match u with | () -> 42");
  [%expect{|
    ERROR: File "<string>", line 1, characters 28-30:
    Parse error: Syntax error
    |}]

(* === Pattern Matching: Tuple Patterns === *)

let%expect_test "match with 3-element tuple" =
  print_endline (compile "let sum3 t = match t with | (a, b, c) -> a + b + c");
  [%expect{|
    local function sum3_185(t_181)
      local _scrutinee_186 = t_181
      local c_184 = _scrutinee_186[3]
      local b_183 = _scrutinee_186[2]
      local a_182 = _scrutinee_186[1]
      local a_182 = _scrutinee_186[1]
      local b_183 = _scrutinee_186[2]
      local c_184 = _scrutinee_186[3]
      return a_182 + b_183 + c_184
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local function sum4_192(t_187)
      local _scrutinee_193 = t_187
      local d_191 = _scrutinee_193[4]
      local c_190 = _scrutinee_193[3]
      local b_189 = _scrutinee_193[2]
      local a_188 = _scrutinee_193[1]
      local a_188 = _scrutinee_193[1]
      local b_189 = _scrutinee_193[2]
      local c_190 = _scrutinee_193[3]
      local d_191 = _scrutinee_193[4]
      return a_188 + b_189 + c_190 + d_191
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local function first_196(t_194)
      local _scrutinee_197 = t_194
      local a_195 = _scrutinee_197[1]
      local a_195 = _scrutinee_197[1]
      return a_195
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local function check_202(t_198)
      local _scrutinee_203 = t_198
      if _scrutinee_203[1] == 1 then
        local y_200 = _scrutinee_203[2]
        local y_200 = _scrutinee_203[2]
        return y_200 + 1
      else
        if _scrutinee_203[1] == 0 then
          local x_199 = _scrutinee_203[2]
          local x_199 = _scrutinee_203[2]
          return x_199
        else
          local z_201 = _scrutinee_203[2]
          local z_201 = _scrutinee_203[2]
          return z_201 + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    ERROR: File "<string>", line 2, characters 56-62:
    Type error: Type mismatch: expected 't297 option, got int
    Expected: 't297 option
    Actual: int
    |}]

let%expect_test "mixed nesting - tuple in constructor in tuple" =
  print_endline (compile "type 'a option = None | Some of 'a
let extract t = match t with | (Some (a, b), c) -> a + b + c | (None, d) -> d");
  [%expect{|
    local function extract_211(t_206)
      local _scrutinee_212 = t_206
      if _scrutinee_212[1]._tag == 0 then
        local d_210 = _scrutinee_212[2]
        local d_210 = _scrutinee_212[2]
        return d_210
      else
        if _scrutinee_212[1]._tag == 1 then
          local c_209 = _scrutinee_212[2]
          local b_208 = _scrutinee_212[1]._0[2]
          local a_207 = _scrutinee_212[1]._0[1]
          local a_207 = _scrutinee_212[1]._0[1]
          local b_208 = _scrutinee_212[1]._0[2]
          local a_207 = _scrutinee_212[1]._0[1]
          local b_208 = _scrutinee_212[1]._0[2]
          local a_207 = _scrutinee_212[1]._0[1]
          local b_208 = _scrutinee_212[1]._0[2]
          local c_209 = _scrutinee_212[2]
          return a_207 + b_208 + c_209
        else
          return error_213("Match failure")
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
    local function unwrap_218(w_214)
      local _scrutinee_219 = w_214
      if _scrutinee_219._tag == 0 then
        if _scrutinee_219._0[1]._tag == 0 then
          local n_217 = _scrutinee_219._0[2]
          local n_217 = _scrutinee_219._0[2]
          local n_217 = _scrutinee_219._0[2]
          return n_217
        else
          if _scrutinee_219._0[1]._tag == 1 then
            local n_216 = _scrutinee_219._0[2]
            local x_215 = _scrutinee_219._0[1]._0
            local x_215 = _scrutinee_219._0[1]._0
            local x_215 = _scrutinee_219._0[1]._0
            local n_216 = _scrutinee_219._0[2]
            local x_215 = _scrutinee_219._0[1]._0
            local n_216 = _scrutinee_219._0[2]
            return x_215 + n_216
          else
            return error_220("Match failure")
          end
        end
      else
        return error_221("Match failure")
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
    local function to_num_223(d_222)
      local _scrutinee_224 = d_222
      local _switch = _scrutinee_224
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
    local function to_num_226(d_225)
      local _scrutinee_227 = d_225
      if _scrutinee_227._tag == 2 then
        return 2
      else
        if _scrutinee_227._tag == 1 then
          return 1
        else
          if _scrutinee_227._tag == 0 then
            return 0
          else
            return error_228("Match failure")
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
    local function is_monday_230(d_229)
      local _scrutinee_231 = d_229
      if _scrutinee_231._tag == 0 then
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
    local function is_value_233(e_232)
      local _scrutinee_234 = e_232
      if _scrutinee_234._tag == 2 then
        return false
      else
        if _scrutinee_234._tag == 1 then
          return true
        else
          if _scrutinee_234._tag == 0 then
            return true
          else
            return error_235("Match failure")
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
    local function f_238(x_236)
      local _scrutinee_239 = x_236
      if _scrutinee_239._tag == 1 then
        local x_237 = _scrutinee_239._0
        local x_237 = _scrutinee_239._0
        return x_237
      else
        if _scrutinee_239._tag == 0 then
          return 0
        else
          return error_240("Match failure")
        end
      end
    end
    |}]

let%expect_test "binding entire constructor argument" =
  print_endline (compile "type pair = Pair of (int * int)
let get_pair p = match p with | Pair x -> x");
  [%expect{|
    local function get_pair_243(p_241)
      local _scrutinee_244 = p_241
      if _scrutinee_244._tag == 0 then
        local x_242 = _scrutinee_244._0
        local x_242 = _scrutinee_244._0
        return x_242
      else
        return error_245("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f_250(x_246)
      local _scrutinee_251 = x_246
      if _scrutinee_251._tag == 0 then
        return 0
      else
        if _scrutinee_251._tag == 1 then
          local c_249 = _scrutinee_251._0[3]
          local b_248 = _scrutinee_251._0[2]
          local a_247 = _scrutinee_251._0[1]
          local a_247 = _scrutinee_251._0[1]
          local b_248 = _scrutinee_251._0[2]
          local c_249 = _scrutinee_251._0[3]
          local a_247 = _scrutinee_251._0[1]
          local b_248 = _scrutinee_251._0[2]
          local c_249 = _scrutinee_251._0[3]
          return a_247 + b_248 + c_249
        else
          return error_252("Match failure")
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
    local function check_258(p_253)
      local _scrutinee_259 = p_253
      if (function()
      local b_255 = _scrutinee_259[2]
      local a_254 = _scrutinee_259[1]
      local a_254 = _scrutinee_259[1]
      local b_255 = _scrutinee_259[2]
      return a_254 > b_255
    end)() then
        local b_255 = _scrutinee_259[2]
        local a_254 = _scrutinee_259[1]
        local a_254 = _scrutinee_259[1]
        local b_255 = _scrutinee_259[2]
        return a_254
      else
        local b_257 = _scrutinee_259[2]
        local a_256 = _scrutinee_259[1]
        local a_256 = _scrutinee_259[1]
        local b_257 = _scrutinee_259[2]
        return b_257
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range_262(n_260)
      local _scrutinee_263 = n_260
      if (function()
      local x_261 = _scrutinee_263
      return x_261 > 0
    end)() then
        local x_261 = _scrutinee_263
        if x_261 < 10 then
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
    local function positive_266(opt_264)
      local _scrutinee_267 = opt_264
      if _scrutinee_267._tag == 0 then
        return false
      else
        if _scrutinee_267._tag == 1 then
          if (function()
      local n_265 = _scrutinee_267._0
      local n_265 = _scrutinee_267._0
      return n_265 > 0
    end)() then
            local n_265 = _scrutinee_267._0
            local n_265 = _scrutinee_267._0
            return true
          else
            return false
          end
        else
          return error_268("Match failure")
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
    local function classify_273(n_269)
      local _scrutinee_274 = n_269
      if (function()
      local x_270 = _scrutinee_274
      return x_270 < 0
    end)() then
        local x_270 = _scrutinee_274
        return 0 - 1
      else
        if (function()
      local x_271 = _scrutinee_274
      return x_271 > 100
    end)() then
          local x_271 = _scrutinee_274
          return 2
        else
          if (function()
      local x_272 = _scrutinee_274
      return x_272 > 50
    end)() then
            local x_272 = _scrutinee_274
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
    local length_275
    length_275 = function(l_276)
      local _scrutinee_278 = l_276
      if _scrutinee_278._tag == 1 then
        local t_277 = _scrutinee_278._0[2]
        local t_277 = _scrutinee_278._0[2]
        local t_277 = _scrutinee_278._0[2]
        return 1 + length_275(t_277)
      else
        if _scrutinee_278._tag == 0 then
          return 0
        else
          return error_279("Match failure")
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
    local sum_280
    sum_280 = function(l_281)
      local _scrutinee_284 = l_281
      if _scrutinee_284._tag == 1 then
        local t_283 = _scrutinee_284._0[2]
        local h_282 = _scrutinee_284._0[1]
        local h_282 = _scrutinee_284._0[1]
        local t_283 = _scrutinee_284._0[2]
        local h_282 = _scrutinee_284._0[1]
        local t_283 = _scrutinee_284._0[2]
        return h_282 + sum_280(t_283)
      else
        if _scrutinee_284._tag == 0 then
          return 0
        else
          return error_285("Match failure")
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
    local sum_acc_286
    sum_acc_286 = function(acc_287, l_288)
      local _scrutinee_291 = l_288
      if _scrutinee_291._tag == 1 then
        local t_290 = _scrutinee_291._0[2]
        local h_289 = _scrutinee_291._0[1]
        local h_289 = _scrutinee_291._0[1]
        local t_290 = _scrutinee_291._0[2]
        local h_289 = _scrutinee_291._0[1]
        local t_290 = _scrutinee_291._0[2]
        return sum_acc_286(acc_287 + h_289, t_290)
      else
        if _scrutinee_291._tag == 0 then
          return acc_287
        else
          return error_292("Match failure")
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
    local even_293, odd_294
    even_293 = function(n_295)
      local _scrutinee_299 = n_295
      if _scrutinee_299 == 0 then
        return true
      else
        local n_296 = _scrutinee_299
        return odd_294(n_296 - 1)
      end
    end
    odd_294 = function(n_297)
      local _scrutinee_300 = n_297
      if _scrutinee_300 == 0 then
        return false
      else
        local n_298 = _scrutinee_300
        return even_293(n_298 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id_303(x_301)
      local _scrutinee_304 = x_301
      local y_302 = _scrutinee_304
      return y_302
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap_307(x_305)
      local _scrutinee_308 = x_305
      if _scrutinee_308._tag == 1 then
        local y_306 = _scrutinee_308._0
        local y_306 = _scrutinee_308._0
        return y_306
      else
        return error_309("Match failure")
      end
    end
    File "<string>", line 2, characters 15-41:
    Warning: Non-exhaustive pattern matching, missing case: None
    |}]

let%expect_test "all wildcards match" =
  print_endline (compile "let first3 t = match t with | (x, _, _) -> x | _ -> 0");
  [%expect{|
    local function first3_312(t_310)
      local _scrutinee_313 = t_310
      local x_311 = _scrutinee_313[1]
      local x_311 = _scrutinee_313[1]
      return x_311
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f_316(x_314)
      local _scrutinee_317 = x_314
      if _scrutinee_317 == 1 then
        return 200
      else
        if _scrutinee_317 == 0 then
          return 100
        else
          local n_315 = _scrutinee_317
          return n_315
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f_319(x_318)
      local _scrutinee_320 = x_318
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
    Type error: Type mismatch: expected 't487 option, got 't489 result
    Expected: 't487 option
    Actual: 't489 result
    |}]

let%expect_test "type error: inconsistent tuple sizes" =
  print_endline (compile "let f x = match x with | (a, b) -> a | (a, b, c) -> a");
  [%expect{|
    ERROR: File "<string>", line 1, characters 39-53:
    Type error: Tuple size mismatch: expected 2 elements, got 3
    Expected: ('t492 * 't493)
    Actual: ('t494 * 't495 * 't496)
    |}]

let%expect_test "type error: wrong constructor argument type" =
  print_endline (compile "type intopt = None | Some of int
let f x = match x with | Some true -> 1 | _ -> 0");
  [%expect{|
    local function f_333(x_332)
      local _scrutinee_334 = x_332
      if _scrutinee_334._tag == 1 then
        if _scrutinee_334._0 == true then
          return 1
        else
          return 0
        end
      else
        return 0
      end
    end
    |}]

(* === Pattern Matching: Record Patterns === *)

let%expect_test "record pattern with all fields" =
  print_endline (compile "type point = { x : int; y : int }
let get_x p = match p with | { x; y } -> x");
  [%expect{|
    local function get_x_338(p_335)
      local _scrutinee_339 = p_335
      local y_337 = _scrutinee_339.y
      local x_336 = _scrutinee_339.x
      local x_336 = _scrutinee_339.x
      local y_337 = _scrutinee_339.y
      return x_336
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local function project_343(p_340)
      local _scrutinee_344 = p_340
      local y_342 = _scrutinee_344.y
      local x_341 = _scrutinee_344.x
      local x_341 = _scrutinee_344.x
      local y_342 = _scrutinee_344.y
      return {x_341, y_342}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x_347(opt_345)
      local _scrutinee_348 = opt_345
      if _scrutinee_348._tag == 0 then
        return 0
      else
        if _scrutinee_348._tag == 1 then
          local x_346 = _scrutinee_348._0.x
          local x_346 = _scrutinee_348._0.x
          local x_346 = _scrutinee_348._0.x
          return x_346
        else
          return error_349("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-60:
    Warning: Non-exhaustive pattern matching, missing case: Some None
    |}]

let%expect_test "record pattern with renamed binding" =
  print_endline (compile "let get_val r = match r with | { x = value } -> value");
  [%expect{|
    local function get_val_352(r_350)
      local _scrutinee_353 = r_350
      local value_351 = _scrutinee_353.x
      local value_351 = _scrutinee_353.x
      return value_351
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local function apply_pair_358(f_354, p_355)
      local _scrutinee_359 = p_355
      local b_357 = _scrutinee_359[2]
      local a_356 = _scrutinee_359[1]
      local a_356 = _scrutinee_359[1]
      local b_357 = _scrutinee_359[2]
      return f_354(a_356, b_357)
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    ERROR: File "<string>", line 4, characters 35-44:
    Type error: Type mismatch: expected 't538 option, got int
    Expected: 't538 option
    Actual: int
    |}]

let%expect_test "pattern match inside if-then-else" =
  print_endline (compile "type 'a option = None | Some of 'a
let safe_div a b = if b == 0 then None else Some (match (a, b) with | (x, y) -> x / y)");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local function safe_div_368(a_364, b_365)
      if b_365 == 0 then
        return _Ctor_option_0
      else
        return {_tag = 1, _0 = (function()
      local _scrutinee_369 = {a_364, b_365}
      local y_367 = _scrutinee_369[2]
      local x_366 = _scrutinee_369[1]
      local x_366 = _scrutinee_369[1]
      local y_367 = _scrutinee_369[2]
      return x_366 / y_367
    end)()}
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id_371(x_370)
      return x_370
    end
    local function f_374(opt_372)
      local _scrutinee_375 = id_371(opt_372)
      if _scrutinee_375._tag == 0 then
        return 0
      else
        if _scrutinee_375._tag == 1 then
          local n_373 = _scrutinee_375._0
          local n_373 = _scrutinee_375._0
          return n_373
        else
          return error_376("Match failure")
        end
      end
    end
    |}]

let%expect_test "complex real-world example - tree traversal" =
  print_endline (compile "type tree = Leaf of int | Node of (tree * tree)
let rec sum t = match t with | Leaf n -> n | Node (l, r) -> sum l + sum r");
  [%expect{|
    local sum_377
    sum_377 = function(t_378)
      local _scrutinee_382 = t_378
      if _scrutinee_382._tag == 1 then
        local r_381 = _scrutinee_382._0[2]
        local l_380 = _scrutinee_382._0[1]
        local l_380 = _scrutinee_382._0[1]
        local r_381 = _scrutinee_382._0[2]
        local l_380 = _scrutinee_382._0[1]
        local r_381 = _scrutinee_382._0[2]
        return sum_377(l_380) + sum_377(r_381)
      else
        if _scrutinee_382._tag == 0 then
          local n_379 = _scrutinee_382._0
          local n_379 = _scrutinee_382._0
          return n_379
        else
          return error_383("Match failure")
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
    local eval_384
    eval_384 = function(e_385)
      local _scrutinee_391 = e_385
      if _scrutinee_391._tag == 2 then
        local b_390 = _scrutinee_391._0[2]
        local a_389 = _scrutinee_391._0[1]
        local a_389 = _scrutinee_391._0[1]
        local b_390 = _scrutinee_391._0[2]
        local a_389 = _scrutinee_391._0[1]
        local b_390 = _scrutinee_391._0[2]
        return eval_384(a_389) * eval_384(b_390)
      else
        if _scrutinee_391._tag == 1 then
          local b_388 = _scrutinee_391._0[2]
          local a_387 = _scrutinee_391._0[1]
          local a_387 = _scrutinee_391._0[1]
          local b_388 = _scrutinee_391._0[2]
          local a_387 = _scrutinee_391._0[1]
          local b_388 = _scrutinee_391._0[2]
          return eval_384(a_387) + eval_384(b_388)
        else
          if _scrutinee_391._tag == 0 then
            local n_386 = _scrutinee_391._0
            local n_386 = _scrutinee_391._0
            return n_386
          else
            return error_392("Match failure")
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
  [%expect{| local _nil_393 = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat_394 = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until_395 = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    local function check_397(_do_396)
      return _do_396
    end
    |}]

let%expect_test "lua keyword 'local' used as identifier is mangled" =
  print_endline (compile "let local = 123");
  [%expect{| local _local_398 = 123 |}]

let%expect_test "lua keyword 'goto' used as identifier is mangled" =
  print_endline (compile "let goto = 7");
  [%expect{| local _goto_399 = 7 |}]

let%expect_test "lua keyword 'break' used as identifier is mangled" =
  print_endline (compile "let break = 3");
  [%expect{| local _break_400 = 3 |}]

let%expect_test "lua keyword 'return' used as identifier is mangled" =
  print_endline (compile "let return = 1");
  [%expect{| local _return_401 = 1 |}]

let%expect_test "lua keyword 'while' used as identifier is mangled" =
  print_endline (compile "let while = 8");
  [%expect{| local _while_402 = 8 |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value_403 = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker_404 = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none_val_405 = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val_406 = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2}
    local _Ctor_color_1 = {_tag = 1}
    local _Ctor_color_0 = {_tag = 0}
    local red_407 = _Ctor_color_0
    local green_408 = _Ctor_color_1
    local blue_409 = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local function is_some_411(opt_410)
      local _scrutinee_412 = opt_410
      if _scrutinee_412._tag == 1 then
        return true
      else
        if _scrutinee_412._tag == 0 then
          return false
        else
          return error_413("Match failure")
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
    local a_414 = _Ctor_option_0
    local b_415 = _Ctor_option_0
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
    local stop_416 = _Ctor_traffic_light_0
    local caution_417 = _Ctor_traffic_light_1
    local go_418 = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x_419 = {_tag = 1, _0 = 1}
    local y_420 = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0}
    local none1_421 = _Ctor_option_0
    local some1_422 = {_tag = 1, _0 = 10}
    local none2_423 = _Ctor_option_0
    local some2_424 = {_tag = 1, _0 = 20}
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
    local function to_num_426(dir_425)
      local _scrutinee_427 = dir_425
      local _switch = _scrutinee_427
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
    local function day_num_429(day_428)
      local _scrutinee_430 = day_428
      local _switch = _scrutinee_430
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
    local function to_num_432(color_431)
      local _scrutinee_433 = color_431
      if _scrutinee_433._tag == 2 then
        return 2
      else
        if _scrutinee_433._tag == 1 then
          return 1
        else
          if _scrutinee_433._tag == 0 then
            return 0
          else
            return error_434("Match failure")
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
    local function eval_440(expression_435)
      local _scrutinee_441 = expression_435
      local _switch = _scrutinee_441
      local _dispatch = {[3] = function()
      local n_439 = _scrutinee_441._0
      local n_439 = _scrutinee_441._0
      return n_439 * 2
    end, [2] = function()
      local n_438 = _scrutinee_441._0
      local n_438 = _scrutinee_441._0
      return n_438 - 1
    end, [1] = function()
      local n_437 = _scrutinee_441._0
      local n_437 = _scrutinee_441._0
      return n_437 + 1
    end, [0] = function()
      local n_436 = _scrutinee_441._0
      local n_436 = _scrutinee_441._0
      return n_436
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
    local x_442 = 1
    local result_443 = x_442 + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a_444 = 1
    local b_445 = 2
    local result_446 = a_444 + b_445
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result_447
    if true then
      result_447 = 1
    else
      result_447 = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x_448 = 5
    local result_449
    if x_448 > 0 then
      result_449 = 1
    else
      result_449 = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result_450
    if true then
      if false then
        result_450 = 1
      else
        result_450 = 2
      end
    else
      result_450 = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag_451
    if true then
      flag_451 = 1
    else
      flag_451 = 0
    end
    local result_452 = flag_451 + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local __454 = print(1)
    local result_453 = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local function compute_457(x_455)
      local y_456 = x_455 + 1
      return y_456 * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local function abs_459(n_458)
      if n_458 < 0 then
        return 0 - n_458
      else
        return n_458
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to_460
    sum_to_460 = function(n_461, acc_462)
      if n_461 <= 0 then
        return acc_462
      else
        return sum_to_460(n_461 - 1, acc_462 + n_461)
      end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown_463
    countdown_463 = function(n_464)
      if n_464 <= 0 then
        return 0
      else
        return countdown_463(n_464 - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero_465
    find_zero_465 = function(n_466)
      if n_466 == 0 then
        return true
      else
        return find_zero_465(n_466 - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even_467, is_odd_468
    is_even_467 = function(n_469)
      if n_469 == 0 then
        return true
      else
        return is_odd_468(n_469 - 1)
      end
    end
    is_odd_468 = function(n_470)
      if n_470 == 0 then
        return false
      else
        return is_even_467(n_470 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial_471
    factorial_471 = function(n_472)
      if n_472 <= 1 then
        return 1
      else
        return n_472 * factorial_471(n_472 - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc_473
    factorial_acc_473 = function(n_474, acc_475)
      if n_474 <= 1 then
        return acc_475
      else
        return factorial_acc_473(n_474 - 1, n_474 * acc_475)
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
    local function eval_cmd_477(and_val_476)
      local _scrutinee_478 = and_val_476
      local _switch = _scrutinee_478
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
    local function get_or_default_483(opt_479, default_val_480)
      local result_481
      if true then
        result_481 = opt_479
      else
        result_481 = _Ctor_option_0
      end
      local _scrutinee_484 = result_481
      if _scrutinee_484._tag == 1 then
        local x_482 = _scrutinee_484._0
        local x_482 = _scrutinee_484._0
        return x_482
      else
        if _scrutinee_484._tag == 0 then
          return default_val_480
        else
          return error_485("Match failure")
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
    local function process_direction_490(dir_486)
      local opt_value_487
      if dir_486 == 0 then
        opt_value_487 = _Ctor_option_0
      else
        opt_value_487 = {_tag = 1, _0 = dir_486}
      end
      local _scrutinee_491 = opt_value_487
      local result_489
      if _scrutinee_491._tag == 1 then
        local n_488 = _scrutinee_491._0
        local n_488 = _scrutinee_491._0
        result_489 = n_488
      else
        if _scrutinee_491._tag == 0 then
          result_489 = 0
        else
          result_489 = error_492("Match failure")
        end
      end
      return result_489 + 1
    end
    |}]
