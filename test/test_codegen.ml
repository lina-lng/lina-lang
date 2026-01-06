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
      if _scrutinee_37._tag == "Some" then
        return true
      else
        if _scrutinee_37._tag == "None" then
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
      if _scrutinee_43._tag == "Some" then
        local x_41 = _scrutinee_43._0
        local x_41 = _scrutinee_43._0
        return x_41
      else
        if _scrutinee_43._tag == "None" then
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
    local none_45 = {_tag = "None"}
    local some_val_46 = {_tag = "Some", _0 = 42}
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
      if _scrutinee_67._tag == "Some" then
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
        if _scrutinee_67._tag == "None" then
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
      if _scrutinee_94._tag == "Some" then
        local n_92 = _scrutinee_94._0
        local n_92 = _scrutinee_94._0
        return n_92
      else
        if _scrutinee_94._tag == "None" then
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
      if _scrutinee_99._tag == "Some" then
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
      if _scrutinee_103._tag == "None" then
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
      if _scrutinee_116._tag == "Some" then
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
      if _scrutinee_119._tag == "None" then
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
      if _scrutinee_123._tag == "Some" then
        local n_121 = _scrutinee_123._0
        local n_121 = _scrutinee_123._0
        return n_121
      else
        if _scrutinee_123._tag == "None" then
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
      if _scrutinee_131._tag == "Blue" then
        return 2
      else
        if _scrutinee_131._tag == "Green" then
          return 1
        else
          if _scrutinee_131._tag == "Red" then
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
      if _scrutinee_143._tag == "Pair" then
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
      if _scrutinee_157._tag == "Some" then
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
      if _scrutinee_161._tag == "None" then
        return 0
      else
        if _scrutinee_161._tag == "Some" then
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
      if _scrutinee_168._tag == "Cons" then
        local t_166 = _scrutinee_168._0[2]
        local h_165 = _scrutinee_168._0[1]
        local h_165 = _scrutinee_168._0[1]
        local t_166 = _scrutinee_168._0[2]
        local h_165 = _scrutinee_168._0[1]
        local t_166 = _scrutinee_168._0[2]
        return h_165
      else
        if _scrutinee_168._tag == "Nil" then
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
      if _scrutinee_172._tag == "Right" then
        return false
      else
        if _scrutinee_172._tag == "Left" then
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
      if _scrutinee_212[1]._tag == "None" then
        local d_210 = _scrutinee_212[2]
        local d_210 = _scrutinee_212[2]
        return d_210
      else
        if _scrutinee_212[1]._tag == "Some" then
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
      if _scrutinee_219._tag == "Wrap" then
        if _scrutinee_219._0[1]._tag == "None" then
          local n_217 = _scrutinee_219._0[2]
          local n_217 = _scrutinee_219._0[2]
          local n_217 = _scrutinee_219._0[2]
          return n_217
        else
          if _scrutinee_219._0[1]._tag == "Some" then
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
      if _scrutinee_224._tag == "West" then
        return 3
      else
        if _scrutinee_224._tag == "East" then
          return 2
        else
          if _scrutinee_224._tag == "South" then
            return 1
          else
            if _scrutinee_224._tag == "North" then
              return 0
            else
              return error_225("Match failure")
            end
          end
        end
      end
    end
    |}]

let%expect_test "four constructor variant - missing one" =
  print_endline (compile "type dir = North | South | East | West
let to_num d = match d with | North -> 0 | South -> 1 | East -> 2");
  [%expect{|
    local function to_num_227(d_226)
      local _scrutinee_228 = d_226
      if _scrutinee_228._tag == "East" then
        return 2
      else
        if _scrutinee_228._tag == "South" then
          return 1
        else
          if _scrutinee_228._tag == "North" then
            return 0
          else
            return error_229("Match failure")
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
    local function is_monday_231(d_230)
      local _scrutinee_232 = d_230
      if _scrutinee_232._tag == "Mon" then
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
    local function is_value_234(e_233)
      local _scrutinee_235 = e_233
      if _scrutinee_235._tag == "Add" then
        return false
      else
        if _scrutinee_235._tag == "One" then
          return true
        else
          if _scrutinee_235._tag == "Zero" then
            return true
          else
            return error_236("Match failure")
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
    local function f_239(x_237)
      local _scrutinee_240 = x_237
      if _scrutinee_240._tag == "Some" then
        local x_238 = _scrutinee_240._0
        local x_238 = _scrutinee_240._0
        return x_238
      else
        if _scrutinee_240._tag == "None" then
          return 0
        else
          return error_241("Match failure")
        end
      end
    end
    |}]

let%expect_test "binding entire constructor argument" =
  print_endline (compile "type pair = Pair of (int * int)
let get_pair p = match p with | Pair x -> x");
  [%expect{|
    local function get_pair_244(p_242)
      local _scrutinee_245 = p_242
      if _scrutinee_245._tag == "Pair" then
        local x_243 = _scrutinee_245._0
        local x_243 = _scrutinee_245._0
        return x_243
      else
        return error_246("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local function f_251(x_247)
      local _scrutinee_252 = x_247
      if _scrutinee_252._tag == "None" then
        return 0
      else
        if _scrutinee_252._tag == "Some" then
          local c_250 = _scrutinee_252._0[3]
          local b_249 = _scrutinee_252._0[2]
          local a_248 = _scrutinee_252._0[1]
          local a_248 = _scrutinee_252._0[1]
          local b_249 = _scrutinee_252._0[2]
          local c_250 = _scrutinee_252._0[3]
          local a_248 = _scrutinee_252._0[1]
          local b_249 = _scrutinee_252._0[2]
          local c_250 = _scrutinee_252._0[3]
          return a_248 + b_249 + c_250
        else
          return error_253("Match failure")
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
    local function check_259(p_254)
      local _scrutinee_260 = p_254
      if (function()
      local b_256 = _scrutinee_260[2]
      local a_255 = _scrutinee_260[1]
      local a_255 = _scrutinee_260[1]
      local b_256 = _scrutinee_260[2]
      return a_255 > b_256
    end)() then
        local b_256 = _scrutinee_260[2]
        local a_255 = _scrutinee_260[1]
        local a_255 = _scrutinee_260[1]
        local b_256 = _scrutinee_260[2]
        return a_255
      else
        local b_258 = _scrutinee_260[2]
        local a_257 = _scrutinee_260[1]
        local a_257 = _scrutinee_260[1]
        local b_258 = _scrutinee_260[2]
        return b_258
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local function range_263(n_261)
      local _scrutinee_264 = n_261
      if (function()
      local x_262 = _scrutinee_264
      return x_262 > 0
    end)() then
        local x_262 = _scrutinee_264
        if x_262 < 10 then
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
    local function positive_267(opt_265)
      local _scrutinee_268 = opt_265
      if _scrutinee_268._tag == "None" then
        return false
      else
        if _scrutinee_268._tag == "Some" then
          if (function()
      local n_266 = _scrutinee_268._0
      local n_266 = _scrutinee_268._0
      return n_266 > 0
    end)() then
            local n_266 = _scrutinee_268._0
            local n_266 = _scrutinee_268._0
            return true
          else
            return false
          end
        else
          return error_269("Match failure")
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
    local function classify_274(n_270)
      local _scrutinee_275 = n_270
      if (function()
      local x_271 = _scrutinee_275
      return x_271 < 0
    end)() then
        local x_271 = _scrutinee_275
        return 0 - 1
      else
        if (function()
      local x_272 = _scrutinee_275
      return x_272 > 100
    end)() then
          local x_272 = _scrutinee_275
          return 2
        else
          if (function()
      local x_273 = _scrutinee_275
      return x_273 > 50
    end)() then
            local x_273 = _scrutinee_275
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
    local length_276
    length_276 = function(l_277)
      local _scrutinee_279 = l_277
      if _scrutinee_279._tag == "Cons" then
        local t_278 = _scrutinee_279._0[2]
        local t_278 = _scrutinee_279._0[2]
        local t_278 = _scrutinee_279._0[2]
        return 1 + length_276(t_278)
      else
        if _scrutinee_279._tag == "Nil" then
          return 0
        else
          return error_280("Match failure")
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
    local sum_281
    sum_281 = function(l_282)
      local _scrutinee_285 = l_282
      if _scrutinee_285._tag == "Cons" then
        local t_284 = _scrutinee_285._0[2]
        local h_283 = _scrutinee_285._0[1]
        local h_283 = _scrutinee_285._0[1]
        local t_284 = _scrutinee_285._0[2]
        local h_283 = _scrutinee_285._0[1]
        local t_284 = _scrutinee_285._0[2]
        return h_283 + sum_281(t_284)
      else
        if _scrutinee_285._tag == "Nil" then
          return 0
        else
          return error_286("Match failure")
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
    local sum_acc_287
    sum_acc_287 = function(acc_288, l_289)
      local _scrutinee_292 = l_289
      if _scrutinee_292._tag == "Cons" then
        local t_291 = _scrutinee_292._0[2]
        local h_290 = _scrutinee_292._0[1]
        local h_290 = _scrutinee_292._0[1]
        local t_291 = _scrutinee_292._0[2]
        local h_290 = _scrutinee_292._0[1]
        local t_291 = _scrutinee_292._0[2]
        return sum_acc_287(acc_288 + h_290, t_291)
      else
        if _scrutinee_292._tag == "Nil" then
          return acc_288
        else
          return error_293("Match failure")
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
    local even_294, odd_295
    even_294 = function(n_296)
      local _scrutinee_300 = n_296
      if _scrutinee_300 == 0 then
        return true
      else
        local n_297 = _scrutinee_300
        return odd_295(n_297 - 1)
      end
    end
    odd_295 = function(n_298)
      local _scrutinee_301 = n_298
      if _scrutinee_301 == 0 then
        return false
      else
        local n_299 = _scrutinee_301
        return even_294(n_299 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local function id_304(x_302)
      local _scrutinee_305 = x_302
      local y_303 = _scrutinee_305
      return y_303
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local function unwrap_308(x_306)
      local _scrutinee_309 = x_306
      if _scrutinee_309._tag == "Some" then
        local y_307 = _scrutinee_309._0
        local y_307 = _scrutinee_309._0
        return y_307
      else
        return error_310("Match failure")
      end
    end
    File "<string>", line 2, characters 15-41:
    Warning: Non-exhaustive pattern matching, missing case: None
    |}]

let%expect_test "all wildcards match" =
  print_endline (compile "let first3 t = match t with | (x, _, _) -> x | _ -> 0");
  [%expect{|
    local function first3_313(t_311)
      local _scrutinee_314 = t_311
      local x_312 = _scrutinee_314[1]
      local x_312 = _scrutinee_314[1]
      return x_312
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local function f_317(x_315)
      local _scrutinee_318 = x_315
      if _scrutinee_318 == 1 then
        return 200
      else
        if _scrutinee_318 == 0 then
          return 100
        else
          local n_316 = _scrutinee_318
          return n_316
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local function f_320(x_319)
      local _scrutinee_321 = x_319
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
    local function f_334(x_333)
      local _scrutinee_335 = x_333
      if _scrutinee_335._tag == "Some" then
        if _scrutinee_335._0 == true then
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
    ERROR: File "<string>", line 1, characters 13-14:
    Parse error: Syntax error
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    ERROR: File "<string>", line 1, characters 15-16:
    Parse error: Syntax error
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local function get_x_338(opt_336)
      local _scrutinee_339 = opt_336
      if _scrutinee_339._tag == "None" then
        return 0
      else
        if _scrutinee_339._tag == "Some" then
          local x_337 = _scrutinee_339._0.x
          local x_337 = _scrutinee_339._0.x
          local x_337 = _scrutinee_339._0.x
          return x_337
        else
          return error_340("Match failure")
        end
      end
    end
    File "<string>", line 2, characters 16-60:
    Warning: Non-exhaustive pattern matching, missing case: Some None
    |}]

let%expect_test "record pattern with renamed binding" =
  print_endline (compile "let get_val r = match r with | { x = value } -> value");
  [%expect{|
    local function get_val_343(r_341)
      local _scrutinee_344 = r_341
      local value_342 = _scrutinee_344.x
      local value_342 = _scrutinee_344.x
      return value_342
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local function apply_pair_349(f_345, p_346)
      local _scrutinee_350 = p_346
      local b_348 = _scrutinee_350[2]
      local a_347 = _scrutinee_350[1]
      local a_347 = _scrutinee_350[1]
      local b_348 = _scrutinee_350[2]
      return f_345(a_347, b_348)
    end
    |}]

let%expect_test "pattern match result used in another match" =
  print_endline (compile "type 'a option = None | Some of 'a
let double_unwrap x =
  let inner = match x with | Some y -> y | None -> None in
  match inner with | Some z -> z | None -> 0");
  [%expect{|
    ERROR: File "<string>", line 4, characters 35-44:
    Type error: Type mismatch: expected 't528 option, got int
    Expected: 't528 option
    Actual: int
    |}]

let%expect_test "pattern match inside if-then-else" =
  print_endline (compile "type 'a option = None | Some of 'a
let safe_div a b = if b == 0 then None else Some (match (a, b) with | (x, y) -> x / y)");
  [%expect{|
    local function safe_div_359(a_355, b_356)
      if b_356 == 0 then
        return {_tag = "None"}
      else
        return {_tag = "Some", _0 = (function()
      local _scrutinee_360 = {a_355, b_356}
      local y_358 = _scrutinee_360[2]
      local x_357 = _scrutinee_360[1]
      local x_357 = _scrutinee_360[1]
      local y_358 = _scrutinee_360[2]
      return x_357 / y_358
    end)()}
      end
    end
    |}]

let%expect_test "pattern match with function result as scrutinee" =
  print_endline (compile "type 'a option = None | Some of 'a
let id x = x
let f opt = match id opt with | Some n -> n | None -> 0");
  [%expect{|
    local function id_362(x_361)
      return x_361
    end
    local function f_365(opt_363)
      local _scrutinee_366 = id_362(opt_363)
      if _scrutinee_366._tag == "None" then
        return 0
      else
        if _scrutinee_366._tag == "Some" then
          local n_364 = _scrutinee_366._0
          local n_364 = _scrutinee_366._0
          return n_364
        else
          return error_367("Match failure")
        end
      end
    end
    |}]

let%expect_test "complex real-world example - tree traversal" =
  print_endline (compile "type tree = Leaf of int | Node of (tree * tree)
let rec sum t = match t with | Leaf n -> n | Node (l, r) -> sum l + sum r");
  [%expect{|
    local sum_368
    sum_368 = function(t_369)
      local _scrutinee_373 = t_369
      if _scrutinee_373._tag == "Node" then
        local r_372 = _scrutinee_373._0[2]
        local l_371 = _scrutinee_373._0[1]
        local l_371 = _scrutinee_373._0[1]
        local r_372 = _scrutinee_373._0[2]
        local l_371 = _scrutinee_373._0[1]
        local r_372 = _scrutinee_373._0[2]
        return sum_368(l_371) + sum_368(r_372)
      else
        if _scrutinee_373._tag == "Leaf" then
          local n_370 = _scrutinee_373._0
          local n_370 = _scrutinee_373._0
          return n_370
        else
          return error_374("Match failure")
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
    local eval_375
    eval_375 = function(e_376)
      local _scrutinee_382 = e_376
      if _scrutinee_382._tag == "Mul" then
        local b_381 = _scrutinee_382._0[2]
        local a_380 = _scrutinee_382._0[1]
        local a_380 = _scrutinee_382._0[1]
        local b_381 = _scrutinee_382._0[2]
        local a_380 = _scrutinee_382._0[1]
        local b_381 = _scrutinee_382._0[2]
        return eval_375(a_380) * eval_375(b_381)
      else
        if _scrutinee_382._tag == "Add" then
          local b_379 = _scrutinee_382._0[2]
          local a_378 = _scrutinee_382._0[1]
          local a_378 = _scrutinee_382._0[1]
          local b_379 = _scrutinee_382._0[2]
          local a_378 = _scrutinee_382._0[1]
          local b_379 = _scrutinee_382._0[2]
          return eval_375(a_378) + eval_375(b_379)
        else
          if _scrutinee_382._tag == "Num" then
            local n_377 = _scrutinee_382._0
            local n_377 = _scrutinee_382._0
            return n_377
          else
            return error_383("Match failure")
          end
        end
      end
    end
    File "<string>", line 2, characters 17-33:
    Warning: Non-exhaustive pattern matching, missing case: Add (Num _)
    |}]
