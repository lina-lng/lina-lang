let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "record literal generates lua table" =
  print_endline (compile "let r = { x = 1; y = 2 }");
  [%expect{| local r_15 = {x = 1, y = 2} |}]

let%expect_test "record field access generates dot notation" =
  print_endline (compile "let r = { x = 42 }
let v = r.x");
  [%expect{|
    local r_16 = {x = 42};
    local v_17 = r_16.x
    |}]

let%expect_test "record update generates shallow copy" =
  print_endline (compile "let r = { x = 1; y = 2 }
let r2 = { r with x = 10 }");
  [%expect{|
    local r_18 = {x = 1, y = 2};
    local r2_19 = (function()
      local _result = {};
      for _k, _v in pairs(r_18) do
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
    local x_20 = 1;
    local y_21 = 2;
    local r_22 = {x = x_20, y = y_21}
    |}]

let%expect_test "nested record access" =
  print_endline (compile "let outer = { inner = { value = 42 } }
let v = outer.inner.value");
  [%expect{|
    local outer_23 = {inner = {value = 42}};
    local v_24 = outer_23.inner.value
    |}]

let%expect_test "match with integer patterns" =
  print_endline (compile "let f x = match x with | 0 -> 1 | 1 -> 2 | n -> n");
  [%expect{|
    local f_27 = function(x_25)
      local _scrutinee_28 = x_25;
      if _scrutinee_28 == 1 then
        return 2
      else
        if _scrutinee_28 == 0 then
          return 1
        else
          local n_26 = _scrutinee_28;
          return n_26
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local sum_pair_32 = function(p_29)
      local _scrutinee_33 = p_29;
      local b_31 = _scrutinee_33[2];
      local a_30 = _scrutinee_33[1];
      return a_30 + b_31
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local abs_37 = function(n_34)
      local _scrutinee_38 = n_34;
      if (function()
      local x_35 = _scrutinee_38;
      return x_35 < 0
    end)() then
        local x_35 = _scrutinee_38;
        return 0 - x_35
      else
        local x_36 = _scrutinee_38;
        return x_36
      end
    end
    File "<string>", line 1, characters 51-57:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local is_some_40 = function(opt_39)
      local _scrutinee_41 = opt_39;
      if _scrutinee_41._tag == 1 then
        return true
      else
        if _scrutinee_41._tag == 0 then
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
    local get_or_default_45 = function(opt_42)
      return function(default_43)
      local _scrutinee_46 = opt_42;
      if _scrutinee_46._tag == 1 then
        local x_44 = _scrutinee_46._0;
        return x_44
      else
        if _scrutinee_46._tag == 0 then
          return default_43
        else
          return error("Match failure")
        end
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
    local none_47 = _Ctor_option_0;
    local some_val_48 = {_tag = 1, _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local get_x_50 = function(r_49)
      return r_49.x
    end;
    local p1_51 = {x = 1, y = 2};
    local p2_52 = {x = 10, z = 20};
    local v1_53 = get_x_50(p1_51);
    local v2_54 = get_x_50(p2_52)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r_55 = {a = 1, b = 2, c = 3};
    local r2_56 = (function()
      local _result = {};
      for _k, _v in pairs(r_55) do
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
    local make_point_59 = function(x_57)
      return function(y_58)
      return {x = x_57, y = y_58}
    end
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local f_63 = function(p_60)
      local _tuple_64 = p_60;
      local a_61 = _tuple_64[1];
      local b_62 = _tuple_64[2];
      return a_61 + b_62
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
    local f_68 = function(opt_65)
      local _scrutinee_69 = opt_65;
      if _scrutinee_69._tag == 1 then
        local x_66 = _scrutinee_69._0;
        local _scrutinee_70 = x_66;
        if _scrutinee_70 == 0 then
          return 1
        else
          local n_67 = _scrutinee_70;
          return n_67 + 1
        end
      else
        if _scrutinee_69._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r_71 = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x_72 = 1;
    local r_73 = {a = x_72 + 1, b = x_72 * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local f_75 = function(x_74)
      local _scrutinee_76 = x_74;
      return 42
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local classify_81 = function(n_77)
      local _scrutinee_82 = n_77;
      if (function()
      local x_78 = _scrutinee_82;
      return x_78 < 0
    end)() then
        local x_78 = _scrutinee_82;
        return 0 - 1
      else
        if (function()
      local x_79 = _scrutinee_82;
      return x_79 == 0
    end)() then
          local x_79 = _scrutinee_82;
          return 0
        else
          local x_80 = _scrutinee_82;
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
    local f_89 = function(x_87)
      local _scrutinee_90 = x_87;
      if (function()
      local n_88 = _scrutinee_90;
      return n_88
    end)() then
        local n_88 = _scrutinee_90;
        return n_88
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
    local f_93 = function(x_91)
      local _scrutinee_94 = x_91;
      if _scrutinee_94._tag == 1 then
        local n_92 = _scrutinee_94._0;
        return n_92
      else
        if _scrutinee_94._tag == 0 then
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
    local f_97 = function(x_95)
      local _scrutinee_98 = x_95;
      if _scrutinee_98._tag == 1 then
        local n_96 = _scrutinee_98._0;
        return n_96
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
    local f_100 = function(x_99)
      local _scrutinee_101 = x_99;
      if _scrutinee_101._tag == 0 then
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
    local f_103 = function(b_102)
      local _scrutinee_104 = b_102;
      if _scrutinee_104 == false then
        return 0
      else
        if _scrutinee_104 == true then
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
    local f_106 = function(b_105)
      local _scrutinee_107 = b_105;
      if _scrutinee_107 == true then
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
    local f_110 = function(x_108)
      local _scrutinee_111 = x_108;
      if _scrutinee_111._tag == 1 then
        local n_109 = _scrutinee_111._0;
        return n_109
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
    local f_113 = function(x_112)
      local _scrutinee_114 = x_112;
      if _scrutinee_114._tag == 0 then
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
    local f_117 = function(x_115)
      local _scrutinee_118 = x_115;
      if _scrutinee_118._tag == 1 then
        local n_116 = _scrutinee_118._0;
        return n_116
      else
        if _scrutinee_118._tag == 0 then
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
    local f_121 = function(x_119)
      local _scrutinee_122 = x_119;
      if _scrutinee_122 == 0 then
        return 1
      else
        local n_120 = _scrutinee_122;
        return n_120
      end
    end
    |}]

(* === Pattern Matching: Decision Tree Tests === *)

let%expect_test "multiple constructors - three cases" =
  print_endline (compile "type color = Red | Green | Blue
let to_num c = match c with | Red -> 0 | Green -> 1 | Blue -> 2");
  [%expect{|
    local to_num_124 = function(c_123)
      local _scrutinee_125 = c_123;
      if _scrutinee_125._tag == 2 then
        return 2
      else
        if _scrutinee_125._tag == 1 then
          return 1
        else
          if _scrutinee_125._tag == 0 then
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
    local f_130 = function(p_126)
      local _scrutinee_131 = p_126;
      local c_129 = _scrutinee_131[2];
      local b_128 = _scrutinee_131[1][2];
      local a_127 = _scrutinee_131[1][1];
      return a_127 + b_128 + c_129
    end
    |}]

let%expect_test "constructor with tuple argument" =
  print_endline (compile "type pair = Pair of (int * int)
let sum p = match p with | Pair (a, b) -> a + b");
  [%expect{|
    local sum_135 = function(p_132)
      local _scrutinee_136 = p_132;
      if _scrutinee_136._tag == 0 then
        local b_134 = _scrutinee_136._0[2];
        local a_133 = _scrutinee_136._0[1];
        return a_133 + b_134
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "match on record pattern" =
  print_endline (compile "let get_x r = match r with | { x } -> x");
  [%expect{|
    local get_x_139 = function(r_137)
      local _scrutinee_140 = r_137;
      local x_138 = _scrutinee_140.x;
      return x_138
    end
    File "<string>", line 1, characters 14-39:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "match with multiple record fields" =
  print_endline (compile "let sum_xy r = match r with | { x; y } -> x + y");
  [%expect{|
    local sum_xy_144 = function(r_141)
      local _scrutinee_145 = r_141;
      local y_143 = _scrutinee_145.y;
      local x_142 = _scrutinee_145.x;
      return x_142 + y_143
    end
    File "<string>", line 1, characters 15-47:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "guard with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some n when n > 0 -> n | _ -> 0");
  [%expect{|
    local f_148 = function(x_146)
      local _scrutinee_149 = x_146;
      if _scrutinee_149._tag == 1 then
        if (function()
      local n_147 = _scrutinee_149._0;
      return n_147 > 0
    end)() then
          local n_147 = _scrutinee_149._0;
          return n_147
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
    local f_152 = function(x_150)
      local _scrutinee_153 = x_150;
      if _scrutinee_153._tag == 0 then
        return 0
      else
        if _scrutinee_153._tag == 1 then
          if (function()
      local n_151 = _scrutinee_153._0;
      return n_151 > 0
    end)() then
            local n_151 = _scrutinee_153._0;
            return n_151
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
    local head_157 = function(l_154)
      local _scrutinee_158 = l_154;
      if _scrutinee_158._tag == 1 then
        local t_156 = _scrutinee_158._0[2];
        local h_155 = _scrutinee_158._0[1];
        return h_155
      else
        if _scrutinee_158._tag == 0 then
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
    local is_left_160 = function(e_159)
      local _scrutinee_161 = e_159;
      if _scrutinee_161._tag == 1 then
        return false
      else
        if _scrutinee_161._tag == 0 then
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
    local greet_163 = function(name_162)
      local _scrutinee_164 = name_162;
      if _scrutinee_164 == "Bob" then
        return "Hey Bob"
      else
        if _scrutinee_164 == "Alice" then
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
    local negate_166 = function(b_165)
      local _scrutinee_167 = b_165;
      if _scrutinee_167 == false then
        return true
      else
        if _scrutinee_167 == true then
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
    local f_169 = function(u_168)
      local _scrutinee_170 = u_168;
      if _scrutinee_170 == nil then
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
    local sum3_175 = function(t_171)
      local _scrutinee_176 = t_171;
      local c_174 = _scrutinee_176[3];
      local b_173 = _scrutinee_176[2];
      local a_172 = _scrutinee_176[1];
      return a_172 + b_173 + c_174
    end
    |}]

let%expect_test "match with 4-element tuple" =
  print_endline (compile "let sum4 t = match t with | (a, b, c, d) -> a + b + c + d");
  [%expect{|
    local sum4_182 = function(t_177)
      local _scrutinee_183 = t_177;
      local d_181 = _scrutinee_183[4];
      local c_180 = _scrutinee_183[3];
      local b_179 = _scrutinee_183[2];
      local a_178 = _scrutinee_183[1];
      return a_178 + b_179 + c_180 + d_181
    end
    |}]

let%expect_test "match tuple with wildcard elements" =
  print_endline (compile "let first t = match t with | (a, _, _) -> a");
  [%expect{|
    local first_186 = function(t_184)
      local _scrutinee_187 = t_184;
      local a_185 = _scrutinee_187[1];
      return a_185
    end
    |}]

let%expect_test "match tuple with mixed patterns" =
  print_endline (compile "let check t = match t with | (0, x) -> x | (1, y) -> y + 1 | (_, z) -> z + 2");
  [%expect{|
    local check_192 = function(t_188)
      local _scrutinee_193 = t_188;
      if _scrutinee_193[1] == 1 then
        local y_190 = _scrutinee_193[2];
        return y_190 + 1
      else
        if _scrutinee_193[1] == 0 then
          local x_189 = _scrutinee_193[2];
          return x_189
        else
          local z_191 = _scrutinee_193[2];
          return z_191 + 2
        end
      end
    end
    |}]

(* === Pattern Matching: Deep Nesting === *)

let%expect_test "deeply nested constructor patterns" =
  print_endline (compile "type 'a option = None | Some of 'a
let deep x = match x with | Some (Some (Some n)) -> n | _ -> 0");
  [%expect{|
    local deep_196 = function(x_194)
      local _scrutinee_197 = x_194;
      if _scrutinee_197._tag == 1 then
        if _scrutinee_197._0._tag == 1 then
          if _scrutinee_197._0._0._tag == 1 then
            local n_195 = _scrutinee_197._0._0._0;
            return n_195
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
    local extract_203 = function(t_198)
      local _scrutinee_204 = t_198;
      if _scrutinee_204[1]._tag == 0 then
        local d_202 = _scrutinee_204[2];
        return d_202
      else
        if _scrutinee_204[1]._tag == 1 then
          local c_201 = _scrutinee_204[2];
          local b_200 = _scrutinee_204[1]._0[2];
          local a_199 = _scrutinee_204[1]._0[1];
          return a_199 + b_200 + c_201
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
    local unwrap_209 = function(w_205)
      local _scrutinee_210 = w_205;
      if _scrutinee_210._tag == 0 then
        if _scrutinee_210._0[1]._tag == 0 then
          local n_208 = _scrutinee_210._0[2];
          return n_208
        else
          if _scrutinee_210._0[1]._tag == 1 then
            local n_207 = _scrutinee_210._0[2];
            local x_206 = _scrutinee_210._0[1]._0;
            return x_206 + n_207
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
    local to_num_212 = function(d_211)
      local _scrutinee_213 = d_211;
      local _switch = _scrutinee_213;
      local _dispatch = {[3] = function()
      return 3
    end, [2] = function()
      return 2
    end, [1] = function()
      return 1
    end, [0] = function()
      return 0
    end};
      local _handler = _dispatch[_switch._tag];
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
    local to_num_215 = function(d_214)
      local _scrutinee_216 = d_214;
      if _scrutinee_216._tag == 2 then
        return 2
      else
        if _scrutinee_216._tag == 1 then
          return 1
        else
          if _scrutinee_216._tag == 0 then
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
    local is_monday_218 = function(d_217)
      local _scrutinee_219 = d_217;
      if _scrutinee_219._tag == 0 then
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
    local is_value_221 = function(e_220)
      local _scrutinee_222 = e_220;
      if _scrutinee_222._tag == 2 then
        return false
      else
        if _scrutinee_222._tag == 1 then
          return true
        else
          if _scrutinee_222._tag == 0 then
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
    local f_225 = function(x_223)
      local _scrutinee_226 = x_223;
      if _scrutinee_226._tag == 1 then
        local x_224 = _scrutinee_226._0;
        return x_224
      else
        if _scrutinee_226._tag == 0 then
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
    local get_pair_229 = function(p_227)
      local _scrutinee_230 = p_227;
      if _scrutinee_230._tag == 0 then
        local x_228 = _scrutinee_230._0;
        return x_228
      else
        return error("Match failure")
      end
    end
    |}]

let%expect_test "multiple bindings from nested pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let f x = match x with | Some (a, b, c) -> a + b + c | None -> 0");
  [%expect{|
    local f_235 = function(x_231)
      local _scrutinee_236 = x_231;
      if _scrutinee_236._tag == 0 then
        return 0
      else
        if _scrutinee_236._tag == 1 then
          local c_234 = _scrutinee_236._0[3];
          local b_233 = _scrutinee_236._0[2];
          local a_232 = _scrutinee_236._0[1];
          return a_232 + b_233 + c_234
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
    local check_242 = function(p_237)
      local _scrutinee_243 = p_237;
      if (function()
      local b_239 = _scrutinee_243[2];
      local a_238 = _scrutinee_243[1];
      return a_238 > b_239
    end)() then
        local b_239 = _scrutinee_243[2];
        local a_238 = _scrutinee_243[1];
        return a_238
      else
        local b_241 = _scrutinee_243[2];
        local a_240 = _scrutinee_243[1];
        return b_241
      end
    end
    File "<string>", line 1, characters 54-65:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "guard with boolean operators" =
  print_endline (compile "let range n = match n with | x when x > 0 -> (if x < 10 then 1 else 2) | _ -> 0");
  [%expect{|
    local range_246 = function(n_244)
      local _scrutinee_247 = n_244;
      if (function()
      local x_245 = _scrutinee_247;
      return x_245 > 0
    end)() then
        local x_245 = _scrutinee_247;
        if x_245 < 10 then
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
    local positive_250 = function(opt_248)
      local _scrutinee_251 = opt_248;
      if _scrutinee_251._tag == 0 then
        return false
      else
        if _scrutinee_251._tag == 1 then
          if (function()
      local n_249 = _scrutinee_251._0;
      return n_249 > 0
    end)() then
            local n_249 = _scrutinee_251._0;
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
    local classify_256 = function(n_252)
      local _scrutinee_257 = n_252;
      if (function()
      local x_253 = _scrutinee_257;
      return x_253 < 0
    end)() then
        local x_253 = _scrutinee_257;
        return 0 - 1
      else
        if (function()
      local x_254 = _scrutinee_257;
      return x_254 > 100
    end)() then
          local x_254 = _scrutinee_257;
          return 2
        else
          if (function()
      local x_255 = _scrutinee_257;
      return x_255 > 50
    end)() then
            local x_255 = _scrutinee_257;
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
    local length_258;
    length_258 = function(l_259)
      local _scrutinee_261 = l_259;
      if _scrutinee_261._tag == 1 then
        local t_260 = _scrutinee_261._0[2];
        return 1 + length_258(t_260)
      else
        if _scrutinee_261._tag == 0 then
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
    local sum_262;
    sum_262 = function(l_263)
      local _scrutinee_266 = l_263;
      if _scrutinee_266._tag == 1 then
        local t_265 = _scrutinee_266._0[2];
        local h_264 = _scrutinee_266._0[1];
        return h_264 + sum_262(t_265)
      else
        if _scrutinee_266._tag == 0 then
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
    local sum_acc_267;
    sum_acc_267 = function(acc_268)
      return function(l_269)
      local _scrutinee_272 = l_269;
      if _scrutinee_272._tag == 1 then
        local t_271 = _scrutinee_272._0[2];
        local h_270 = _scrutinee_272._0[1];
        return sum_acc_267(acc_268 + h_270)(t_271)
      else
        if _scrutinee_272._tag == 0 then
          return acc_268
        else
          return error("Match failure")
        end
      end
    end
    end
    |}]

let%expect_test "mutual recursion with pattern matching" =
  print_endline (compile "let rec even n = match n with | 0 -> true | n -> odd (n - 1)
and odd n = match n with | 0 -> false | n -> even (n - 1)");
  [%expect{|
    local even_273, odd_274;
    even_273 = function(n_275)
      local _scrutinee_279 = n_275;
      if _scrutinee_279 == 0 then
        return true
      else
        local n_276 = _scrutinee_279;
        return odd_274(n_276 - 1)
      end
    end;
    odd_274 = function(n_277)
      local _scrutinee_280 = n_277;
      if _scrutinee_280 == 0 then
        return false
      else
        local n_278 = _scrutinee_280;
        return even_273(n_278 - 1)
      end
    end
    |}]

(* === Pattern Matching: Edge Cases === *)

let%expect_test "single arm match - exhaustive via wildcard" =
  print_endline (compile "let id x = match x with | y -> y");
  [%expect{|
    local id_283 = function(x_281)
      local _scrutinee_284 = x_281;
      local y_282 = _scrutinee_284;
      return y_282
    end
    |}]

let%expect_test "single arm match with constructor - non-exhaustive" =
  print_endline (compile "type 'a option = None | Some of 'a
let unwrap x = match x with | Some y -> y");
  [%expect{|
    local unwrap_287 = function(x_285)
      local _scrutinee_288 = x_285;
      if _scrutinee_288._tag == 1 then
        local y_286 = _scrutinee_288._0;
        return y_286
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
    local first3_291 = function(t_289)
      local _scrutinee_292 = t_289;
      local x_290 = _scrutinee_292[1];
      return x_290
    end
    File "<string>", line 1, characters 47-53:
    Warning: Redundant pattern: this case will never be matched
    |}]

let%expect_test "overlapping constant and variable patterns" =
  print_endline (compile "let f x = match x with | 0 -> 100 | 1 -> 200 | n -> n");
  [%expect{|
    local f_295 = function(x_293)
      local _scrutinee_296 = x_293;
      if _scrutinee_296 == 1 then
        return 200
      else
        if _scrutinee_296 == 0 then
          return 100
        else
          local n_294 = _scrutinee_296;
          return n_294
        end
      end
    end
    |}]

let%expect_test "multiple identical wildcards" =
  print_endline (compile "let f x = match x with | _ -> 1 | _ -> 2 | _ -> 3");
  [%expect{|
    local f_298 = function(x_297)
      local _scrutinee_299 = x_297;
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
    local get_x_315 = function(p_312)
      local _scrutinee_316 = p_312;
      local y_314 = _scrutinee_316.y;
      local x_313 = _scrutinee_316.x;
      return x_313
    end
    File "<string>", line 2, characters 14-42:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "record pattern ignoring some fields" =
  print_endline (compile "type point3d = { x : int; y : int; z : int }
let project p = match p with | { x; y } -> (x, y)");
  [%expect{|
    local project_320 = function(p_317)
      local _scrutinee_321 = p_317;
      local y_319 = _scrutinee_321.y;
      local x_318 = _scrutinee_321.x;
      return {x_318, y_319}
    end
    File "<string>", line 2, characters 16-49:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

let%expect_test "nested record in constructor" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_x opt = match opt with | Some { x } -> x | None -> 0");
  [%expect{|
    local get_x_324 = function(opt_322)
      local _scrutinee_325 = opt_322;
      if _scrutinee_325._tag == 0 then
        return 0
      else
        if _scrutinee_325._tag == 1 then
          local x_323 = _scrutinee_325._0.x;
          return x_323
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
    local get_val_328 = function(r_326)
      local _scrutinee_329 = r_326;
      local value_327 = _scrutinee_329.x;
      return value_327
    end
    File "<string>", line 1, characters 16-53:
    Warning: Non-exhaustive pattern matching, missing case: _
    |}]

(* === Pattern Matching: Integration Tests === *)

let%expect_test "pattern match in function argument position" =
  print_endline (compile "let apply_pair f p = match p with | (a, b) -> f a b");
  [%expect{|
    local apply_pair_334 = function(f_330)
      return function(p_331)
      local _scrutinee_335 = p_331;
      local b_333 = _scrutinee_335[2];
      local a_332 = _scrutinee_335[1];
      return f_330(a_332)(b_333)
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
    local double_unwrap_340 = function(x_336)
      local _scrutinee_342 = x_336;
      local inner_338;
      if _scrutinee_342._tag == 0 then
        inner_338 = _Ctor_option_0
      else
        if _scrutinee_342._tag == 1 then
          local y_337 = _scrutinee_342._0;
          inner_338 = y_337
        else
          inner_338 = error("Match failure")
        end
      end;
      local _scrutinee_341 = inner_338;
      if _scrutinee_341._tag == 0 then
        return 0
      else
        if _scrutinee_341._tag == 1 then
          local z_339 = _scrutinee_341._0;
          return z_339
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
    local _Ctor_option_0 = {_tag = 0};
    local safe_div_347 = function(a_343)
      return function(b_344)
      if b_344 == 0 then
        return _Ctor_option_0
      else
        return {_tag = 1, _0 = (function()
      local _scrutinee_348 = {a_343, b_344};
      local y_346 = _scrutinee_348[2];
      local x_345 = _scrutinee_348[1];
      return x_345 / y_346
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
    local id_350 = function(x_349)
      return x_349
    end;
    local f_353 = function(opt_351)
      local _scrutinee_354 = id_350(opt_351);
      if _scrutinee_354._tag == 0 then
        return 0
      else
        if _scrutinee_354._tag == 1 then
          local n_352 = _scrutinee_354._0;
          return n_352
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
    local sum_355;
    sum_355 = function(t_356)
      local _scrutinee_360 = t_356;
      if _scrutinee_360._tag == 1 then
        local r_359 = _scrutinee_360._0[2];
        local l_358 = _scrutinee_360._0[1];
        return sum_355(l_358) + sum_355(r_359)
      else
        if _scrutinee_360._tag == 0 then
          local n_357 = _scrutinee_360._0;
          return n_357
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
    local eval_361;
    eval_361 = function(e_362)
      local _scrutinee_368 = e_362;
      if _scrutinee_368._tag == 2 then
        local b_367 = _scrutinee_368._0[2];
        local a_366 = _scrutinee_368._0[1];
        return eval_361(a_366) * eval_361(b_367)
      else
        if _scrutinee_368._tag == 1 then
          local b_365 = _scrutinee_368._0[2];
          local a_364 = _scrutinee_368._0[1];
          return eval_361(a_364) + eval_361(b_365)
        else
          if _scrutinee_368._tag == 0 then
            local n_363 = _scrutinee_368._0;
            return n_363
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
  [%expect{| local _nil_369 = 0 |}]

let%expect_test "lua keyword 'repeat' used as identifier is mangled" =
  print_endline (compile "let repeat = 99");
  [%expect{| local _repeat_370 = 99 |}]

let%expect_test "lua keyword 'until' used as identifier is mangled" =
  print_endline (compile "let until = 5");
  [%expect{| local _until_371 = 5 |}]

let%expect_test "lua keyword 'do' in function parameter is mangled" =
  print_endline (compile "let check do = do");
  [%expect{|
    ERROR: File "<string>", line 1, characters 10-12:
    Parse error: Syntax error
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
  [%expect{|
    ERROR: File "<string>", line 1, characters 4-9:
    Parse error: Syntax error
    |}]

let%expect_test "non-keyword identifier is not mangled" =
  print_endline (compile "let value = 42");
  [%expect{| local value_376 = 42 |}]

let%expect_test "identifier containing keyword is not mangled" =
  print_endline (compile "let end_marker = 10");
  [%expect{| local end_marker_377 = 10 |}]

(* --- Integer Tags Tests --- *)

let%expect_test "constructor uses integer tag 0 for first variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let none_val = None");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none_val_378 = _Ctor_option_0
    |}]

let%expect_test "constructor uses integer tag 1 for second variant" =
  print_endline (compile "type 'a option = None | Some of 'a
let some_val = Some 42");
  [%expect{| local some_val_379 = {_tag = 1, _0 = 42} |}]

let%expect_test "three variant type uses correct integer tags" =
  print_endline (compile "type color = Red | Green | Blue
let red = Red
let green = Green
let blue = Blue");
  [%expect{|
    local _Ctor_color_2 = {_tag = 2};
    local _Ctor_color_1 = {_tag = 1};
    local _Ctor_color_0 = {_tag = 0};
    local red_380 = _Ctor_color_0;
    local green_381 = _Ctor_color_1;
    local blue_382 = _Ctor_color_2
    |}]

let%expect_test "pattern match compares against integer tags" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with
  | None -> false
  | Some _ -> true");
  [%expect{|
    local is_some_384 = function(opt_383)
      local _scrutinee_385 = opt_383;
      if _scrutinee_385._tag == 1 then
        return true
      else
        if _scrutinee_385._tag == 0 then
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
    local _Ctor_option_0 = {_tag = 0};
    local a_386 = _Ctor_option_0;
    local b_387 = _Ctor_option_0
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
    local stop_388 = _Ctor_traffic_light_0;
    local caution_389 = _Ctor_traffic_light_1;
    local go_390 = _Ctor_traffic_light_2
    |}]

let%expect_test "non-nullary constructor does not use singleton" =
  print_endline (compile "type 'a option = None | Some of 'a
let x = Some 1
let y = Some 2");
  [%expect{|
    local x_391 = {_tag = 1, _0 = 1};
    local y_392 = {_tag = 1, _0 = 2}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  print_endline (compile "type 'a option = None | Some of 'a
let none1 = None
let some1 = Some 10
let none2 = None
let some2 = Some 20");
  [%expect{|
    local _Ctor_option_0 = {_tag = 0};
    local none1_393 = _Ctor_option_0;
    local some1_394 = {_tag = 1, _0 = 10};
    local none2_395 = _Ctor_option_0;
    local some2_396 = {_tag = 1, _0 = 20}
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
    local to_num_398 = function(dir_397)
      local _scrutinee_399 = dir_397;
      local _switch = _scrutinee_399;
      local _dispatch = {[3] = function()
      return 3
    end, [2] = function()
      return 2
    end, [1] = function()
      return 1
    end, [0] = function()
      return 0
    end};
      local _handler = _dispatch[_switch._tag];
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
    local day_num_401 = function(day_400)
      local _scrutinee_402 = day_400;
      local _switch = _scrutinee_402;
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
      local _handler = _dispatch[_switch._tag];
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
    local to_num_404 = function(color_403)
      local _scrutinee_405 = color_403;
      if _scrutinee_405._tag == 2 then
        return 2
      else
        if _scrutinee_405._tag == 1 then
          return 1
        else
          if _scrutinee_405._tag == 0 then
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
    local eval_411 = function(expression_406)
      local _scrutinee_412 = expression_406;
      local _switch = _scrutinee_412;
      local _dispatch = {[3] = function()
      local n_410 = _scrutinee_412._0;
      return n_410 * 2
    end, [2] = function()
      local n_409 = _scrutinee_412._0;
      return n_409 - 1
    end, [1] = function()
      local n_408 = _scrutinee_412._0;
      return n_408 + 1
    end, [0] = function()
      local n_407 = _scrutinee_412._0;
      return n_407
    end};
      local _handler = _dispatch[_switch._tag];
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
    local x_413 = 1;
    local result_414 = x_413 + 2
    |}]

let%expect_test "deeply nested let is fully floated" =
  print_endline (compile "let result = let a = 1 in let b = 2 in a + b");
  [%expect{|
    local a_415 = 1;
    local b_416 = 2;
    local result_417 = a_415 + b_416
    |}]

let%expect_test "if in value position uses assignment not IIFE" =
  print_endline (compile "let result = if true then 1 else 2");
  [%expect{|
    local result_418;
    if true then
      result_418 = 1
    else
      result_418 = 2
    end
    |}]

let%expect_test "if with complex condition uses assignment" =
  print_endline (compile "let x = 5
let result = if x > 0 then 1 else 0");
  [%expect{|
    local x_419 = 5;
    local result_420;
    if x_419 > 0 then
      result_420 = 1
    else
      result_420 = 0
    end
    |}]

let%expect_test "nested if in value position is handled" =
  print_endline (compile "let result = if true then if false then 1 else 2 else 3");
  [%expect{|
    local result_421;
    if true then
      if false then
        result_421 = 1
      else
        result_421 = 2
      end
    else
      result_421 = 3
    end
    |}]

let%expect_test "let with if value is properly floated" =
  print_endline (compile "let result = let flag = if true then 1 else 0 in flag + 1");
  [%expect{|
    local flag_422;
    if true then
      flag_422 = 1
    else
      flag_422 = 0
    end;
    local result_423 = flag_422 + 1
    |}]

let%expect_test "sequence in value position is floated" =
  print_endline (compile "let result = let _ = print 1 in 42");
  [%expect{|
    local __425 = print(1);
    local result_424 = 42
    |}]

let%expect_test "function body still uses statements not IIFE" =
  print_endline (compile "let compute x = let y = x + 1 in y * 2");
  [%expect{|
    local compute_428 = function(x_426)
      local y_427 = x_426 + 1;
      return y_427 * 2
    end
    |}]

let%expect_test "if in function body uses statement form" =
  print_endline (compile "let abs n = if n < 0 then 0 - n else n");
  [%expect{|
    local abs_430 = function(n_429)
      if n_429 < 0 then
        return 0 - n_429
      else
        return n_429
      end
    end
    |}]

(* --- Tail Call Optimization Tests --- *)

let%expect_test "simple tail recursive function generates proper return" =
  print_endline (compile "let rec sum_to n acc = if n <= 0 then acc else sum_to (n - 1) (acc + n)");
  [%expect{|
    local sum_to_431;
    sum_to_431 = function(n_432)
      return function(acc_433)
      if n_432 <= 0 then
        return acc_433
      else
        return sum_to_431(n_432 - 1)(acc_433 + n_432)
      end
    end
    end
    |}]

let%expect_test "tail call in then branch uses return" =
  print_endline (compile "let rec countdown n = if n <= 0 then 0 else countdown (n - 1)");
  [%expect{|
    local countdown_434;
    countdown_434 = function(n_435)
      if n_435 <= 0 then
        return 0
      else
        return countdown_434(n_435 - 1)
      end
    end
    |}]

let%expect_test "tail call in else branch uses return" =
  print_endline (compile "let rec find_zero n = if n == 0 then true else find_zero (n - 1)");
  [%expect{|
    local find_zero_436;
    find_zero_436 = function(n_437)
      if n_437 == 0 then
        return true
      else
        return find_zero_436(n_437 - 1)
      end
    end
    |}]

let%expect_test "mutual recursion generates proper tail calls" =
  print_endline (compile "let rec is_even n = if n == 0 then true else is_odd (n - 1)
and is_odd n = if n == 0 then false else is_even (n - 1)");
  [%expect{|
    local is_even_438, is_odd_439;
    is_even_438 = function(n_440)
      if n_440 == 0 then
        return true
      else
        return is_odd_439(n_440 - 1)
      end
    end;
    is_odd_439 = function(n_441)
      if n_441 == 0 then
        return false
      else
        return is_even_438(n_441 - 1)
      end
    end
    |}]

let%expect_test "non-tail call is not in return position" =
  print_endline (compile "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect{|
    local factorial_442;
    factorial_442 = function(n_443)
      if n_443 <= 1 then
        return 1
      else
        return n_443 * factorial_442(n_443 - 1)
      end
    end
    |}]

let%expect_test "accumulator pattern enables tail call" =
  print_endline (compile "let rec factorial_acc n acc = if n <= 1 then acc else factorial_acc (n - 1) (n * acc)");
  [%expect{|
    local factorial_acc_444;
    factorial_acc_444 = function(n_445)
      return function(acc_446)
      if n_445 <= 1 then
        return acc_446
      else
        return factorial_acc_444(n_445 - 1)(n_445 * acc_446)
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
    local eval_cmd_448 = function(and_val_447)
      local _scrutinee_449 = and_val_447;
      local _switch = _scrutinee_449;
      local _dispatch = {[3] = function()
      return 4
    end, [2] = function()
      return 3
    end, [1] = function()
      return 2
    end, [0] = function()
      return 1
    end};
      local _handler = _dispatch[_switch._tag];
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
    local get_or_default_454 = function(opt_450)
      return function(default_val_451)
      local result_452;
      if true then
        result_452 = opt_450
      else
        result_452 = _Ctor_option_0
      end;
      local _scrutinee_455 = result_452;
      if _scrutinee_455._tag == 1 then
        local x_453 = _scrutinee_455._0;
        return x_453
      else
        if _scrutinee_455._tag == 0 then
          return default_val_451
        else
          return error("Match failure")
        end
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
    local process_direction_460 = function(dir_456)
      local opt_value_457;
      if dir_456 == 0 then
        opt_value_457 = _Ctor_option_0
      else
        opt_value_457 = {_tag = 1, _0 = dir_456}
      end;
      local _scrutinee_461 = opt_value_457;
      local result_459;
      if _scrutinee_461._tag == 1 then
        local n_458 = _scrutinee_461._0;
        result_459 = n_458
      else
        if _scrutinee_461._tag == 0 then
          result_459 = 0
        else
          result_459 = error("Match failure")
        end
      end;
      return result_459 + 1
    end
    |}]

(* Reference operations *)

let%expect_test "ref creates table with value field" =
  print_endline (compile "let r = ref 42");
  [%expect{| local r_462 = {value = 42} |}]

let%expect_test "deref accesses value field" =
  print_endline (compile "let r = ref 42
let x = !r");
  [%expect{|
    local r_463 = {value = 42};
    local x_464 = r_463.value
    |}]

let%expect_test "assign modifies value field" =
  print_endline (compile "let r = ref 0
let _ = r := 1");
  [%expect{|
    local r_465 = {value = 0};
    local _top_466 = (function()
      r_465.value = 1;
      return nil
    end)()
    |}]

let%expect_test "increment ref" =
  print_endline (compile "let r = ref 0
let _ = r := !r + 1");
  [%expect{|
    local r_467 = {value = 0};
    local _top_468 = (function()
      r_467.value = r_467.value + 1;
      return nil
    end)()
    |}]

let%expect_test "ref in function" =
  print_endline (compile "let make_counter init =
  let c = ref init in
  let get = fun () -> !c in
  (get, c)");
  [%expect{|
    local make_counter_472 = function(init_469)
      local c_470 = {value = init_469};
      local get_471 = function(_param_473)
      return c_470.value
    end;
      return {get_471, c_470}
    end
    |}]

let%expect_test "multiple refs" =
  print_endline (compile "let x = ref 10
let y = ref 20
let _ = x := !x + !y");
  [%expect{|
    local x_474 = {value = 10};
    local y_475 = {value = 20};
    local _top_476 = (function()
      x_474.value = x_474.value + y_475.value;
      return nil
    end)()
    |}]

let%expect_test "ref with string" =
  print_endline (compile {|let s = ref "hello"
let _ = s := "world"|});
  [%expect{|
    local s_477 = {value = "hello"};
    local _top_478 = (function()
      s_477.value = "world";
      return nil
    end)()
    |}]
