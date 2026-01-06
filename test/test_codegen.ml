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
      if _scrutinee_24 == 0 then
        return 1
      else
        if _scrutinee_24 == 1 then
          return 2
        else
          if true then
            local n_22 = _scrutinee_24
            return n_22
          else
            return error_25("Match failure")
          end
        end
      end
    end
    |}]

let%expect_test "match with tuple pattern" =
  print_endline (compile "let sum_pair p = match p with | (a, b) -> a + b");
  [%expect{|
    local function sum_pair_29(p_26)
      local _scrutinee_30 = p_26
      if (function()
      if (function()
      if true then
        return true
      else
        return false
      end
    end)() then
        return true
      else
        return false
      end
    end)() then
        local _tuple_32 = _scrutinee_30
        local a_27 = _tuple_32[1]
        local b_28 = _tuple_32[2]
        return a_27 + b_28
      else
        return error_31("Match failure")
      end
    end
    |}]

let%expect_test "match with guard" =
  print_endline (compile "let abs n = match n with | x when x < 0 -> 0 - x | x -> x");
  [%expect{|
    local function abs_36(n_33)
      local _scrutinee_37 = n_33
      if true then
        if (function()
      local x_34 = _scrutinee_37
      return x_34 < 0
    end)() then
          local x_34 = _scrutinee_37
          return 0 - x_34
        else
          if true then
            local x_35 = _scrutinee_37
            return x_35
          else
            return error_38("Match failure")
          end
        end
      else
        if true then
          local x_35 = _scrutinee_37
          return x_35
        else
          return error_38("Match failure")
        end
      end
    end
    |}]

let%expect_test "match with constructor pattern" =
  print_endline (compile "type 'a option = None | Some of 'a
let is_some opt = match opt with | None -> false | Some _ -> true");
  [%expect{|
    local function is_some_40(opt_39)
      local _scrutinee_41 = opt_39
      if _scrutinee_41._tag == "None" then
        return false
      else
        if (function()
      if _scrutinee_41._tag == "Some" then
        return true
      else
        return false
      end
    end)() then
          local _ctor_43 = _scrutinee_41
          local __44 = _ctor_43._0
          return true
        else
          return error_42("Match failure")
        end
      end
    end
    |}]

let%expect_test "match extracts constructor argument" =
  print_endline (compile "type 'a option = None | Some of 'a
let get_or_default opt default = match opt with | None -> default | Some x -> x");
  [%expect{|
    local function get_or_default_48(opt_45, default_46)
      local _scrutinee_49 = opt_45
      if _scrutinee_49._tag == "None" then
        return default_46
      else
        if (function()
      if _scrutinee_49._tag == "Some" then
        return true
      else
        return false
      end
    end)() then
          local _ctor_51 = _scrutinee_49
          local x_47 = _ctor_51._0
          return x_47
        else
          return error_50("Match failure")
        end
      end
    end
    |}]

let%expect_test "constructor expression generates tagged table" =
  print_endline (compile "type 'a option = None | Some of 'a
let none = None
let some_val = Some 42");
  [%expect{|
    local none_52 = {_tag = "None"}
    local some_val_53 = {_tag = "Some", _0 = 42}
    |}]

let%expect_test "row polymorphic function" =
  print_endline (compile "let get_x r = r.x
let p1 = { x = 1; y = 2 }
let p2 = { x = 10; z = 20 }
let v1 = get_x p1
let v2 = get_x p2");
  [%expect{|
    local function get_x_55(r_54)
      return r_54.x
    end
    local p1_56 = {x = 1, y = 2}
    local p2_57 = {x = 10, z = 20}
    local v1_58 = get_x_55(p1_56)
    local v2_59 = get_x_55(p2_57)
    |}]

let%expect_test "multiple record updates" =
  print_endline (compile "let r = { a = 1; b = 2; c = 3 }
let r2 = { r with a = 10; c = 30 }");
  [%expect{|
    local r_60 = {a = 1, b = 2, c = 3}
    local r2_61 = (function()
      local _result = {}
      for _k, _v in pairs(r_60) do
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
    local function make_point_64(x_62, y_63)
      return {x = x_62, y = y_63}
    end
    |}]

let%expect_test "match in let binding" =
  print_endline (compile "let f p =
  let (a, b) = p in
  a + b");
  [%expect{|
    local function f_68(p_65)
      local _tuple_69 = p_65
      local a_66 = _tuple_69[1]
      local b_67 = _tuple_69[2]
      return a_66 + b_67
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
    local function f_73(opt_70)
      local _scrutinee_74 = opt_70
      if _scrutinee_74._tag == "None" then
        return 0
      else
        if (function()
      if _scrutinee_74._tag == "Some" then
        return true
      else
        return false
      end
    end)() then
          local _ctor_78 = _scrutinee_74
          local x_71 = _ctor_78._0
          local _scrutinee_76 = x_71
          if _scrutinee_76 == 0 then
            return 1
          else
            if true then
              local n_72 = _scrutinee_76
              return n_72 + 1
            else
              return error_77("Match failure")
            end
          end
        else
          return error_75("Match failure")
        end
      end
    end
    |}]

let%expect_test "empty record" =
  print_endline (compile "let r = { }");
  [%expect{| local r_79 = {} |}]

let%expect_test "record with expression values" =
  print_endline (compile "let x = 1
let r = { a = x + 1; b = x * 2 }");
  [%expect{|
    local x_80 = 1
    local r_81 = {a = x_80 + 1, b = x_80 * 2}
    |}]

let%expect_test "match wildcard pattern" =
  print_endline (compile "let f x = match x with | _ -> 42");
  [%expect{|
    local function f_83(x_82)
      local _scrutinee_84 = x_82
      if true then
        local __86 = _scrutinee_84
        return 42
      else
        return error_85("Match failure")
      end
    end
    |}]

let%expect_test "multiple guards in match" =
  print_endline (compile "let classify n = match n with
  | x when x < 0 -> 0 - 1
  | x when x == 0 -> 0
  | x -> 1");
  [%expect{|
    local function classify_91(n_87)
      local _scrutinee_92 = n_87
      if true then
        if (function()
      local x_88 = _scrutinee_92
      return x_88 < 0
    end)() then
          local x_88 = _scrutinee_92
          return 0 - 1
        else
          if true then
            if (function()
      local x_89 = _scrutinee_92
      return x_89 == 0
    end)() then
              local x_89 = _scrutinee_92
              return 0
            else
              if true then
                local x_90 = _scrutinee_92
                return 1
              else
                return error_93("Match failure")
              end
            end
          else
            if true then
              local x_90 = _scrutinee_92
              return 1
            else
              return error_93("Match failure")
            end
          end
        end
      else
        if true then
          if (function()
      local x_89 = _scrutinee_92
      return x_89 == 0
    end)() then
            local x_89 = _scrutinee_92
            return 0
          else
            if true then
              local x_90 = _scrutinee_92
              return 1
            else
              return error_93("Match failure")
            end
          end
        else
          if true then
            local x_90 = _scrutinee_92
            return 1
          else
            return error_93("Match failure")
          end
        end
      end
    end
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
    local function f_100(x_98)
      local _scrutinee_101 = x_98
      if true then
        if (function()
      local n_99 = _scrutinee_101
      return n_99
    end)() then
          local n_99 = _scrutinee_101
          return n_99
        else
          return error_102("Match failure")
        end
      else
        return error_102("Match failure")
      end
    end
    |}]
