(** Unit tests for Lua codegen context threading.

    Tests cover:
    - Singleton optimization for nullary constructors
    - Dispatch table threshold behavior
    - Generated Lua code structure

    Note: The context is internal to codegen, so we test through the public
    generate function and verify the output. *)

open Driver

(** Helper for String.is_substring *)
module String = struct
  include String
  let is_substring ~substring s =
    let len_sub = String.length substring in
    let len_s = String.length s in
    if len_sub > len_s then false
    else
      let rec check i =
        if i + len_sub > len_s then false
        else if String.sub s i len_sub = substring then true
        else check (i + 1)
      in
      check 0
end

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

(** Helper to compile Lina code to Lua and return the generated code *)
let compile code =
  Typing.Types.reset_type_variable_id ();
  match Pipeline.compile_string test_options "<test>" code with
  | Ok lua -> lua
  | Error msg -> "ERROR: " ^ msg

(** {1 Singleton Optimization Tests} *)

let%expect_test "nullary constructor generates singleton preamble" =
  let code = {|
type option = None | Some of int
let x = None
|} in
  let lua = compile code in
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_option_0 = {_tag = 0};
    local x = _Ctor_option_0
    |}]

let%expect_test "multiple nullary constructors share singletons" =
  let code = {|
type color = Red | Green | Blue
let r = Red
let g = Green
let b = Blue
|} in
  let lua = compile code in
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_color_2 = {_tag = 2};
    local _Ctor_color_1 = {_tag = 1};
    local _Ctor_color_0 = {_tag = 0};
    local r = _Ctor_color_0;
    local g = _Ctor_color_1;
    local b = _Ctor_color_2
    |}]

let%expect_test "same nullary constructor used multiple times reuses singleton" =
  let code = {|
type option = None | Some of int
let a = None
let b = None
let c = None
|} in
  let lua = compile code in
  (* Should only have ONE _Ctor_option_0 declaration *)
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_option_0 = {_tag = 0};
    local a = _Ctor_option_0;
    local b = _Ctor_option_0;
    local c = _Ctor_option_0
    |}]

let%expect_test "non-nullary constructor does not generate singleton" =
  let code = {|
type option = None | Some of int
let x = Some 42
|} in
  let lua = compile code in
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local x = {_tag = 1, _0 = 42}
    |}]

let%expect_test "mixed nullary and non-nullary constructors" =
  let code = {|
type option = None | Some of int
let a = None
let b = Some 42
let c = None
|} in
  let lua = compile code in
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_option_0 = {_tag = 0};
    local a = _Ctor_option_0;
    local b = {_tag = 1, _0 = 42};
    local c = _Ctor_option_0
    |}]

(** {1 Dispatch Table Threshold Tests} *)

let%expect_test "3 constructors uses if-chain" =
  let code = {|
type small = A | B | C
let f x = match x with
  | A -> 1
  | B -> 2
  | C -> 3
|} in
  let lua = compile code in
  (* Should use if-else chain, not dispatch table *)
  let has_dispatch = String.is_substring ~substring:"_dispatch" lua in
  print_endline (if has_dispatch then "dispatch" else "if-chain");
  [%expect {| if-chain |}]

let%expect_test "4 constructors uses dispatch table" =
  let code = {|
type medium = W | X | Y | Z
let f x = match x with
  | W -> 1
  | X -> 2
  | Y -> 3
  | Z -> 4
|} in
  let lua = compile code in
  (* Should use dispatch table *)
  let has_dispatch = String.is_substring ~substring:"_dispatch" lua in
  print_endline (if has_dispatch then "dispatch" else "if-chain");
  [%expect {| dispatch |}]

let%expect_test "6 constructors uses dispatch table" =
  let code = {|
type large = P | Q | R | S | T | U
let f x = match x with
  | P -> 1
  | Q -> 2
  | R -> 3
  | S -> 4
  | T -> 5
  | U -> 6
|} in
  let lua = compile code in
  let has_dispatch = String.is_substring ~substring:"_dispatch" lua in
  print_endline (if has_dispatch then "dispatch" else "if-chain");
  [%expect {| dispatch |}]

(** {1 Constants Tests} *)

let%expect_test "dispatch_table_threshold constant value" =
  print_int Common.Codegen_constants.dispatch_table_threshold;
  [%expect {| 4 |}]

let%expect_test "singleton_var_name generates correct format" =
  let name1 = Common.Codegen_constants.singleton_var_name "option" 0 in
  let name2 = Common.Codegen_constants.singleton_var_name "color" 2 in
  print_endline name1;
  print_endline name2;
  [%expect {|
    _Ctor_option_0
    _Ctor_color_2 |}]

(** {1 Context Threading Integration Tests} *)

let%expect_test "nested let expressions preserve singletons" =
  let code = {|
type option = None | Some of int
let x =
  let a = None in
  let b = None in
  a
|} in
  let lua = compile code in
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_option_0 = {_tag = 0};
    local a = _Ctor_option_0;
    local b = _Ctor_option_0;
    local x = a
    |}]

let%expect_test "conditionals with singletons in both branches" =
  let code = {|
type option = None | Some of int
let f b = if b then None else None
|} in
  let lua = compile code in
  (* Both branches should reference the same singleton *)
  print_endline lua;
  [%expect {|
    local Fn = (function()
    local function id(x)
      return x
    end;

    local const = function(x_1)
      return function(param)
        local _ = param;
        return x_1
      end
    end;

    local flip = function(f)
      return function(x_2)
        return function(y)
          return f(y)(x_2)
        end
      end
    end;

    local _bar__gt_ = function(x_3)
      return function(f_1)
        return f_1(x_3)
      end
    end;

    local _at__at_ = function(f_2)
      return function(x_4)
        return f_2(x_4)
      end
    end;

    local _gt__gt_ = function(f_3)
      return function(g)
        return function(x_5)
          return g(f_3(x_5))
        end
      end
    end;

    local _lt__lt_ = function(f_4)
      return function(g_1)
        return function(x_6)
          return f_4(g_1(x_6))
        end
      end
    end;

    local tap = function(f_5)
      return function(x_7)
        local __1 = f_5(x_7);
        return x_7
      end
    end;

    local compose = function(f_6)
      return function(g_2)
        return function(x_8)
          return g_2(f_6(x_8))
        end
      end
    end;

    local compose_left = function(f_7)
      return function(g_3)
        return function(x_9)
          return f_7(g_3(x_9))
        end
      end
    end;

    local negate = function(pred)
      return function(x_10)
        if pred(x_10) then
          return false
        else
          return true
        end
      end
    end;

    local apply = function(f_8)
      return function(x_11)
        return f_8(x_11)
      end
    end;

    local pipe = function(x_12)
      return function(f_9)
        return f_9(x_12)
      end
    end;

    local function ignore(param_1)
      local __2 = param_1;
      return nil
    end
    return {["id"] = id, ["const"] = const, ["flip"] = flip, ["|>"] = _bar__gt_, ["@@"] = _at__at_, [">>"] = _gt__gt_, ["<<"] = _lt__lt_, ["tap"] = tap, ["compose"] = compose, ["compose_left"] = compose_left, ["negate"] = negate, ["apply"] = apply, ["pipe"] = pipe, ["ignore"] = ignore}
    end)();

    local Result = (function()
    local _Ctor_option_0 = {_tag = 0};
    local function ok(x)
      return {_tag = 0, _0 = x}
    end;

    local function error(e)
      return {_tag = 1, _0 = e}
    end;

    local function is_ok(r)
      local r_match = r;
      local matched = r_match;
      if matched._tag == 1 then
        return false
      elseif matched._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_error(r_1)
      local r_match_1 = r_1;
      local matched_1 = r_match_1;
      if matched_1._tag == 1 then
        return true
      elseif matched_1._tag == 0 then
        return false
      else
        return error("Match failure")
      end
    end;

    local get_or = function(r_2)
      return function(default)
        local r_match_2 = r_2;
        local matched_2 = r_match_2;
        if matched_2._tag == 1 then
          return default
        elseif matched_2._tag == 0 then
          local x_1 = r_match_2._0;
          return x_1
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(r_3)
      return function(f)
        local r_match_3 = r_3;
        local matched_3 = r_match_3;
        if matched_3._tag == 1 then
          local e_1 = r_match_3._0;
          return f(e_1)
        elseif matched_3._tag == 0 then
          local x_2 = r_match_3._0;
          return x_2
        else
          return error("Match failure")
        end
      end
    end;

    local get_error_or = function(r_4)
      return function(default_1)
        local r_match_4 = r_4;
        local matched_4 = r_match_4;
        if matched_4._tag == 1 then
          local e_2 = r_match_4._0;
          return e_2
        elseif matched_4._tag == 0 then
          return default_1
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f_1)
      return function(r_5)
        local r_match_5 = r_5;
        local matched_5 = r_match_5;
        if matched_5._tag == 1 then
          local e_3 = r_match_5._0;
          return {_tag = 1, _0 = e_3}
        elseif matched_5._tag == 0 then
          local x_3 = r_match_5._0;
          return {_tag = 0, _0 = f_1(x_3)}
        else
          return error("Match failure")
        end
      end
    end;

    local map_error = function(f_2)
      return function(r_6)
        local r_match_6 = r_6;
        local matched_6 = r_match_6;
        if matched_6._tag == 1 then
          local e_4 = r_match_6._0;
          return {_tag = 1, _0 = f_2(e_4)}
        elseif matched_6._tag == 0 then
          local x_4 = r_match_6._0;
          return {_tag = 0, _0 = x_4}
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_3)
      return function(r_7)
        local r_match_7 = r_7;
        local matched_7 = r_match_7;
        if matched_7._tag == 1 then
          local e_5 = r_match_7._0;
          return {_tag = 1, _0 = e_5}
        elseif matched_7._tag == 0 then
          local x_5 = r_match_7._0;
          return f_3(x_5)
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(r_8)
      local r_match_8 = r_8;
      local matched_8 = r_match_8;
      if matched_8._tag == 1 then
        local e_6 = r_match_8._0;
        return {_tag = 1, _0 = e_6}
      elseif matched_8._tag == 0 then
        local inner = r_match_8._0;
        return inner
      else
        return error("Match failure")
      end
    end;

    local or_ = function(r1)
      return function(r2)
        local r1_match = r1;
        local matched_9 = r1_match;
        if matched_9._tag == 1 then
          return r2
        elseif matched_9._tag == 0 then
          return r1
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(r1_1)
      return function(r2_1)
        local r1_match_1 = r1_1;
        local matched_10 = r1_match_1;
        if matched_10._tag == 1 then
          return r1_1
        elseif matched_10._tag == 0 then
          return r2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_4)
      return function(r1_2)
        return function(r2_2)
          local r1_match_2 = r1_2;
          local matched_11 = r1_match_2;
          if matched_11._tag == 0 then
            local x_6 = r1_match_2._0;
            local r2_match = r2_2;
            local matched_12 = r2_match;
            if matched_12._tag == 0 then
              local y = r2_match._0;
              return {_tag = 0, _0 = f_4(x_6)(y)}
            elseif matched_12._tag == 1 then
              local e_7 = r2_match._0;
              return {_tag = 1, _0 = e_7}
            else
              return error("Match failure")
            end
          elseif matched_11._tag == 1 then
            local e_8 = r1_match_2._0;
            return {_tag = 1, _0 = e_8}
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(ok_fn)
      return function(error_fn)
        return function(r_9)
          local r_match_9 = r_9;
          local matched_13 = r_match_9;
          if matched_13._tag == 1 then
            local e_9 = r_match_9._0;
            return error_fn(e_9)
          elseif matched_13._tag == 0 then
            local x_7 = r_match_9._0;
            return ok_fn(x_7)
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_5)
      return function(r_10)
        local r_match_10 = r_10;
        local matched_14 = r_match_10;
        if matched_14._tag == 1 then
          return nil
        elseif matched_14._tag == 0 then
          local x_8 = r_match_10._0;
          return f_5(x_8)
        else
          return error("Match failure")
        end
      end
    end;

    local iter_error = function(f_6)
      return function(r_11)
        local r_match_11 = r_11;
        local matched_15 = r_match_11;
        if matched_15._tag == 1 then
          local e_10 = r_match_11._0;
          return f_6(e_10)
        elseif matched_15._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local function to_option(r_12)
      local r_match_12 = r_12;
      local matched_16 = r_match_12;
      if matched_16._tag == 1 then
        return _Ctor_option_0
      elseif matched_16._tag == 0 then
        local x_9 = r_match_12._0;
        return {_tag = 1, _0 = x_9}
      else
        return error("Match failure")
      end
    end;

    local of_option = function(opt)
      return function(error_value)
        local opt_match = opt;
        local matched_17 = opt_match;
        if matched_17._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_17._tag == 1 then
          local x_10 = opt_match._0;
          return {_tag = 0, _0 = x_10}
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(ok_eq)
      return function(err_eq)
        return function(r1_3)
          return function(r2_3)
            local matched_18 = {r1_3, r2_3};
            local matched_19 = matched_18[1];
            if matched_19._tag == 1 then
              local matched_20 = matched_18[2];
              if matched_20._tag == 1 then
                local e2 = matched_18[2]._0;
                local e1 = matched_18[1]._0;
                return err_eq(e1)(e2)
              else
                return false
              end
            elseif matched_19._tag == 0 then
              local matched_21 = matched_18[2];
              if matched_21._tag == 0 then
                local x2 = matched_18[2]._0;
                local x1 = matched_18[1]._0;
                return ok_eq(x1)(x2)
              else
                return false
              end
            else
              return false
            end
          end
        end
      end
    end;

    local let_star_ = function(r_13)
      return function(f_7)
        return flat_map(f_7)(r_13)
      end
    end;

    local and_star_ = function(r1_4)
      return function(r2_4)
        return map2(function(a)
          return function(b)
            return {a, b}
          end
        end)(r1_4)(r2_4)
      end
    end;

    local let_plus_ = function(r_14)
      return function(f_8)
        return map(f_8)(r_14)
      end
    end;
    local and_plus_ = and_star_
    return {["ok"] = ok, ["error"] = error, ["is_ok"] = is_ok, ["is_error"] = is_error, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_error_or"] = get_error_or, ["map"] = map, ["map_error"] = map_error, ["flat_map"] = flat_map, ["flatten"] = flatten, ["or_"] = or_, ["and_"] = and_, ["map2"] = map2, ["fold"] = fold, ["iter"] = iter, ["iter_error"] = iter_error, ["to_option"] = to_option, ["of_option"] = of_option, ["equal"] = equal, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local Option = (function()
    local _Ctor_option_0 = {_tag = 0};
    local none = _Ctor_option_0;
    local function some(value)
      return {_tag = 1, _0 = value}
    end;

    local function is_some(opt)
      local opt_match = opt;
      local matched = opt_match;
      if matched._tag == 0 then
        return false
      elseif matched._tag == 1 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function is_none(opt_1)
      local opt_match_1 = opt_1;
      local matched_1 = opt_match_1;
      if matched_1._tag == 0 then
        return true
      elseif matched_1._tag == 1 then
        return false
      else
        return error("Match failure")
      end
    end;

    local contains = function(value_1)
      return function(opt_2)
        local opt_match_2 = opt_2;
        local matched_2 = opt_match_2;
        if matched_2._tag == 0 then
          return false
        elseif matched_2._tag == 1 then
          local inner = opt_match_2._0;
          return inner == value_1
        else
          return error("Match failure")
        end
      end
    end;

    local for_all = function(predicate)
      return function(opt_3)
        local opt_match_3 = opt_3;
        local matched_3 = opt_match_3;
        if matched_3._tag == 1 then
          local value_2 = opt_match_3._0;
          return predicate(value_2)
        elseif matched_3._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;

    local exists = function(predicate_1)
      return function(opt_4)
        local opt_match_4 = opt_4;
        local matched_4 = opt_match_4;
        if matched_4._tag == 1 then
          local value_3 = opt_match_4._0;
          return predicate_1(value_3)
        elseif matched_4._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local get_or = function(opt_5)
      return function(default)
        local opt_match_5 = opt_5;
        local matched_5 = opt_match_5;
        if matched_5._tag == 0 then
          return default
        elseif matched_5._tag == 1 then
          local value_4 = opt_match_5._0;
          return value_4
        else
          return error("Match failure")
        end
      end
    end;

    local get_or_else = function(opt_6)
      return function(compute_default)
        local opt_match_6 = opt_6;
        local matched_6 = opt_match_6;
        if matched_6._tag == 0 then
          return compute_default(nil)
        elseif matched_6._tag == 1 then
          local value_5 = opt_match_6._0;
          return value_5
        else
          return error("Match failure")
        end
      end
    end;

    local function get_exn(opt_7)
      local opt_match_7 = opt_7;
      local matched_7 = opt_match_7;
      if matched_7._tag == 0 then
        if false then
          return nil
        else
          return error("Assertion failed")
        end
      elseif matched_7._tag == 1 then
        local value_6 = opt_match_7._0;
        return value_6
      else
        return error("Match failure")
      end
    end;

    local expect = function(message)
      return function(opt_8)
        local opt_match_8 = opt_8;
        local matched_8 = opt_match_8;
        if matched_8._tag == 0 then
          local _ = print(message);
          if false then
            return nil
          else
            return error("Assertion failed")
          end
        elseif matched_8._tag == 1 then
          local value_7 = opt_match_8._0;
          return value_7
        else
          return error("Match failure")
        end
      end
    end;

    local map = function(f)
      return function(opt_9)
        local opt_match_9 = opt_9;
        local matched_9 = opt_match_9;
        if matched_9._tag == 1 then
          local value_8 = opt_match_9._0;
          return {_tag = 1, _0 = f(value_8)}
        elseif matched_9._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local flat_map = function(f_1)
      return function(opt_10)
        local opt_match_10 = opt_10;
        local matched_10 = opt_match_10;
        if matched_10._tag == 1 then
          local value_9 = opt_match_10._0;
          return f_1(value_9)
        elseif matched_10._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local bind = function(opt_11)
      return function(f_2)
        local opt_match_11 = opt_11;
        local matched_11 = opt_match_11;
        if matched_11._tag == 1 then
          local value_10 = opt_match_11._0;
          return f_2(value_10)
        elseif matched_11._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local filter = function(predicate_2)
      return function(opt_12)
        local opt_match_12 = opt_12;
        local matched_12 = opt_match_12;
        if matched_12._tag == 1 then
          local value_11 = opt_match_12._0;
          if predicate_2(value_11) then
            return {_tag = 1, _0 = value_11}
          else
            return _Ctor_option_0
          end
        elseif matched_12._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local function flatten(opt_13)
      local opt_match_13 = opt_13;
      local matched_13 = opt_match_13;
      if matched_13._tag == 0 then
        return _Ctor_option_0
      elseif matched_13._tag == 1 then
        local inner_1 = opt_match_13._0;
        return inner_1
      else
        return error("Match failure")
      end
    end;

    local function join(opt_14)
      return flatten(opt_14)
    end;

    local or_ = function(opt1)
      return function(opt2)
        local opt1_match = opt1;
        local matched_14 = opt1_match;
        if matched_14._tag == 0 then
          return opt2
        elseif matched_14._tag == 1 then
          return opt1
        else
          return error("Match failure")
        end
      end
    end;

    local or_else = function(opt_15)
      return function(compute_alternative)
        local opt_match_14 = opt_15;
        local matched_15 = opt_match_14;
        if matched_15._tag == 0 then
          return compute_alternative(nil)
        elseif matched_15._tag == 1 then
          return opt_15
        else
          return error("Match failure")
        end
      end
    end;

    local and_ = function(opt1_1)
      return function(opt2_1)
        local opt1_match_1 = opt1_1;
        local matched_16 = opt1_match_1;
        if matched_16._tag == 0 then
          return _Ctor_option_0
        elseif matched_16._tag == 1 then
          return opt2_1
        else
          return error("Match failure")
        end
      end
    end;

    local map2 = function(f_3)
      return function(opt1_2)
        return function(opt2_2)
          local opt1_match_2 = opt1_2;
          local matched_17 = opt1_match_2;
          if matched_17._tag == 1 then
            local value1 = opt1_match_2._0;
            local opt2_match = opt2_2;
            local matched_18 = opt2_match;
            if matched_18._tag == 1 then
              local value2 = opt2_match._0;
              return {_tag = 1, _0 = f_3(value1)(value2)}
            elseif matched_18._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          elseif matched_17._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local zip = function(opt1_3)
      return function(opt2_3)
        local opt1_match_3 = opt1_3;
        local matched_19 = opt1_match_3;
        if matched_19._tag == 1 then
          local value1_1 = opt1_match_3._0;
          local opt2_match_1 = opt2_3;
          local matched_20 = opt2_match_1;
          if matched_20._tag == 1 then
            local value2_1 = opt2_match_1._0;
            return {_tag = 1, _0 = {value1_1, value2_1}}
          elseif matched_20._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        elseif matched_19._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local product = function(opt1_4)
      return function(opt2_4)
        return zip(opt1_4)(opt2_4)
      end
    end;

    local blend = function(merge_fn)
      return function(opt1_5)
        return function(opt2_5)
          local matched_21 = {opt1_5, opt2_5};
          local matched_22 = matched_21[1];
          if matched_22._tag == 1 then
            local matched_23 = matched_21[2];
            if matched_23._tag == 1 then
              local value2_2 = matched_21[2]._0;
              local value1_2 = matched_21[1]._0;
              return {_tag = 1, _0 = merge_fn(value1_2)(value2_2)}
            elseif matched_23._tag == 0 then
              local value_12 = matched_21[1]._0;
              return {_tag = 1, _0 = value_12}
            else
              return error("Match failure")
            end
          elseif matched_22._tag == 0 then
            local matched_24 = matched_21[2];
            if matched_24._tag == 1 then
              local value_13 = matched_21[2]._0;
              return {_tag = 1, _0 = value_13}
            elseif matched_24._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local fold = function(default_value)
      return function(some_fn)
        return function(opt_16)
          local opt_match_15 = opt_16;
          local matched_25 = opt_match_15;
          if matched_25._tag == 1 then
            local value_14 = opt_match_15._0;
            return some_fn(value_14)
          elseif matched_25._tag == 0 then
            return default_value
          else
            return error("Match failure")
          end
        end
      end
    end;

    local iter = function(f_4)
      return function(opt_17)
        local opt_match_16 = opt_17;
        local matched_26 = opt_match_16;
        if matched_26._tag == 0 then
          return nil
        elseif matched_26._tag == 1 then
          local value_15 = opt_match_16._0;
          return f_4(value_15)
        else
          return error("Match failure")
        end
      end
    end;

    local equal = function(eq_fn)
      return function(opt1_6)
        return function(opt2_6)
          local matched_27 = {opt1_6, opt2_6};
          local matched_28 = matched_27[1];
          if matched_28._tag == 1 then
            local matched_29 = matched_27[2];
            if matched_29._tag == 1 then
              local value2_3 = matched_27[2]._0;
              local value1_3 = matched_27[1]._0;
              return eq_fn(value1_3)(value2_3)
            else
              return false
            end
          elseif matched_28._tag == 0 then
            local matched_30 = matched_27[2];
            if matched_30._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;

    local compare = function(cmp_fn)
      return function(opt1_7)
        return function(opt2_7)
          local matched_31 = {opt1_7, opt2_7};
          local matched_32 = matched_31[1];
          if matched_32._tag == 1 then
            local matched_33 = matched_31[2];
            if matched_33._tag == 1 then
              local value2_4 = matched_31[2]._0;
              local value1_4 = matched_31[1]._0;
              return cmp_fn(value1_4)(value2_4)
            elseif matched_33._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_32._tag == 0 then
            local matched_34 = matched_31[2];
            if matched_34._tag == 1 then
              return 0 - 1
            elseif matched_34._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local to_result = function(error_value)
      return function(opt_18)
        local opt_match_17 = opt_18;
        local matched_35 = opt_match_17;
        if matched_35._tag == 0 then
          return {_tag = 1, _0 = error_value}
        elseif matched_35._tag == 1 then
          local value_16 = opt_match_17._0;
          return {_tag = 0, _0 = value_16}
        else
          return error("Match failure")
        end
      end
    end;

    local function of_result(result)
      local result_match = result;
      local matched_36 = result_match;
      if matched_36._tag == 1 then
        return _Ctor_option_0
      elseif matched_36._tag == 0 then
        local value_17 = result_match._0;
        return {_tag = 1, _0 = value_17}
      else
        return error("Match failure")
      end
    end;

    local let_star_ = function(opt_19)
      return function(f_5)
        return flat_map(f_5)(opt_19)
      end
    end;

    local and_star_ = function(opt1_8)
      return function(opt2_8)
        return product(opt1_8)(opt2_8)
      end
    end;

    local let_plus_ = function(opt_20)
      return function(f_6)
        return map(f_6)(opt_20)
      end
    end;
    local and_plus_ = and_star_
    return {["none"] = none, ["some"] = some, ["is_some"] = is_some, ["is_none"] = is_none, ["contains"] = contains, ["for_all"] = for_all, ["exists"] = exists, ["get_or"] = get_or, ["get_or_else"] = get_or_else, ["get_exn"] = get_exn, ["expect"] = expect, ["map"] = map, ["flat_map"] = flat_map, ["bind"] = bind, ["filter"] = filter, ["flatten"] = flatten, ["join"] = join, ["or_"] = or_, ["or_else"] = or_else, ["and_"] = and_, ["map2"] = map2, ["zip"] = zip, ["product"] = product, ["blend"] = blend, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_result"] = to_result, ["of_result"] = of_result, ["let*"] = let_star_, ["and*"] = and_star_, ["let+"] = let_plus_, ["and+"] = and_plus_}
    end)();

    local List = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local empty = _Ctor_list_0;
    local function singleton(x)
      return {_tag = 1, _0 = {x, _Ctor_list_0}}
    end;

    local cons = function(x_1)
      return function(xs)
        return {_tag = 1, _0 = {x_1, xs}}
      end
    end;
    local range;
    range = function(start)
      return function(stop)
        if start > stop then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {start, range(start + 1)(stop)}}
        end
      end
    end;
    local replicate;
    replicate = function(n)
      return function(x_2)
        if n <= 0 then
          return _Ctor_list_0
        else
          return {_tag = 1, _0 = {x_2, replicate(n - 1)(x_2)}}
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local go;
        go = function(i)
          if i >= n_1 then
            return _Ctor_list_0
          else
            return {_tag = 1, _0 = {f(i), go(i + 1)}}
          end
        end;
        return go(0)
      end
    end;

    local function length(lst)
      local go_1;
      go_1 = function(acc)
        return function(xs_1)
          local xs_match = xs_1;
          local matched = xs_match;
          if matched._tag == 1 then
            local rest = xs_match._0[2];
            return go_1(acc + 1)(rest)
          elseif matched._tag == 0 then
            return acc
          else
            return error("Match failure")
          end
        end
      end;
      return go_1(0)(lst)
    end;

    local function is_empty(lst_1)
      local lst_match = lst_1;
      local matched_1 = lst_match;
      if matched_1._tag == 1 then
        return false
      elseif matched_1._tag == 0 then
        return true
      else
        return error("Match failure")
      end
    end;

    local function head(lst_2)
      local lst_match_1 = lst_2;
      local matched_2 = lst_match_1;
      if matched_2._tag == 1 then
        local x_3 = lst_match_1._0[1];
        return {_tag = 1, _0 = x_3}
      elseif matched_2._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;

    local function tail(lst_3)
      local lst_match_2 = lst_3;
      local matched_3 = lst_match_2;
      if matched_3._tag == 1 then
        local xs_2 = lst_match_2._0[2];
        return {_tag = 1, _0 = xs_2}
      elseif matched_3._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local last;
    last = function(lst_4)
      local lst_match_3 = lst_4;
      local matched_4 = lst_match_3;
      if matched_4._tag == 1 then
        local matched_5 = lst_match_3._0[2];
        if matched_5._tag == 0 then
          local x_4 = lst_match_3._0[1];
          return {_tag = 1, _0 = x_4}
        else
          local xs_3 = lst_match_3._0[2];
          return last(xs_3)
        end
      elseif matched_4._tag == 0 then
        return _Ctor_option_0
      else
        return error("Match failure")
      end
    end;
    local nth;
    nth = function(n_2)
      return function(lst_5)
        if n_2 < 0 then
          return _Ctor_option_0
        else
          local lst_match_4 = lst_5;
          local matched_6 = lst_match_4;
          if matched_6._tag == 1 then
            local xs_4 = lst_match_4._0[2];
            local x_5 = lst_match_4._0[1];
            if n_2 == 0 then
              return {_tag = 1, _0 = x_5}
            else
              return nth(n_2 - 1)(xs_4)
            end
          elseif matched_6._tag == 0 then
            return _Ctor_option_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local map;
    map = function(f_1)
      return function(lst_6)
        local lst_match_5 = lst_6;
        local matched_7 = lst_match_5;
        if matched_7._tag == 1 then
          local xs_5 = lst_match_5._0[2];
          local x_6 = lst_match_5._0[1];
          return {_tag = 1, _0 = {f_1(x_6), map(f_1)(xs_5)}}
        elseif matched_7._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local mapi = function(f_2)
      return function(lst_7)
        local go_2;
        go_2 = function(i_1)
          return function(xs_6)
            local xs_match_1 = xs_6;
            local matched_8 = xs_match_1;
            if matched_8._tag == 1 then
              local rest_1 = xs_match_1._0[2];
              local x_7 = xs_match_1._0[1];
              return {_tag = 1, _0 = {f_2(i_1)(x_7), go_2(i_1 + 1)(rest_1)}}
            elseif matched_8._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_2(0)(lst_7)
      end
    end;
    local filter;
    filter = function(predicate)
      return function(lst_8)
        local lst_match_6 = lst_8;
        local matched_9 = lst_match_6;
        if matched_9._tag == 1 then
          local xs_7 = lst_match_6._0[2];
          local x_8 = lst_match_6._0[1];
          if predicate(x_8) then
            return {_tag = 1, _0 = {x_8, filter(predicate)(xs_7)}}
          else
            return filter(predicate)(xs_7)
          end
        elseif matched_9._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;
    local filter_map;
    filter_map = function(f_3)
      return function(lst_9)
        local lst_match_7 = lst_9;
        local matched_10 = lst_match_7;
        if matched_10._tag == 1 then
          local xs_8 = lst_match_7._0[2];
          local x_9 = lst_match_7._0[1];
          local matched_11 = f_3(x_9);
          local matched_12 = matched_11;
          if matched_12._tag == 1 then
            local y = matched_11._0;
            return {_tag = 1, _0 = {y, filter_map(f_3)(xs_8)}}
          elseif matched_12._tag == 0 then
            return filter_map(f_3)(xs_8)
          else
            return error("Match failure")
          end
        elseif matched_10._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end;

    local function reverse(lst_10)
      local go_3;
      go_3 = function(acc_1)
        return function(xs_9)
          local xs_match_2 = xs_9;
          local matched_13 = xs_match_2;
          if matched_13._tag == 1 then
            local rest_2 = xs_match_2._0[2];
            local x_10 = xs_match_2._0[1];
            return go_3({_tag = 1, _0 = {x_10, acc_1}})(rest_2)
          elseif matched_13._tag == 0 then
            return acc_1
          else
            return error("Match failure")
          end
        end
      end;
      return go_3(_Ctor_list_0)(lst_10)
    end;
    local append;
    append = function(lst1)
      return function(lst2)
        local lst1_match = lst1;
        local matched_14 = lst1_match;
        if matched_14._tag == 1 then
          local xs_10 = lst1_match._0[2];
          local x_11 = lst1_match._0[1];
          return {_tag = 1, _0 = {x_11, append(xs_10)(lst2)}}
        elseif matched_14._tag == 0 then
          return lst2
        else
          return error("Match failure")
        end
      end
    end;
    local concat;
    concat = function(lists)
      local lists_match = lists;
      local matched_15 = lists_match;
      if matched_15._tag == 1 then
        local xs_11 = lists_match._0[2];
        local x_12 = lists_match._0[1];
        return append(x_12)(concat(xs_11))
      elseif matched_15._tag == 0 then
        return _Ctor_list_0
      else
        return error("Match failure")
      end
    end;

    local flat_map = function(f_4)
      return function(lst_11)
        return concat(map(f_4)(lst_11))
      end
    end;
    local fold_left;
    fold_left = function(f_5)
      return function(acc_2)
        return function(lst_12)
          local lst_match_8 = lst_12;
          local matched_16 = lst_match_8;
          if matched_16._tag == 1 then
            local xs_12 = lst_match_8._0[2];
            local x_13 = lst_match_8._0[1];
            return fold_left(f_5)(f_5(acc_2)(x_13))(xs_12)
          elseif matched_16._tag == 0 then
            return acc_2
          else
            return error("Match failure")
          end
        end
      end
    end;
    local fold_right;
    fold_right = function(f_6)
      return function(lst_13)
        return function(acc_3)
          local lst_match_9 = lst_13;
          local matched_17 = lst_match_9;
          if matched_17._tag == 1 then
            local xs_13 = lst_match_9._0[2];
            local x_14 = lst_match_9._0[1];
            return f_6(x_14)(fold_right(f_6)(xs_13)(acc_3))
          elseif matched_17._tag == 0 then
            return acc_3
          else
            return error("Match failure")
          end
        end
      end
    end;
    local find;
    find = function(predicate_1)
      return function(lst_14)
        local lst_match_10 = lst_14;
        local matched_18 = lst_match_10;
        if matched_18._tag == 1 then
          local xs_14 = lst_match_10._0[2];
          local x_15 = lst_match_10._0[1];
          if predicate_1(x_15) then
            return {_tag = 1, _0 = x_15}
          else
            return find(predicate_1)(xs_14)
          end
        elseif matched_18._tag == 0 then
          return _Ctor_option_0
        else
          return error("Match failure")
        end
      end
    end;

    local find_index = function(predicate_2)
      return function(lst_15)
        local go_4;
        go_4 = function(i_2)
          return function(xs_15)
            local xs_match_3 = xs_15;
            local matched_19 = xs_match_3;
            if matched_19._tag == 1 then
              local rest_3 = xs_match_3._0[2];
              local x_16 = xs_match_3._0[1];
              if predicate_2(x_16) then
                return {_tag = 1, _0 = i_2}
              else
                return go_4(i_2 + 1)(rest_3)
              end
            elseif matched_19._tag == 0 then
              return _Ctor_option_0
            else
              return error("Match failure")
            end
          end
        end;
        return go_4(0)(lst_15)
      end
    end;
    local exists;
    exists = function(predicate_3)
      return function(lst_16)
        local lst_match_11 = lst_16;
        local matched_20 = lst_match_11;
        if matched_20._tag == 1 then
          local xs_16 = lst_match_11._0[2];
          local x_17 = lst_match_11._0[1];
          if predicate_3(x_17) then
            return true
          else
            return exists(predicate_3)(xs_16)
          end
        elseif matched_20._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;
    local for_all;
    for_all = function(predicate_4)
      return function(lst_17)
        local lst_match_12 = lst_17;
        local matched_21 = lst_match_12;
        if matched_21._tag == 1 then
          local xs_17 = lst_match_12._0[2];
          local x_18 = lst_match_12._0[1];
          if predicate_4(x_18) then
            return for_all(predicate_4)(xs_17)
          else
            return false
          end
        elseif matched_21._tag == 0 then
          return true
        else
          return error("Match failure")
        end
      end
    end;
    local mem;
    mem = function(element)
      return function(lst_18)
        local lst_match_13 = lst_18;
        local matched_22 = lst_match_13;
        if matched_22._tag == 1 then
          local xs_18 = lst_match_13._0[2];
          local x_19 = lst_match_13._0[1];
          if x_19 == element then
            return true
          else
            return mem(element)(xs_18)
          end
        elseif matched_22._tag == 0 then
          return false
        else
          return error("Match failure")
        end
      end
    end;

    local function split_half(lst_19)
      local go_5;
      go_5 = function(slow)
        return function(fast)
          local fast_match = fast;
          local matched_23 = fast_match;
          if matched_23._tag == 1 then
            local matched_24 = fast_match._0[2];
            if matched_24._tag == 1 then
              local fast_rest = fast_match._0[2]._0[2];
              local slow_match = slow;
              local matched_25 = slow_match;
              if matched_25._tag == 1 then
                local slow_rest = slow_match._0[2];
                local y_1 = slow_match._0[1];
                local tuple = go_5(slow_rest)(fast_rest);
                local left = tuple[1];
                local right = tuple[2];
                return {{_tag = 1, _0 = {y_1, left}}, right}
              elseif matched_25._tag == 0 then
                return {_Ctor_list_0, _Ctor_list_0}
              else
                return error("Match failure")
              end
            elseif matched_24._tag == 0 then
              return {_Ctor_list_0, slow}
            else
              return error("Match failure")
            end
          elseif matched_23._tag == 0 then
            return {_Ctor_list_0, slow}
          else
            return error("Match failure")
          end
        end
      end;
      return go_5(lst_19)(lst_19)
    end;
    local merge;
    merge = function(cmp)
      return function(lst1_1)
        return function(lst2_1)
          local matched_26 = {lst1_1, lst2_1};
          local matched_27 = matched_26[1];
          if matched_27._tag == 1 then
            local matched_28 = matched_26[2];
            if matched_28._tag == 1 then
              local ys = matched_26[2]._0[2];
              local y_2 = matched_26[2]._0[1];
              local xs_19 = matched_26[1]._0[2];
              local x_20 = matched_26[1]._0[1];
              if cmp(x_20)(y_2) <= 0 then
                return {_tag = 1, _0 = {x_20, merge(cmp)(xs_19)(lst2_1)}}
              else
                return {_tag = 1, _0 = {y_2, merge(cmp)(lst1_1)(ys)}}
              end
            elseif matched_28._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          elseif matched_27._tag == 0 then
            local matched_29 = matched_26[2];
            if matched_29._tag == 0 then
              local ys_1 = matched_26[2];
              return ys_1
            else
              local ys_1 = matched_26[2];
              return ys_1
            end
          else
            local matched_30 = matched_26[2];
            if matched_30._tag == 0 then
              local xs_20 = matched_26[1];
              return xs_20
            else
              return error("Match failure")
            end
          end
        end
      end
    end;
    local sort;
    sort = function(cmp_1)
      return function(lst_20)
        local lst_match_14 = lst_20;
        local matched_31 = lst_match_14;
        if matched_31._tag == 1 then
          local matched_32 = lst_match_14._0[2];
          if matched_32._tag == 0 then
            return lst_20
          else
            local tuple_1 = split_half(lst_20);
            local left_1 = tuple_1[1];
            local right_1 = tuple_1[2];
            return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
          end
        elseif matched_31._tag == 0 then
          return _Ctor_list_0
        else
          local tuple_2 = split_half(lst_20);
          local left_1 = tuple_2[1];
          local right_1 = tuple_2[2];
          return merge(cmp_1)(sort(cmp_1)(left_1))(sort(cmp_1)(right_1))
        end
      end
    end;

    local sort_by = function(key_fn)
      return function(lst_21)
        return sort(function(a)
          return function(b)
            local ka = key_fn(a);
            local kb = key_fn(b);
            if ka < kb then
              return 0 - 1
            else
              if ka > kb then
                return 1
              else
                return 0
              end
            end
          end
        end)(lst_21)
      end
    end;
    local iter;
    iter = function(f_7)
      return function(lst_22)
        local lst_match_15 = lst_22;
        local matched_33 = lst_match_15;
        if matched_33._tag == 1 then
          local xs_21 = lst_match_15._0[2];
          local x_21 = lst_match_15._0[1];
          local _ = f_7(x_21);
          return iter(f_7)(xs_21)
        elseif matched_33._tag == 0 then
          return nil
        else
          return error("Match failure")
        end
      end
    end;

    local iteri = function(f_8)
      return function(lst_23)
        local go_6;
        go_6 = function(i_3)
          return function(xs_22)
            local xs_match_4 = xs_22;
            local matched_34 = xs_match_4;
            if matched_34._tag == 1 then
              local rest_4 = xs_match_4._0[2];
              local x_22 = xs_match_4._0[1];
              local __1 = f_8(i_3)(x_22);
              return go_6(i_3 + 1)(rest_4)
            elseif matched_34._tag == 0 then
              return nil
            else
              return error("Match failure")
            end
          end
        end;
        return go_6(0)(lst_23)
      end
    end;
    local zip;
    zip = function(lst1_2)
      return function(lst2_2)
        local matched_35 = {lst1_2, lst2_2};
        local matched_36 = matched_35[1];
        if matched_36._tag == 1 then
          local matched_37 = matched_35[2];
          if matched_37._tag == 1 then
            local ys_2 = matched_35[2]._0[2];
            local y_3 = matched_35[2]._0[1];
            local xs_23 = matched_35[1]._0[2];
            local x_23 = matched_35[1]._0[1];
            return {_tag = 1, _0 = {{x_23, y_3}, zip(xs_23)(ys_2)}}
          elseif matched_37._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        elseif matched_36._tag == 0 then
          local matched_38 = matched_35[2];
          if matched_38._tag == 0 then
            return _Ctor_list_0
          else
            return _Ctor_list_0
          end
        else
          local matched_39 = matched_35[2];
          if matched_39._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local function unzip(lst_24)
      local go_7;
      go_7 = function(acc1)
        return function(acc2)
          return function(xs_24)
            local xs_match_5 = xs_24;
            local matched_40 = xs_match_5;
            if matched_40._tag == 1 then
              local rest_5 = xs_match_5._0[2];
              local b_1 = xs_match_5._0[1][2];
              local a_1 = xs_match_5._0[1][1];
              return go_7({_tag = 1, _0 = {a_1, acc1}})({_tag = 1, _0 = {b_1, acc2}})(rest_5)
            elseif matched_40._tag == 0 then
              return {reverse(acc1), reverse(acc2)}
            else
              return error("Match failure")
            end
          end
        end
      end;
      return go_7(_Ctor_list_0)(_Ctor_list_0)(lst_24)
    end;
    local equal;
    equal = function(eq_fn)
      return function(lst1_3)
        return function(lst2_3)
          local matched_41 = {lst1_3, lst2_3};
          local matched_42 = matched_41[1];
          if matched_42._tag == 1 then
            local matched_43 = matched_41[2];
            if matched_43._tag == 1 then
              local ys_3 = matched_41[2]._0[2];
              local y_4 = matched_41[2]._0[1];
              local xs_25 = matched_41[1]._0[2];
              local x_24 = matched_41[1]._0[1];
              if eq_fn(x_24)(y_4) then
                return equal(eq_fn)(xs_25)(ys_3)
              else
                return false
              end
            else
              return false
            end
          elseif matched_42._tag == 0 then
            local matched_44 = matched_41[2];
            if matched_44._tag == 0 then
              return true
            else
              return false
            end
          else
            return false
          end
        end
      end
    end;
    local compare;
    compare = function(cmp_fn)
      return function(lst1_4)
        return function(lst2_4)
          local matched_45 = {lst1_4, lst2_4};
          local matched_46 = matched_45[1];
          if matched_46._tag == 1 then
            local matched_47 = matched_45[2];
            if matched_47._tag == 1 then
              local ys_4 = matched_45[2]._0[2];
              local y_5 = matched_45[2]._0[1];
              local xs_26 = matched_45[1]._0[2];
              local x_25 = matched_45[1]._0[1];
              local c = cmp_fn(x_25)(y_5);
              if c ~= 0 then
                return c
              else
                return compare(cmp_fn)(xs_26)(ys_4)
              end
            elseif matched_47._tag == 0 then
              return 1
            else
              return error("Match failure")
            end
          elseif matched_46._tag == 0 then
            local matched_48 = matched_45[2];
            if matched_48._tag == 1 then
              return 0 - 1
            elseif matched_48._tag == 0 then
              return 0
            else
              return error("Match failure")
            end
          else
            return error("Match failure")
          end
        end
      end
    end;
    local take;
    take = function(n_3)
      return function(lst_25)
        if n_3 <= 0 then
          return _Ctor_list_0
        else
          local lst_match_16 = lst_25;
          local matched_49 = lst_match_16;
          if matched_49._tag == 1 then
            local xs_27 = lst_match_16._0[2];
            local x_26 = lst_match_16._0[1];
            return {_tag = 1, _0 = {x_26, take(n_3 - 1)(xs_27)}}
          elseif matched_49._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;
    local drop;
    drop = function(n_4)
      return function(lst_26)
        if n_4 <= 0 then
          return lst_26
        else
          local lst_match_17 = lst_26;
          local matched_50 = lst_match_17;
          if matched_50._tag == 1 then
            local xs_28 = lst_match_17._0[2];
            return drop(n_4 - 1)(xs_28)
          elseif matched_50._tag == 0 then
            return _Ctor_list_0
          else
            return error("Match failure")
          end
        end
      end
    end;

    local split_at = function(n_5)
      return function(lst_27)
        return {take(n_5)(lst_27), drop(n_5)(lst_27)}
      end
    end;

    local partition = function(predicate_5)
      return function(lst_28)
        local go_8;
        go_8 = function(yes)
          return function(no)
            return function(xs_29)
              local xs_match_6 = xs_29;
              local matched_51 = xs_match_6;
              if matched_51._tag == 1 then
                local rest_6 = xs_match_6._0[2];
                local x_27 = xs_match_6._0[1];
                if predicate_5(x_27) then
                  return go_8({_tag = 1, _0 = {x_27, yes}})(no)(rest_6)
                else
                  return go_8(yes)({_tag = 1, _0 = {x_27, no}})(rest_6)
                end
              elseif matched_51._tag == 0 then
                return {reverse(yes), reverse(no)}
              else
                return error("Match failure")
              end
            end
          end
        end;
        return go_8(_Ctor_list_0)(_Ctor_list_0)(lst_28)
      end
    end;

    local intersperse = function(separator)
      return function(lst_29)
        local lst_match_18 = lst_29;
        local matched_52 = lst_match_18;
        if matched_52._tag == 1 then
          local xs_30 = lst_match_18._0[2];
          local x_28 = lst_match_18._0[1];
          local go_9;
          go_9 = function(ys_5)
            local ys_match = ys_5;
            local matched_53 = ys_match;
            if matched_53._tag == 1 then
              local rest_7 = ys_match._0[2];
              local y_6 = ys_match._0[1];
              return {_tag = 1, _0 = {separator, {_tag = 1, _0 = {y_6, go_9(rest_7)}}}}
            elseif matched_53._tag == 0 then
              return _Ctor_list_0
            else
              return error("Match failure")
            end
          end;
          return {_tag = 1, _0 = {x_28, go_9(xs_30)}}
        elseif matched_52._tag == 0 then
          return _Ctor_list_0
        else
          return error("Match failure")
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["cons"] = cons, ["range"] = range, ["replicate"] = replicate, ["init"] = init, ["length"] = length, ["is_empty"] = is_empty, ["head"] = head, ["tail"] = tail, ["last"] = last, ["nth"] = nth, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["reverse"] = reverse, ["append"] = append, ["concat"] = concat, ["flat_map"] = flat_map, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["find"] = find, ["find_index"] = find_index, ["exists"] = exists, ["for_all"] = for_all, ["mem"] = mem, ["split_half"] = split_half, ["merge"] = merge, ["sort"] = sort, ["sort_by"] = sort_by, ["iter"] = iter, ["iteri"] = iteri, ["zip"] = zip, ["unzip"] = unzip, ["equal"] = equal, ["compare"] = compare, ["take"] = take, ["drop"] = drop, ["split_at"] = split_at, ["partition"] = partition, ["intersperse"] = intersperse}
    end)();
    local _Ctor_option_0 = {_tag = 0};
    local function f(b)
      if b then
        return _Ctor_option_0
      else
        return _Ctor_option_0
      end
    end
    |}]
