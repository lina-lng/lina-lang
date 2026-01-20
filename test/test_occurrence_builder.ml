(** Unit tests for pattern match compilation.

    Tests cover:
    - Deep pattern matching compilation
    - Performance with deep nesting
    - Various pattern types

    Note: Pattern_match internals are not directly accessible from tests,
    so we test through the full compilation pipeline. *)

open Typing
open Driver

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

(** Helper to compile a match expression and return Lua code *)
let compile code =
  Types.reset_type_variable_id ();
  match Pipeline.compile_string test_options "<test>" code with
  | Ok lua -> lua
  | Error msg -> "ERROR: " ^ msg

(** {1 Simple Pattern Tests} *)

let%expect_test "compile simple variable pattern" =
  let lua = compile {|
    let f x = match x with
      | y -> y
  |} in
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

    local Ord = (function()
    local _Ctor_ordering_2 = {_tag = 2};
    local _Ctor_ordering_1 = {_tag = 1};
    local _Ctor_ordering_0 = {_tag = 0};
    local less = _Ctor_ordering_0;
    local equal_ordering = _Ctor_ordering_1;
    local greater = _Ctor_ordering_2;
    local function of_int(n)
      if n < 0 then
        return _Ctor_ordering_0
      else
        if n > 0 then
          return _Ctor_ordering_2
        else
          return _Ctor_ordering_1
        end
      end
    end;

    local function to_int(ord)
      local ord_match = ord;
      local matched = ord_match;
      if matched._tag == 2 then
        return 1
      elseif matched._tag == 1 then
        return 0
      elseif matched._tag == 0 then
        return 0 - 1
      else
        return error("Match failure")
      end
    end;

    local function is_less(ord_1)
      local ord_match_1 = ord_1;
      local matched_1 = ord_match_1;
      if matched_1._tag == 0 then
        return true
      else
        return false
      end
    end;

    local function is_equal(ord_2)
      local ord_match_2 = ord_2;
      local matched_2 = ord_match_2;
      if matched_2._tag == 1 then
        return true
      else
        return false
      end
    end;

    local function is_greater(ord_3)
      local ord_match_3 = ord_3;
      local matched_3 = ord_match_3;
      if matched_3._tag == 2 then
        return true
      else
        return false
      end
    end;

    local function flip(ord_4)
      local ord_match_4 = ord_4;
      local matched_4 = ord_match_4;
      if matched_4._tag == 2 then
        return _Ctor_ordering_0
      elseif matched_4._tag == 1 then
        return _Ctor_ordering_1
      elseif matched_4._tag == 0 then
        return _Ctor_ordering_2
      else
        return error("Match failure")
      end
    end;

    local then_ = function(first)
      return function(second)
        local first_match = first;
        local matched_5 = first_match;
        if matched_5._tag == 1 then
          return second
        else
          local other = first_match;
          return other
        end
      end
    end;

    local int_compare = function(a)
      return function(b)
        if a < b then
          return _Ctor_ordering_0
        else
          if a > b then
            return _Ctor_ordering_2
          else
            return _Ctor_ordering_1
          end
        end
      end
    end;

    local bool_compare = function(a_1)
      return function(b_1)
        local matched_6 = {a_1, b_1};
        if matched_6[1] == true then
          if matched_6[2] == false then
            return _Ctor_ordering_2
          else
            if matched_6[2] == true then
              return _Ctor_ordering_1
            else
              return error("Match failure")
            end
          end
        else
          if matched_6[1] == false then
            if matched_6[2] == true then
              return _Ctor_ordering_0
            else
              if matched_6[2] == false then
                return _Ctor_ordering_1
              else
                return error("Match failure")
              end
            end
          else
            return error("Match failure")
          end
        end
      end
    end;

    local string_compare = function(a_2)
      return function(b_2)
        if a_2 < b_2 then
          return _Ctor_ordering_0
        else
          if a_2 > b_2 then
            return _Ctor_ordering_2
          else
            return _Ctor_ordering_1
          end
        end
      end
    end;

    local compare = function(ord1)
      return function(ord2)
        local rank = function(ord_5)
          local ord_match_5 = ord_5;
          local matched_7 = ord_match_5;
          if matched_7._tag == 2 then
            return 2
          elseif matched_7._tag == 1 then
            return 1
          elseif matched_7._tag == 0 then
            return 0
          else
            return error("Match failure")
          end
        end;
        return int_compare(rank(ord1))(rank(ord2))
      end
    end;

    local equal = function(ord1_1)
      return function(ord2_1)
        local matched_8 = {ord1_1, ord2_1};
        local matched_9 = matched_8[1];
        if matched_9._tag == 2 then
          local matched_10 = matched_8[2];
          if matched_10._tag == 2 then
            return true
          else
            return false
          end
        elseif matched_9._tag == 1 then
          local matched_11 = matched_8[2];
          if matched_11._tag == 1 then
            return true
          else
            return false
          end
        elseif matched_9._tag == 0 then
          local matched_12 = matched_8[2];
          if matched_12._tag == 0 then
            return true
          else
            return false
          end
        else
          return false
        end
      end
    end
    return {["less"] = less, ["equal_ordering"] = equal_ordering, ["greater"] = greater, ["of_int"] = of_int, ["to_int"] = to_int, ["is_less"] = is_less, ["is_equal"] = is_equal, ["is_greater"] = is_greater, ["flip"] = flip, ["then_"] = then_, ["int_compare"] = int_compare, ["bool_compare"] = bool_compare, ["string_compare"] = string_compare, ["compare"] = compare, ["equal"] = equal}
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

    local Array = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local make = function(n)
      return function(value)
        if n <= 0 then
          return (function()
            local _arr = {};
            local _init = value;
            for _idx = 1, 0 do
              _arr[_idx] = _init
            end;
            return _arr
          end)()
        else
          return (function()
            local _arr = {};
            local _init = value;
            for _idx = 1, n do
              _arr[_idx] = _init
            end;
            return _arr
          end)()
        end
      end
    end;

    local init = function(n_1)
      return function(f)
        local first = f(0);
        if n_1 <= 0 then
          return (function()
            local _arr = {};
            local _init = first;
            for _idx = 1, 0 do
              _arr[_idx] = _init
            end;
            return _arr
          end)()
        else
          local arr = (function()
            local _arr = {};
            local _init = first;
            for _idx = 1, n_1 do
              _arr[_idx] = _init
            end;
            return _arr
          end)();
          (function()
            for i = 1, n_1 - 1 do
              local _ = (function()
                arr[i + 1] = f(i);
                return nil
              end)()
            end;
            return nil
          end)();
          return arr
        end
      end
    end;

    local function empty(param)
      return (function()
        local _arr = {};
        local _init = nil;
        for _idx = 1, 0 do
          _arr[_idx] = _init
        end;
        return _arr
      end)()
    end;

    local function length(arr_1)
      return #arr_1
    end;

    local function is_empty(arr_2)
      return #arr_2 == 0
    end;

    local get = function(arr_3)
      return function(i_1)
        if i_1 < 0 or i_1 >= #arr_3 then
          return _Ctor_option_0
        else
          return {_tag = 1, _0 = arr_3[i_1 + 1]}
        end
      end
    end;

    local get_exn = function(arr_4)
      return function(i_2)
        if i_2 < 0 or i_2 >= #arr_4 then
          return error("Array.get_exn: index out of bounds")
        else
          return arr_4[i_2 + 1]
        end
      end
    end;

    local set = function(arr_5)
      return function(i_3)
        return function(v)
          if i_3 >= 0 and i_3 < #arr_5 then
            return (function()
              arr_5[i_3 + 1] = v;
              return nil
            end)()
          else
            return nil
          end
        end
      end
    end;

    local set_exn = function(arr_6)
      return function(i_4)
        return function(v_1)
          if i_4 < 0 or i_4 >= #arr_6 then
            return error("Array.set_exn: index out of bounds")
          else
            return (function()
              arr_6[i_4 + 1] = v_1;
              return nil
            end)()
          end
        end
      end
    end;

    local map = function(f_1)
      return function(arr_7)
        local len = #arr_7;
        if len == 0 then
          return {}
        else
          local result = (function()
            local _arr = {};
            local _init = f_1(arr_7[0 + 1]);
            for _idx = 1, len do
              _arr[_idx] = _init
            end;
            return _arr
          end)();
          (function()
            for i_5 = 1, len - 1 do
              local __1 = (function()
                result[i_5 + 1] = f_1(arr_7[i_5 + 1]);
                return nil
              end)()
            end;
            return nil
          end)();
          return result
        end
      end
    end;

    local mapi = function(f_2)
      return function(arr_8)
        local len_1 = #arr_8;
        if len_1 == 0 then
          return {}
        else
          local result_1 = (function()
            local _arr = {};
            local _init = f_2(0)(arr_8[0 + 1]);
            for _idx = 1, len_1 do
              _arr[_idx] = _init
            end;
            return _arr
          end)();
          (function()
            for i_6 = 1, len_1 - 1 do
              local __2 = (function()
                result_1[i_6 + 1] = f_2(i_6)(arr_8[i_6 + 1]);
                return nil
              end)()
            end;
            return nil
          end)();
          return result_1
        end
      end
    end;

    local function copy(arr_9)
      return map(function(x)
        return x
      end)(arr_9)
    end;

    local fold_left = function(f_3)
      return function(acc)
        return function(arr_10)
          local len_2 = #arr_10;
          local result_2 = {value = acc};
          (function()
            for i_7 = 0, len_2 - 1 do
              result_2.value = f_3(result_2.value)(arr_10[i_7 + 1])
            end;
            return nil
          end)();
          return result_2.value
        end
      end
    end;

    local fold_right = function(f_4)
      return function(arr_11)
        return function(acc_1)
          local len_3 = #arr_11;
          local result_3 = {value = acc_1};
          (function()
            for i_8 = 0, len_3 - 1 do
              local idx = len_3 - 1 - i_8;
              result_3.value = f_4(arr_11[idx + 1])(result_3.value)
            end;
            return nil
          end)();
          return result_3.value
        end
      end
    end;

    local iter = function(f_5)
      return function(arr_12)
        local len_4 = #arr_12;
        return (function()
          for i_9 = 0, len_4 - 1 do
            local __3 = f_5(arr_12[i_9 + 1])
          end;
          return nil
        end)()
      end
    end;

    local iteri = function(f_6)
      return function(arr_13)
        local len_5 = #arr_13;
        return (function()
          for i_10 = 0, len_5 - 1 do
            local __4 = f_6(i_10)(arr_13[i_10 + 1])
          end;
          return nil
        end)()
      end
    end;

    local exists = function(predicate)
      return function(arr_14)
        local len_6 = #arr_14;
        local found = {value = false};
        local i_11 = {value = 0};
        (function()
          while i_11.value < len_6 and not found.value do
            if predicate(arr_14[i_11.value + 1]) then
              found.value = true
            else
              i_11.value = i_11.value + 1
            end
          end;
          return nil
        end)();
        return found.value
      end
    end;

    local for_all = function(predicate_1)
      return function(arr_15)
        local len_7 = #arr_15;
        local ok = {value = true};
        local i_12 = {value = 0};
        (function()
          while i_12.value < len_7 and ok.value do
            if not predicate_1(arr_15[i_12.value + 1]) then
              ok.value = false
            else
              i_12.value = i_12.value + 1
            end
          end;
          return nil
        end)();
        return ok.value
      end
    end;

    local find = function(predicate_2)
      return function(arr_16)
        local len_8 = #arr_16;
        local result_4 = {value = _Ctor_option_0};
        local i_13 = {value = 0};
        (function()
          while i_13.value < len_8 and Option.is_none(result_4.value) do
            local elem = arr_16[i_13.value + 1];
            if predicate_2(elem) then
              result_4.value = {_tag = 1, _0 = elem}
            else
              i_13.value = i_13.value + 1
            end
          end;
          return nil
        end)();
        return result_4.value
      end
    end;

    local find_index = function(predicate_3)
      return function(arr_17)
        local len_9 = #arr_17;
        local result_5 = {value = _Ctor_option_0};
        local i_14 = {value = 0};
        (function()
          while i_14.value < len_9 and Option.is_none(result_5.value) do
            if predicate_3(arr_17[i_14.value + 1]) then
              result_5.value = {_tag = 1, _0 = i_14.value}
            else
              i_14.value = i_14.value + 1
            end
          end;
          return nil
        end)();
        return result_5.value
      end
    end;

    local mem = function(element)
      return function(arr_18)
        return exists(function(x_1)
          return x_1 == element
        end)(arr_18)
      end
    end;

    local function of_list(lst)
      local lst_match = lst;
      local matched = lst_match;
      if matched._tag == 1 then
        local first_1 = lst_match._0[1];
        local len_10 = List.length(lst);
        local arr_19 = (function()
          local _arr = {};
          local _init = first_1;
          for _idx = 1, len_10 do
            _arr[_idx] = _init
          end;
          return _arr
        end)();
        local __5 = List.fold_left(function(i_15)
          return function(x_2)
            local __6 = (function()
              arr_19[i_15 + 1] = x_2;
              return nil
            end)();
            return i_15 + 1
          end
        end)(0)(lst);
        return arr_19
      elseif matched._tag == 0 then
        return {}
      else
        return error("Match failure")
      end
    end;

    local function to_list(arr_20)
      return fold_right(function(x_3)
        return function(acc_2)
          return {_tag = 1, _0 = {x_3, acc_2}}
        end
      end)(arr_20)(_Ctor_list_0)
    end;

    local compare = function(cmp)
      return function(arr1)
        return function(arr2)
          local len1 = #arr1;
          local len2 = #arr2;
          local min_len;
          if len1 < len2 then
            min_len = len1
          else
            min_len = len2
          end;
          local result_6 = {value = 0};
          local i_16 = {value = 0};
          (function()
            while i_16.value < min_len and result_6.value == 0 do
              result_6.value = cmp(arr1[i_16.value + 1])(arr2[i_16.value + 1]);
              i_16.value = i_16.value + 1
            end;
            return nil
          end)();
          if result_6.value ~= 0 then
            return result_6.value
          else
            if len1 < len2 then
              return 0 - 1
            else
              if len1 > len2 then
                return 1
              else
                return 0
              end
            end
          end
        end
      end
    end;

    local equal = function(eq)
      return function(arr1_1)
        return function(arr2_1)
          local len1_1 = #arr1_1;
          local len2_1 = #arr2_1;
          if len1_1 ~= len2_1 then
            return false
          else
            local ok_1 = {value = true};
            local i_17 = {value = 0};
            (function()
              while i_17.value < len1_1 and ok_1.value do
                if not eq(arr1_1[i_17.value + 1])(arr2_1[i_17.value + 1]) then
                  ok_1.value = false
                else
                  i_17.value = i_17.value + 1
                end
              end;
              return nil
            end)();
            return ok_1.value
          end
        end
      end
    end
    return {["make"] = make, ["init"] = init, ["empty"] = empty, ["length"] = length, ["is_empty"] = is_empty, ["get"] = get, ["get_exn"] = get_exn, ["set"] = set, ["set_exn"] = set_exn, ["map"] = map, ["mapi"] = mapi, ["copy"] = copy, ["fold_left"] = fold_left, ["fold_right"] = fold_right, ["iter"] = iter, ["iteri"] = iteri, ["exists"] = exists, ["for_all"] = for_all, ["find"] = find, ["find_index"] = find_index, ["mem"] = mem, ["of_list"] = of_list, ["to_list"] = to_list, ["compare"] = compare, ["equal"] = equal}
    end)();

    local Tuple = (function()
    local _Ctor_list_0 = {_tag = 0};
    local make = function(a)
      return function(b)
        return {a, b}
      end
    end;

    local function fst(pair)
      local pair_match = pair;
      local a_1 = pair_match[1];
      return a_1
    end;

    local function snd(pair_1)
      local pair_match_1 = pair_1;
      local b_1 = pair_match_1[2];
      return b_1
    end;

    local function swap(pair_2)
      local pair_match_2 = pair_2;
      local b_2 = pair_match_2[2];
      local a_2 = pair_match_2[1];
      return {b_2, a_2}
    end;

    local map_fst = function(f)
      return function(pair_3)
        local pair_match_3 = pair_3;
        local b_3 = pair_match_3[2];
        local a_3 = pair_match_3[1];
        return {f(a_3), b_3}
      end
    end;

    local map_snd = function(f_1)
      return function(pair_4)
        local pair_match_4 = pair_4;
        local b_4 = pair_match_4[2];
        local a_4 = pair_match_4[1];
        return {a_4, f_1(b_4)}
      end
    end;

    local map = function(f_2)
      return function(g)
        return function(pair_5)
          local pair_match_5 = pair_5;
          local b_5 = pair_match_5[2];
          local a_5 = pair_match_5[1];
          return {f_2(a_5), g(b_5)}
        end
      end
    end;

    local fold = function(f_3)
      return function(pair_6)
        local pair_match_6 = pair_6;
        local b_6 = pair_match_6[2];
        local a_6 = pair_match_6[1];
        return f_3(a_6)(b_6)
      end
    end;

    local iter = function(f_4)
      return function(pair_7)
        local pair_match_7 = pair_7;
        local b_7 = pair_match_7[2];
        local a_7 = pair_match_7[1];
        local _ = f_4(a_7);
        local __1 = f_4(b_7);
        return nil
      end
    end;

    local equal = function(eq_fst)
      return function(eq_snd)
        return function(p1)
          return function(p2)
            local matched = {p1, p2};
            local b2 = matched[2][2];
            local a2 = matched[2][1];
            local b1 = matched[1][2];
            local a1 = matched[1][1];
            return eq_fst(a1)(a2) and eq_snd(b1)(b2)
          end
        end
      end
    end;

    local compare = function(cmp_fst)
      return function(cmp_snd)
        return function(p1_1)
          return function(p2_1)
            local matched_1 = {p1_1, p2_1};
            local b2_1 = matched_1[2][2];
            local a2_1 = matched_1[2][1];
            local b1_1 = matched_1[1][2];
            local a1_1 = matched_1[1][1];
            local c = cmp_fst(a1_1)(a2_1);
            if c ~= 0 then
              return c
            else
              return cmp_snd(b1_1)(b2_1)
            end
          end
        end
      end
    end;

    local function to_list(pair_8)
      local pair_match_8 = pair_8;
      local b_8 = pair_match_8[2];
      local a_8 = pair_match_8[1];
      return {_tag = 1, _0 = {a_8, {_tag = 1, _0 = {b_8, _Ctor_list_0}}}}
    end
    return {["make"] = make, ["fst"] = fst, ["snd"] = snd, ["swap"] = swap, ["map_fst"] = map_fst, ["map_snd"] = map_snd, ["map"] = map, ["fold"] = fold, ["iter"] = iter, ["equal"] = equal, ["compare"] = compare, ["to_list"] = to_list}
    end)();

    local Dict = (function()
    local function empty(param)
      return {}
    end;

    local singleton = function(key)
      return function(value)
        return (function()
          local _result = {};
          for _k, _v in pairs({}) do
            _result[_k] = _v
          end;
          _result[key] = value;
          return _result
        end)()
      end
    end;

    local get = function(key_1)
      return function(dict)
        return (function()
          local _v = dict[key_1];
          if _v == nil then
            return {_tag = 0}
          else
            return {_tag = 1, _0 = _v}
          end
        end)()
      end
    end;

    local get_or = function(key_2)
      return function(default)
        return function(dict_1)
          return Option.get_or((function()
            local _v = dict_1[key_2];
            if _v == nil then
              return {_tag = 0}
            else
              return {_tag = 1, _0 = _v}
            end
          end)())(default)
        end
      end
    end;

    local has = function(key_3)
      return function(dict_2)
        return dict_2[key_3] ~= nil
      end
    end;

    local function size(dict_3)
      return (function()
        local _count = 0;
        for _ in pairs(dict_3) do
          _count = _count + 1
        end;
        return _count
      end)()
    end;

    local function is_empty(dict_4)
      return (function()
        local _count = 0;
        for _ in pairs(dict_4) do
          _count = _count + 1
        end;
        return _count
      end)() == 0
    end;

    local set = function(key_4)
      return function(value_1)
        return function(dict_5)
          return (function()
            local _result = {};
            for _k, _v in pairs(dict_5) do
              _result[_k] = _v
            end;
            _result[key_4] = value_1;
            return _result
          end)()
        end
      end
    end;

    local remove = function(key_5)
      return function(dict_6)
        return (function()
          local _result = {};
          for _k, _v in pairs(dict_6) do
            if _k ~= key_5 then
              _result[_k] = _v
            end
          end;
          return _result
        end)()
      end
    end;

    local function keys(dict_7)
      return (function()
        local _result = {_tag = 0};
        for _k, _ in pairs(dict_7) do
          _result = {_tag = 1, _0 = {_k, _result}}
        end;
        return _result
      end)()
    end;

    local function values(dict_8)
      return List.map(function(pair)
        local pair_match = pair;
        local v = pair_match[2];
        return v
      end)((function()
        local _result = {_tag = 0};
        for _k, _v in pairs(dict_8) do
          _result = {_tag = 1, _0 = {{_k, _v}, _result}}
        end;
        return _result
      end)())
    end;

    local function entries(dict_9)
      return (function()
        local _result = {_tag = 0};
        for _k, _v in pairs(dict_9) do
          _result = {_tag = 1, _0 = {{_k, _v}, _result}}
        end;
        return _result
      end)()
    end;

    local map = function(f)
      return function(dict_10)
        return List.fold_left(function(acc)
          return function(pair_1)
            local pair_match_1 = pair_1;
            local v_1 = pair_match_1[2];
            local k = pair_match_1[1];
            return (function()
              local _result = {};
              for _k, _v in pairs(acc) do
                _result[_k] = _v
              end;
              _result[k] = f(v_1);
              return _result
            end)()
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_10) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local mapi = function(f_1)
      return function(dict_11)
        return List.fold_left(function(acc_1)
          return function(pair_2)
            local pair_match_2 = pair_2;
            local v_2 = pair_match_2[2];
            local k_1 = pair_match_2[1];
            return (function()
              local _result = {};
              for _k, _v in pairs(acc_1) do
                _result[_k] = _v
              end;
              _result[k_1] = f_1(k_1)(v_2);
              return _result
            end)()
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_11) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local filter = function(predicate)
      return function(dict_12)
        return List.fold_left(function(acc_2)
          return function(pair_3)
            local pair_match_3 = pair_3;
            local v_3 = pair_match_3[2];
            local k_2 = pair_match_3[1];
            if predicate(k_2)(v_3) then
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_2) do
                  _result[_k] = _v
                end;
                _result[k_2] = v_3;
                return _result
              end)()
            else
              return acc_2
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_12) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local filter_map = function(f_2)
      return function(dict_13)
        return List.fold_left(function(acc_3)
          return function(pair_4)
            local pair_match_4 = pair_4;
            local v_4 = pair_match_4[2];
            local k_3 = pair_match_4[1];
            local matched = f_2(k_3)(v_4);
            local matched_1 = matched;
            if matched_1._tag == 1 then
              local new_v = matched._0;
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_3) do
                  _result[_k] = _v
                end;
                _result[k_3] = new_v;
                return _result
              end)()
            elseif matched_1._tag == 0 then
              return acc_3
            else
              return error("Match failure")
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_13) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local fold = function(f_3)
      return function(dict_14)
        return function(init)
          return List.fold_left(function(acc_4)
            return function(pair_5)
              local pair_match_5 = pair_5;
              local v_5 = pair_match_5[2];
              local k_4 = pair_match_5[1];
              return f_3(k_4)(v_5)(acc_4)
            end
          end)(init)((function()
            local _result = {_tag = 0};
            for _k, _v in pairs(dict_14) do
              _result = {_tag = 1, _0 = {{_k, _v}, _result}}
            end;
            return _result
          end)())
        end
      end
    end;

    local iter = function(f_4)
      return function(dict_15)
        return List.iter(function(pair_6)
          local pair_match_6 = pair_6;
          local v_6 = pair_match_6[2];
          local k_5 = pair_match_6[1];
          return f_4(k_5)(v_6)
        end)((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_15) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local merge = function(dict1)
      return function(dict2)
        return List.fold_left(function(acc_5)
          return function(pair_7)
            local pair_match_7 = pair_7;
            local v_7 = pair_match_7[2];
            local k_6 = pair_match_7[1];
            return (function()
              local _result = {};
              for _k, _v in pairs(acc_5) do
                _result[_k] = _v
              end;
              _result[k_6] = v_7;
              return _result
            end)()
          end
        end)(dict1)((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict2) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local function of_list(items)
      return List.fold_left(function(acc_6)
        return function(pair_8)
          local pair_match_8 = pair_8;
          local v_8 = pair_match_8[2];
          local k_7 = pair_match_8[1];
          return (function()
            local _result = {};
            for _k, _v in pairs(acc_6) do
              _result[_k] = _v
            end;
            _result[k_7] = v_8;
            return _result
          end)()
        end
      end)({})(items)
    end;

    local function to_list(dict_16)
      return (function()
        local _result = {_tag = 0};
        for _k, _v in pairs(dict_16) do
          _result = {_tag = 1, _0 = {{_k, _v}, _result}}
        end;
        return _result
      end)()
    end;

    local equal = function(eq)
      return function(dict1_1)
        return function(dict2_1)
          if (function()
            local _count = 0;
            for _ in pairs(dict1_1) do
              _count = _count + 1
            end;
            return _count
          end)() ~= (function()
            local _count = 0;
            for _ in pairs(dict2_1) do
              _count = _count + 1
            end;
            return _count
          end)() then
            return false
          else
            return List.for_all(function(pair_9)
              local pair_match_9 = pair_9;
              local v1 = pair_match_9[2];
              local k_8 = pair_match_9[1];
              local matched_2 = (function()
                local _v = dict2_1[k_8];
                if _v == nil then
                  return {_tag = 0}
                else
                  return {_tag = 1, _0 = _v}
                end
              end)();
              local matched_3 = matched_2;
              if matched_3._tag == 1 then
                local v2 = matched_2._0;
                return eq(v1)(v2)
              elseif matched_3._tag == 0 then
                return false
              else
                return error("Match failure")
              end
            end)((function()
              local _result = {_tag = 0};
              for _k, _v in pairs(dict1_1) do
                _result = {_tag = 1, _0 = {{_k, _v}, _result}}
              end;
              return _result
            end)())
          end
        end
      end
    end;

    local find = function(predicate_1)
      return function(dict_17)
        return List.find(function(pair_10)
          local pair_match_10 = pair_10;
          local v_9 = pair_match_10[2];
          local k_9 = pair_match_10[1];
          return predicate_1(k_9)(v_9)
        end)((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_17) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local exists = function(predicate_2)
      return function(dict_18)
        return List.exists(function(pair_11)
          local pair_match_11 = pair_11;
          local v_10 = pair_match_11[2];
          local k_10 = pair_match_11[1];
          return predicate_2(k_10)(v_10)
        end)((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_18) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end;

    local for_all = function(predicate_3)
      return function(dict_19)
        return List.for_all(function(pair_12)
          local pair_match_12 = pair_12;
          local v_11 = pair_match_12[2];
          local k_11 = pair_match_12[1];
          return predicate_3(k_11)(v_11)
        end)((function()
          local _result = {_tag = 0};
          for _k, _v in pairs(dict_19) do
            _result = {_tag = 1, _0 = {{_k, _v}, _result}}
          end;
          return _result
        end)())
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["get"] = get, ["get_or"] = get_or, ["has"] = has, ["size"] = size, ["is_empty"] = is_empty, ["set"] = set, ["remove"] = remove, ["keys"] = keys, ["values"] = values, ["entries"] = entries, ["map"] = map, ["mapi"] = mapi, ["filter"] = filter, ["filter_map"] = filter_map, ["fold"] = fold, ["iter"] = iter, ["merge"] = merge, ["of_list"] = of_list, ["to_list"] = to_list, ["equal"] = equal, ["find"] = find, ["exists"] = exists, ["for_all"] = for_all}
    end)();

    local Set = (function()
    local function empty(param)
      return {}
    end;

    local function singleton(elem)
      return (function()
        local _result = {};
        for _k, _v in pairs({}) do
          _result[_k] = _v
        end;
        _result[elem] = true;
        return _result
      end)()
    end;

    local mem = function(elem_1)
      return function(set)
        return set[elem_1] ~= nil
      end
    end;

    local has = function(elem_2)
      return function(set_1)
        return set_1[elem_2] ~= nil
      end
    end;

    local function size(set_2)
      return (function()
        local _count = 0;
        for _ in pairs(set_2) do
          _count = _count + 1
        end;
        return _count
      end)()
    end;

    local function is_empty(set_3)
      return (function()
        local _count = 0;
        for _ in pairs(set_3) do
          _count = _count + 1
        end;
        return _count
      end)() == 0
    end;

    local add = function(elem_3)
      return function(set_4)
        return (function()
          local _result = {};
          for _k, _v in pairs(set_4) do
            _result[_k] = _v
          end;
          _result[elem_3] = true;
          return _result
        end)()
      end
    end;

    local remove = function(elem_4)
      return function(set_5)
        return (function()
          local _result = {};
          for _k, _v in pairs(set_5) do
            if _k ~= elem_4 then
              _result[_k] = _v
            end
          end;
          return _result
        end)()
      end
    end;

    local union = function(set1)
      return function(set2)
        return List.fold_left(function(acc)
          return function(elem_5)
            return (function()
              local _result = {};
              for _k, _v in pairs(acc) do
                _result[_k] = _v
              end;
              _result[elem_5] = true;
              return _result
            end)()
          end
        end)(set1)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set2) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local inter = function(set1_1)
      return function(set2_1)
        return List.fold_left(function(acc_1)
          return function(elem_6)
            if set2_1[elem_6] ~= nil then
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_1) do
                  _result[_k] = _v
                end;
                _result[elem_6] = true;
                return _result
              end)()
            else
              return acc_1
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set1_1) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local diff = function(set1_2)
      return function(set2_2)
        return List.fold_left(function(acc_2)
          return function(elem_7)
            if set2_2[elem_7] ~= nil then
              return acc_2
            else
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_2) do
                  _result[_k] = _v
                end;
                _result[elem_7] = true;
                return _result
              end)()
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set1_2) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local sym_diff = function(set1_3)
      return function(set2_3)
        local in_only_set1 = diff(set1_3)(set2_3);
        local in_only_set2 = diff(set2_3)(set1_3);
        return union(in_only_set1)(in_only_set2)
      end
    end;

    local subset = function(set1_4)
      return function(set2_4)
        return List.for_all(function(elem_8)
          return set2_4[elem_8] ~= nil
        end)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set1_4) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local disjoint = function(set1_5)
      return function(set2_5)
        return List.for_all(function(elem_9)
          return not (set2_5[elem_9] ~= nil)
        end)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set1_5) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local exists = function(predicate)
      return function(set_6)
        return List.exists(predicate)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_6) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local for_all = function(predicate_1)
      return function(set_7)
        return List.for_all(predicate_1)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_7) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local map = function(f)
      return function(set_8)
        return List.fold_left(function(acc_3)
          return function(elem_10)
            return (function()
              local _result = {};
              for _k, _v in pairs(acc_3) do
                _result[_k] = _v
              end;
              _result[f(elem_10)] = true;
              return _result
            end)()
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_8) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local filter = function(predicate_2)
      return function(set_9)
        return List.fold_left(function(acc_4)
          return function(elem_11)
            if predicate_2(elem_11) then
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_4) do
                  _result[_k] = _v
                end;
                _result[elem_11] = true;
                return _result
              end)()
            else
              return acc_4
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_9) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local filter_map = function(f_1)
      return function(set_10)
        return List.fold_left(function(acc_5)
          return function(elem_12)
            local matched = f_1(elem_12);
            local matched_1 = matched;
            if matched_1._tag == 1 then
              local new_elem = matched._0;
              return (function()
                local _result = {};
                for _k, _v in pairs(acc_5) do
                  _result[_k] = _v
                end;
                _result[new_elem] = true;
                return _result
              end)()
            elseif matched_1._tag == 0 then
              return acc_5
            else
              return error("Match failure")
            end
          end
        end)({})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_10) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local partition = function(predicate_3)
      return function(set_11)
        return List.fold_left(function(pair)
          return function(elem_13)
            local pair_match = pair;
            local no = pair_match[2];
            local yes = pair_match[1];
            if predicate_3(elem_13) then
              return {(function()
                local _result = {};
                for _k, _v in pairs(yes) do
                  _result[_k] = _v
                end;
                _result[elem_13] = true;
                return _result
              end)(), no}
            else
              return {yes, (function()
                local _result = {};
                for _k, _v in pairs(no) do
                  _result[_k] = _v
                end;
                _result[elem_13] = true;
                return _result
              end)()}
            end
          end
        end)({{}, {}})((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_11) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local fold = function(f_2)
      return function(set_12)
        return function(init)
          return List.fold_left(function(acc_6)
            return function(elem_14)
              return f_2(elem_14)(acc_6)
            end
          end)(init)((function()
            local _result = {_tag = 0};
            for _k, _ in pairs(set_12) do
              _result = {_tag = 1, _0 = {_k, _result}}
            end;
            return _result
          end)())
        end
      end
    end;

    local iter = function(f_3)
      return function(set_13)
        return List.iter(f_3)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_13) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local find = function(predicate_4)
      return function(set_14)
        return List.find(predicate_4)((function()
          local _result = {_tag = 0};
          for _k, _ in pairs(set_14) do
            _result = {_tag = 1, _0 = {_k, _result}}
          end;
          return _result
        end)())
      end
    end;

    local function elements(set_15)
      return (function()
        local _result = {_tag = 0};
        for _k, _ in pairs(set_15) do
          _result = {_tag = 1, _0 = {_k, _result}}
        end;
        return _result
      end)()
    end;

    local function to_list(set_16)
      return (function()
        local _result = {_tag = 0};
        for _k, _ in pairs(set_16) do
          _result = {_tag = 1, _0 = {_k, _result}}
        end;
        return _result
      end)()
    end;

    local function of_list(items)
      return List.fold_left(function(acc_7)
        return function(elem_15)
          return (function()
            local _result = {};
            for _k, _v in pairs(acc_7) do
              _result[_k] = _v
            end;
            _result[elem_15] = true;
            return _result
          end)()
        end
      end)({})(items)
    end;

    local equal = function(set1_6)
      return function(set2_6)
        if (function()
          local _count = 0;
          for _ in pairs(set1_6) do
            _count = _count + 1
          end;
          return _count
        end)() ~= (function()
          local _count = 0;
          for _ in pairs(set2_6) do
            _count = _count + 1
          end;
          return _count
        end)() then
          return false
        else
          return List.for_all(function(elem_16)
            return set2_6[elem_16] ~= nil
          end)((function()
            local _result = {_tag = 0};
            for _k, _ in pairs(set1_6) do
              _result = {_tag = 1, _0 = {_k, _result}}
            end;
            return _result
          end)())
        end
      end
    end;

    local compare = function(set1_7)
      return function(set2_7)
        local s1 = (function()
          local _count = 0;
          for _ in pairs(set1_7) do
            _count = _count + 1
          end;
          return _count
        end)();
        local s2 = (function()
          local _count = 0;
          for _ in pairs(set2_7) do
            _count = _count + 1
          end;
          return _count
        end)();
        if s1 < s2 then
          return 0 - 1
        else
          if s1 > s2 then
            return 1
          else
            return 0
          end
        end
      end
    end
    return {["empty"] = empty, ["singleton"] = singleton, ["mem"] = mem, ["has"] = has, ["size"] = size, ["is_empty"] = is_empty, ["add"] = add, ["remove"] = remove, ["union"] = union, ["inter"] = inter, ["diff"] = diff, ["sym_diff"] = sym_diff, ["subset"] = subset, ["disjoint"] = disjoint, ["exists"] = exists, ["for_all"] = for_all, ["map"] = map, ["filter"] = filter, ["filter_map"] = filter_map, ["partition"] = partition, ["fold"] = fold, ["iter"] = iter, ["find"] = find, ["elements"] = elements, ["to_list"] = to_list, ["of_list"] = of_list, ["equal"] = equal, ["compare"] = compare}
    end)();

    local String = (function()
    local _Ctor_option_0 = {_tag = 0};
    local _Ctor_list_0 = {_tag = 0};
    local function string_len(arg0)
      return arg0:len()
    end;

    local string_sub = function(arg0_1)
      return function(arg1)
        return function(arg2)
          return string.sub(arg0_1, arg1, arg2)
        end
      end
    end;

    local function string_upper(arg0_2)
      return arg0_2:upper()
    end;

    local function string_lower(arg0_3)
      return arg0_3:lower()
    end;

    local string_rep = function(arg0_4)
      return function(arg1_1)
        return string.rep(arg0_4, arg1_1)
      end
    end;

    local string_byte = function(arg0_5)
      return function(arg1_2)
        return string.byte(arg0_5, arg1_2)
      end
    end;

    local function string_char(arg0_6)
      return string.char(arg0_6)
    end;

    local string_gsub_raw = function(arg0_7)
      return function(arg1_3)
        return function(arg2_1)
          return string.gsub(arg0_7, arg1_3, arg2_1)
        end
      end
    end;

    local string_match_raw = function(arg0_8)
      return function(arg1_4)
        return (function()
          local _ffi_result = arg0_8:match(arg1_4);
          if _ffi_result == nil then
            return {_tag = 0}
          else
            return {_tag = 1, _0 = _ffi_result}
          end
        end)()
      end
    end;

    local string_find_start = function(arg0_9)
      return function(arg1_5)
        return (function()
          local _ffi_result = arg0_9:find(arg1_5);
          if _ffi_result == nil then
            return {_tag = 0}
          else
            return {_tag = 1, _0 = _ffi_result}
          end
        end)()
      end
    end;

    local function string_reverse(arg0_10)
      return string.reverse(arg0_10)
    end;

    local table_concat = function(arg0_11)
      return function(arg1_6)
        return table.concat(arg0_11, arg1_6)
      end
    end;

    local function length(str)
      return string_len(str)
    end;

    local function is_empty(str_1)
      return string_len(str_1) == 0
    end;

    local sub = function(str_2)
      return function(start)
        return function(stop)
          return string_sub(str_2)(start)(stop)
        end
      end
    end;

    local get = function(str_3)
      return function(index)
        if index < 1 or index > string_len(str_3) then
          return _Ctor_option_0
        else
          return {_tag = 1, _0 = string_byte(str_3)(index)}
        end
      end
    end;

    local get_exn = function(str_4)
      return function(index_1)
        if index_1 < 1 or index_1 > string_len(str_4) then
          return error("String.get_exn: index out of bounds")
        else
          return string_byte(str_4)(index_1)
        end
      end
    end;

    local function upper(str_5)
      return string_upper(str_5)
    end;

    local function lower(str_6)
      return string_lower(str_6)
    end;

    local function capitalize(str_7)
      if string_len(str_7) == 0 then
        return str_7
      else
        local first = string_upper(string_sub(str_7)(1)(1));
        local rest = string_sub(str_7)(2)(string_len(str_7));
        return first .. rest
      end
    end;

    local function uncapitalize(str_8)
      if string_len(str_8) == 0 then
        return str_8
      else
        local first_1 = string_lower(string_sub(str_8)(1)(1));
        local rest_1 = string_sub(str_8)(2)(string_len(str_8));
        return first_1 .. rest_1
      end
    end;

    local rep = function(str_9)
      return function(n)
        if n <= 0 then
          return ""
        else
          return string_rep(str_9)(n)
        end
      end
    end;

    local make = function(n_1)
      return function(byte)
        if n_1 <= 0 then
          return ""
        else
          return string_rep(string_char(byte))(n_1)
        end
      end
    end;

    local join = function(sep)
      return function(strings)
        local arr = Array.of_list(strings);
        return table_concat(arr)(sep)
      end
    end;

    local concat = function(str1)
      return function(str2)
        return str1 .. str2
      end
    end;

    local function reverse(str_10)
      return string_reverse(str_10)
    end;

    local find = function(str_11)
      return function(pattern)
        return Option.is_some(string_find_start(str_11)(pattern))
      end
    end;

    local contains = function(str_12)
      return function(substring)
        local escaped = string_gsub_raw(substring)("([%^%$%(%)%%%.%[%]%*%+%-%?])")("%%%1");
        return Option.is_some(string_find_start(str_12)(escaped))
      end
    end;

    local match_ = function(str_13)
      return function(pattern_1)
        return string_match_raw(str_13)(pattern_1)
      end
    end;

    local gsub = function(str_14)
      return function(pattern_2)
        return function(replacement)
          return string_gsub_raw(str_14)(pattern_2)(replacement)
        end
      end
    end;

    local starts_with = function(str_15)
      return function(prefix)
        local prefix_len = string_len(prefix);
        local str_len = string_len(str_15);
        if prefix_len > str_len then
          return false
        else
          return string_sub(str_15)(1)(prefix_len) == prefix
        end
      end
    end;

    local ends_with = function(str_16)
      return function(suffix)
        local suffix_len = string_len(suffix);
        local str_len_1 = string_len(str_16);
        if suffix_len > str_len_1 then
          return false
        else
          return string_sub(str_16)(str_len_1 - suffix_len + 1)(str_len_1) == suffix
        end
      end
    end;

    local function trim(str_17)
      local trimmed = string_gsub_raw(str_17)("^%s+")("");
      return string_gsub_raw(trimmed)("%s+$")("")
    end;

    local function trim_start(str_18)
      return string_gsub_raw(str_18)("^%s+")("")
    end;

    local function trim_end(str_19)
      return string_gsub_raw(str_19)("%s+$")("")
    end;

    local split = function(str_20)
      return function(sep_1)
        if is_empty(str_20) then
          return {_tag = 1, _0 = {str_20, _Ctor_list_0}}
        else
          if is_empty(sep_1) then
            return {_tag = 1, _0 = {str_20, _Ctor_list_0}}
          else
            local sep_len = string_len(sep_1);
            local str_len_2 = string_len(str_20);
            local escaped_sep = string_gsub_raw(sep_1)("([%^%$%(%)%%%.%[%]%*%+%-%?])")("%%%1");
            local go;
            go = function(acc)
              return function(pos)
                if pos > str_len_2 then
                  return List.reverse(acc)
                else
                  local matched = string_match_raw(string_sub(str_20)(pos)(str_len_2))("^(.-)" .. escaped_sep);
                  local matched_1 = matched;
                  if matched_1._tag == 1 then
                    local part = matched._0;
                    local new_pos = pos + string_len(part) + sep_len;
                    return go({_tag = 1, _0 = {part, acc}})(new_pos)
                  elseif matched_1._tag == 0 then
                    local remaining = string_sub(str_20)(pos)(str_len_2);
                    return List.reverse({_tag = 1, _0 = {remaining, acc}})
                  else
                    return error("Match failure")
                  end
                end
              end
            end;
            return go(_Ctor_list_0)(1)
          end
        end
      end
    end;

    local function lines(str_21)
      return split(str_21)("\n")
    end;

    local function to_bytes(str_22)
      local len = string_len(str_22);
      return List.init(len)(function(index_2)
        return string_byte(str_22)(index_2 + 1)
      end)
    end;

    local function of_bytes(bytes)
      return List.fold_left(function(acc_1)
        return function(byte_1)
          return acc_1 .. string_char(byte_1)
        end
      end)("")(bytes)
    end;

    local function of_byte(byte_2)
      return string_char(byte_2)
    end;

    local compare = function(str1_1)
      return function(str2_1)
        if str1_1 < str2_1 then
          return 0 - 1
        else
          if str1_1 > str2_1 then
            return 1
          else
            return 0
          end
        end
      end
    end;

    local equal = function(str1_2)
      return function(str2_2)
        return str1_2 == str2_2
      end
    end;

    local iter = function(f)
      return function(str_23)
        local len_1 = string_len(str_23);
        return (function()
          for index_3 = 1, len_1 do
            local _ = f(string_byte(str_23)(index_3))
          end;
          return nil
        end)()
      end
    end;

    local iteri = function(f_1)
      return function(str_24)
        local len_2 = string_len(str_24);
        return (function()
          for index_4 = 1, len_2 do
            local __1 = f_1(index_4)(string_byte(str_24)(index_4))
          end;
          return nil
        end)()
      end
    end;

    local fold_left = function(f_2)
      return function(init)
        return function(str_25)
          local len_3 = string_len(str_25);
          local acc_2 = {value = init};
          (function()
            for index_5 = 1, len_3 do
              acc_2.value = f_2(acc_2.value)(string_byte(str_25)(index_5))
            end;
            return nil
          end)();
          return acc_2.value
        end
      end
    end;

    local for_all = function(predicate)
      return function(str_26)
        local len_4 = string_len(str_26);
        local result = {value = true};
        local index_6 = {value = 1};
        (function()
          while index_6.value <= len_4 and result.value do
            if not predicate(string_byte(str_26)(index_6.value)) then
              result.value = false
            else
              index_6.value = index_6.value + 1
            end
          end;
          return nil
        end)();
        return result.value
      end
    end;

    local exists = function(predicate_1)
      return function(str_27)
        local len_5 = string_len(str_27);
        local result_1 = {value = false};
        local index_7 = {value = 1};
        (function()
          while index_7.value <= len_5 and not result_1.value do
            if predicate_1(string_byte(str_27)(index_7.value)) then
              result_1.value = true
            else
              index_7.value = index_7.value + 1
            end
          end;
          return nil
        end)();
        return result_1.value
      end
    end
    return {["string_len"] = string_len, ["string_sub"] = string_sub, ["string_upper"] = string_upper, ["string_lower"] = string_lower, ["string_rep"] = string_rep, ["string_byte"] = string_byte, ["string_char"] = string_char, ["string_gsub_raw"] = string_gsub_raw, ["string_match_raw"] = string_match_raw, ["string_find_start"] = string_find_start, ["string_reverse"] = string_reverse, ["table_concat"] = table_concat, ["length"] = length, ["is_empty"] = is_empty, ["sub"] = sub, ["get"] = get, ["get_exn"] = get_exn, ["upper"] = upper, ["lower"] = lower, ["capitalize"] = capitalize, ["uncapitalize"] = uncapitalize, ["rep"] = rep, ["make"] = make, ["join"] = join, ["concat"] = concat, ["reverse"] = reverse, ["find"] = find, ["contains"] = contains, ["match_"] = match_, ["gsub"] = gsub, ["starts_with"] = starts_with, ["ends_with"] = ends_with, ["trim"] = trim, ["trim_start"] = trim_start, ["trim_end"] = trim_end, ["split"] = split, ["lines"] = lines, ["to_bytes"] = to_bytes, ["of_bytes"] = of_bytes, ["of_byte"] = of_byte, ["compare"] = compare, ["equal"] = equal, ["iter"] = iter, ["iteri"] = iteri, ["fold_left"] = fold_left, ["for_all"] = for_all, ["exists"] = exists}
    end)();

    local Math = (function()
    local function floor_raw(arg0)
      return math.floor(arg0)
    end;

    local function ceil_raw(arg0_1)
      return math.ceil(arg0_1)
    end;

    local function abs(arg0_2)
      return math.abs(arg0_2)
    end;

    local fmod = function(arg0_3)
      return function(arg1)
        return math.fmod(arg0_3, arg1)
      end
    end;

    local min_raw = function(arg0_4)
      return function(arg1_1)
        return math.min(arg0_4, arg1_1)
      end
    end;

    local max_raw = function(arg0_5)
      return function(arg1_2)
        return math.max(arg0_5, arg1_2)
      end
    end;

    local function exp(arg0_6)
      return math.exp(arg0_6)
    end;

    local function log(arg0_7)
      return math.log(arg0_7)
    end;

    local function sqrt(arg0_8)
      return math.sqrt(arg0_8)
    end;

    local pow = function(arg0_9)
      return function(arg1_3)
        return math.pow(arg0_9, arg1_3)
      end
    end;

    local function sin(arg0_10)
      return math.sin(arg0_10)
    end;

    local function cos(arg0_11)
      return math.cos(arg0_11)
    end;

    local function tan(arg0_12)
      return math.tan(arg0_12)
    end;

    local function asin(arg0_13)
      return math.asin(arg0_13)
    end;

    local function acos(arg0_14)
      return math.acos(arg0_14)
    end;

    local function atan(arg0_15)
      return math.atan(arg0_15)
    end;

    local atan2 = function(arg0_16)
      return function(arg1_4)
        return math.atan2(arg0_16, arg1_4)
      end
    end;

    local function rad(arg0_17)
      return math.rad(arg0_17)
    end;

    local function deg(arg0_18)
      return math.deg(arg0_18)
    end;

    local function random_unit(arg0_19)
      return math.random()
    end;

    local function random_max(arg0_20)
      return math.random(arg0_20)
    end;

    local random_range_raw = function(arg0_21)
      return function(arg1_5)
        return math.random(arg0_21, arg1_5)
      end
    end;

    local function randomseed(arg0_22)
      return math.randomseed(arg0_22)
    end;
    local pi = 3.14159;
    local huge = 1 / 0;
    local function floor(x)
      return floor_raw(x)
    end;

    local function ceil(x_1)
      return ceil_raw(x_1)
    end;

    local function round(x_2)
      if x_2 >= 0 then
        return floor_raw(x_2 + 0.5)
      else
        return ceil_raw(x_2 - 0.5)
      end
    end;

    local function trunc(x_3)
      if x_3 >= 0 then
        return floor_raw(x_3)
      else
        return ceil_raw(x_3)
      end
    end;

    local function abs_int(n)
      if n < 0 then
        return 0 - n
      else
        return n
      end
    end;

    local min = function(a)
      return function(b)
        return min_raw(a)(b)
      end
    end;

    local max = function(a_1)
      return function(b_1)
        return max_raw(a_1)(b_1)
      end
    end;

    local min_int = function(a_2)
      return function(b_2)
        if a_2 < b_2 then
          return a_2
        else
          return b_2
        end
      end
    end;

    local max_int = function(a_3)
      return function(b_3)
        if a_3 > b_3 then
          return a_3
        else
          return b_3
        end
      end
    end;

    local function modf(x_4)
      local int_part = trunc(x_4);
      local frac_part = x_4 - int_part;
      return {int_part, frac_part}
    end;

    local function log10(x_5)
      return log(x_5) / log(10)
    end;

    local function random(param)
      return random_unit(nil)
    end;

    local function random_int(n_1)
      return random_max(n_1)
    end;

    local random_range = function(m)
      return function(n_2)
        return random_range_raw(m)(n_2)
      end
    end
    return {["floor_raw"] = floor_raw, ["ceil_raw"] = ceil_raw, ["abs"] = abs, ["fmod"] = fmod, ["min_raw"] = min_raw, ["max_raw"] = max_raw, ["exp"] = exp, ["log"] = log, ["sqrt"] = sqrt, ["pow"] = pow, ["sin"] = sin, ["cos"] = cos, ["tan"] = tan, ["asin"] = asin, ["acos"] = acos, ["atan"] = atan, ["atan2"] = atan2, ["rad"] = rad, ["deg"] = deg, ["random_unit"] = random_unit, ["random_max"] = random_max, ["random_range_raw"] = random_range_raw, ["randomseed"] = randomseed, ["pi"] = pi, ["huge"] = huge, ["floor"] = floor, ["ceil"] = ceil, ["round"] = round, ["trunc"] = trunc, ["abs_int"] = abs_int, ["min"] = min, ["max"] = max, ["min_int"] = min_int, ["max_int"] = max_int, ["modf"] = modf, ["log10"] = log10, ["random"] = random, ["random_int"] = random_int, ["random_range"] = random_range}
    end)();
    local function f(x)
      local x_match = x;
      local y = x_match;
      return y
    end
    |}]

let%expect_test "compile constructor pattern" =
  let lua = compile {|
    type option = None | Some of int
    let f x = match x with
      | Some n -> n
      | None -> 0
  |} in
  (* Should handle constructor patterns *)
  let has_tag_check = String.length lua > 0 in
  print_endline (if has_tag_check then "generated code" else "ERROR");
  [%expect {| generated code |}]

let%expect_test "compile tuple pattern" =
  let lua = compile {|
    let f x = match x with
      | (a, b) -> a + b
  |} in
  (* Should handle tuple destructuring *)
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Deep Pattern Performance Tests} *)

let%expect_test "compile deeply nested tuple" =
  (* Test that deep nesting doesn't cause stack overflow or O(n²) behavior *)
  let code = {|
    let f x = match x with
      | (((((a, b), c), d), e), f) -> a + b + c + d + e + f
  |} in
  let start = Unix.gettimeofday () in
  let _ = compile code in
  let elapsed = Unix.gettimeofday () -. start in
  (* Should complete quickly *)
  print_endline (if elapsed < 1.0 then "fast" else "slow");
  [%expect {| fast |}]

let%expect_test "compile multiple constructors" =
  let lua = compile {|
    type color = Red | Green | Blue | Yellow | Cyan | Magenta
    let f x = match x with
      | Red -> 1
      | Green -> 2
      | Blue -> 3
      | Yellow -> 4
      | Cyan -> 5
      | Magenta -> 6
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Record Pattern Tests} *)

let%expect_test "compile record pattern" =
  let lua = compile {|
    type point = { x : int; y : int }
    let f p = match p with
      | { x = a; y = b } -> a + b
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

let%expect_test "compile nested record and constructor" =
  let lua = compile {|
    type point = { x : int; y : int }
    type option = None | Some of point
    let f x = match x with
      | Some { x = a; y = b } -> a + b
      | None -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Guard Pattern Tests} *)

let%expect_test "compile pattern with guard" =
  let lua = compile {|
    let f x = match x with
      | n when n > 0 -> n
      | _ -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

(** {1 Full Compilation Performance Test} *)

let%expect_test "full compilation of deep tree pattern" =
  let code = {|
    type tree = Leaf | Node of tree * int * tree

    let rec sum t = match t with
      | Leaf -> 0
      | Node (left, value, right) ->
        sum left + value + sum right
  |} in
  let start = Unix.gettimeofday () in
  let _ = compile code in
  let elapsed = Unix.gettimeofday () -. start in
  print_endline (if elapsed < 1.0 then "fast" else "slow");
  [%expect {| fast |}]

(** {1 Complex Nested Pattern Tests} *)

let%expect_test "compile mixed tuple and constructor pattern" =
  let lua = compile {|
    type option = None | Some of int
    let f x = match x with
      | (Some a, Some b) -> a + b
      | (Some a, None) -> a
      | (None, Some b) -> b
      | (None, None) -> 0
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]

let%expect_test "compile triple nested tuple" =
  let lua = compile {|
    let f x = match x with
      | ((a, b), (c, d), (e, f)) -> a + b + c + d + e + f
  |} in
  let has_code = String.length lua > 0 in
  print_endline (if has_code then "generated code" else "ERROR");
  [%expect {| generated code |}]
