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

(** Compile source and return Lua code (without stdlib prelude) or error. *)
let compile code =
  Typing.Types.reset_type_variable_id ();
  match Pipeline.compile_string test_options "<test>" code with
  | Ok lua_code ->
    let prelude_len = String.length (Stdlib_loader.stdlib_prelude ()) in
    String.sub lua_code prelude_len (String.length lua_code - prelude_len)
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
  [%expect {| local x = {_tag = 1, _0 = 42} |}]

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
    local _Ctor_option_0 = {_tag = 0};
    local function f(b)
      if b then
        return _Ctor_option_0
      else
        return _Ctor_option_0
      end
    end
    |}]
