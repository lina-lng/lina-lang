(** Embedded stdlib source files using ppx_blob for compile-time embedding. *)

type stdlib_module = {
  name : string;
  source : string;
}

let fn_source = [%blob "modules/fn.lina"]
let ord_source = [%blob "modules/ord.lina"]
let list_source = [%blob "modules/list.lina"]
let result_source = [%blob "modules/result.lina"]
let option_source = [%blob "modules/option.lina"]
let array_source = [%blob "modules/array.lina"]
let tuple_source = [%blob "modules/tuple.lina"]
let dict_source = [%blob "modules/dict.lina"]
let set_source = [%blob "modules/set.lina"]
let string_source = [%blob "modules/string.lina"]
let math_source = [%blob "modules/math.lina"]
let io_source = [%blob "modules/io.lina"]
let os_source = [%blob "modules/os.lina"]
let coroutine_source = [%blob "modules/coroutine.lina"]
let debug_source = [%blob "modules/debug.lina"]

let lua_runtime_helpers = [%blob "runtime/helpers.lua"]

let all_modules = [
  { name = "Fn"; source = fn_source };
  { name = "Ord"; source = ord_source };
  { name = "List"; source = list_source };
  { name = "Result"; source = result_source };
  { name = "Option"; source = option_source };
  { name = "Array"; source = array_source };
  { name = "Tuple"; source = tuple_source };
  { name = "Dict"; source = dict_source };
  { name = "Set"; source = set_source };
  { name = "String"; source = string_source };
  { name = "Math"; source = math_source };
  { name = "Io"; source = io_source };
  { name = "Os"; source = os_source };
  { name = "Coroutine"; source = coroutine_source };
  { name = "Debug"; source = debug_source };
]
