(** Constants used in Lambda IR and Lua code generation. *)

(* Variant Runtime Representation *)

let variant_tag_field = "_tag"
(** Field name for variant tag in Lua tables.
    Used in: [{_tag = N}], [{_tag = N, _0 = x}] *)

let variant_arg_field = "_0"
(** Field name for variant argument in Lua tables.
    Used in: [{_tag = N, _0 = x}] *)

(* Reference Cell Representation *)

let ref_value_field = "value"
(** Field name for ref cell value in Lua tables.
    Used in: [{value = x}], [r.value] *)

(* FFI Internal Names *)

let ffi_result_name = "_ffi_result"
(** Temporary variable for FFI return value wrapping.
    Used in nullable return handling. *)

(* Pattern Match Compilation *)

let dispatch_table_threshold = 4

(* Generated Identifier Prefixes *)

let scrutinee_prefix = "_scrutinee"
let tuple_temp_prefix = "_tuple"
let constructor_temp_prefix = "_ctor"
let record_temp_prefix = "_record"
let included_module_prefix = "_included"
let param_prefix = "_param"
let rec_prefix = "_rec"
let top_prefix = "_top"
let include_prefix = "_include"

(* Lua Codegen Names *)

let switch_scrutinee_name = "_switch"
let dispatch_table_name = "_dispatch"
let dispatch_handler_name = "_handler"
let record_update_result_name = "_result"
let pair_key_name = "_k"
let pair_value_name = "_v"

(* Singleton Constructor Names *)

let singleton_var_format : (string -> int -> string, unit, string) format =
  "_Ctor_%s_%d"

let singleton_var_name type_name tag_index =
  Printf.sprintf singleton_var_format type_name tag_index
