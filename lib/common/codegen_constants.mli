(** Constants used in Lambda IR and Lua code generation.

    This module centralizes magic numbers and generated identifier names
    to ensure consistency across translation phases. All generated temporary
    variable names and compilation thresholds are defined here. *)

(** {1 Variant Runtime Representation} *)

val variant_tag_field : string
(** Field name for variant tag in Lua tables.
    Used in: [{_tag = N}], [{_tag = N, _0 = x}] *)

val variant_arg_field : string
(** Field name for variant argument in Lua tables.
    Used in: [{_tag = N, _0 = x}] *)

(** {1 Reference Cell Representation} *)

val ref_value_field : string
(** Field name for ref cell value in Lua tables.
    Used in: [{value = x}], [r.value] *)

(** {1 FFI Internal Names} *)

val ffi_result_name : string
(** Temporary variable for FFI return value wrapping.
    Used in nullable return handling. *)

(** {1 Pattern Match Compilation} *)

val dispatch_table_threshold : int
(** Number of constructor cases at which to use dispatch table
    instead of if-chain. Below this threshold, if-chain is faster
    due to branch prediction. At or above this threshold, dispatch
    table wins due to O(1) lookup. Currently set to 4. *)

(** {1 Generated Identifier Prefixes}

    These prefixes are used for compiler-generated temporary variables.
    All start with underscore to avoid conflicts with user-defined names. *)

val scrutinee_prefix : string
(** Prefix for generated scrutinee variables in match expressions.
    Example: [_scrutinee], [_scrutinee_1] *)

val tuple_temp_prefix : string
(** Prefix for tuple destructuring temporaries.
    Example: [_tuple], [_tuple_1] *)

val constructor_temp_prefix : string
(** Prefix for constructor argument temporaries.
    Example: [_ctor], [_ctor_1] *)

val record_temp_prefix : string
(** Prefix for record destructuring temporaries.
    Example: [_record], [_record_1] *)

val included_module_prefix : string
(** Prefix for included module temporaries.
    Example: [_included], [_included_1] *)

val param_prefix : string
(** Prefix for anonymous function parameters.
    Example: [_param], [_param_1] *)

val rec_prefix : string
(** Prefix for recursive binding temporaries.
    Example: [_rec], [_rec_1] *)

val top_prefix : string
(** Prefix for top-level binding temporaries.
    Example: [_top], [_top_1] *)

val include_prefix : string
(** Prefix for include statement temporaries.
    Example: [_include], [_include_1] *)

(** {1 Lua Codegen Names}

    These are fixed names used in generated Lua code.
    They don't have stamps since they are local to generated blocks. *)

val switch_scrutinee_name : string
(** Local variable name for switch scrutinee.
    Used in: [local _switch = <expr>] *)

val dispatch_table_name : string
(** Local variable name for dispatch table.
    Used in: [local _dispatch = \{...\}] *)

val dispatch_handler_name : string
(** Local variable name for dispatch handler.
    Used in: [local _handler = _dispatch[tag]] *)

val record_update_result_name : string
(** Local variable name for record update result.
    Used in: [local _result = \{\}] *)

val pair_key_name : string
(** Loop variable for key in pairs iteration.
    Used in: [for _k, _v in pairs(...)] *)

val pair_value_name : string
(** Loop variable for value in pairs iteration.
    Used in: [for _k, _v in pairs(...)] *)

(** {1 Singleton Constructor Names} *)

val singleton_var_format : (string -> int -> string, unit, string) format
(** Format string for singleton constructor variables.
    Use with [Printf.sprintf singleton_var_format type_name tag_index].
    Example: ["_Ctor_option_0"] for [None] constructor. *)

val singleton_var_name : string -> int -> string
(** [singleton_var_name type_name tag_index] generates the variable name
    for a nullary constructor singleton.

    @param type_name The name of the variant type (e.g., ["option"])
    @param tag_index The tag index of the constructor (e.g., [0] for [None])
    @return Variable name like ["_Ctor_option_0"] *)
