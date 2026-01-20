(** FFI attribute types (semantic representation).

    These types represent the validated, semantic form of FFI attributes
    after parsing. They are used during type checking and code generation. *)

open Common

(** FFI attribute - validated form of parsed attributes. *)
type ffi_attribute =
  | FFIModule of string
    (** [@module("path")] - Import from a Lua module via require() *)
  | FFIVal
    (** [@val] - Access a global value *)
  | FFIScope of string list
    (** [@scope("name")] or [@scope(("a", "b"))] - Nested access path *)
  | FFISend
    (** [@send] - Method call on first argument (obj:method(args)) *)
  | FFIGet
    (** [@get] - Property getter (obj.field) *)
  | FFISet
    (** [@set] - Property setter (obj.field = value) *)
  | FFIGetIndex
    (** [@get_index] - Index getter (obj[key]) *)
  | FFISetIndex
    (** [@set_index] - Index setter (obj[key] = value) *)
  | FFINew
    (** [@new] - Constructor call (Class.new(args)) *)
  | FFIVariadic
    (** [@variadic] - Last argument is spread as varargs *)
  | FFIReturnNullable
    (** [@return(nullable)] - Lua nil maps to None, value to Some *)
  | FFIAs of string
    (** [@as("name")] - Override the Lua name for this binding *)
[@@deriving show, eq]

(** The kind of FFI call - determines code generation strategy. *)
type ffi_kind =
  | FFIKindModule of string
    (** Call a function from a required module *)
  | FFIKindGlobal of string list
    (** Call a global function, optionally with scope path *)
  | FFIKindMethod
    (** Method call on receiver (first arg) *)
  | FFIKindGetter
    (** Property access on receiver *)
  | FFIKindSetter
    (** Property assignment on receiver *)
  | FFIKindIndexGetter
    (** Index access obj[key] *)
  | FFIKindIndexSetter
    (** Index assignment obj[key] = val *)
  | FFIKindConstructor
    (** Constructor call with .new() *)
[@@deriving show, eq]

(** Complete FFI specification for an external declaration. *)
type ffi_spec = {
  ffi_kind : ffi_kind;
    (** The kind of FFI call *)
  ffi_lua_name : string;
    (** The Lua name to use (from primitive or @as) *)
  ffi_is_variadic : bool;
    (** Whether the last argument should be spread *)
  ffi_return_nullable : bool;
    (** Whether to wrap return value in option *)
  ffi_arity : int;
    (** Number of arguments (including receiver for methods) *)
  ffi_unit_params : bool list;
    (** Which parameters are unit type (true = unit, should not be passed to Lua) *)
  ffi_location : Location.t;
    (** Source location for error reporting *)
}
[@@deriving show, eq]

(** Create a default FFI spec for a global function. *)
let make_global_spec ~lua_name ~arity ~unit_params ~location =
  {
    ffi_kind = FFIKindGlobal [];
    ffi_lua_name = lua_name;
    ffi_is_variadic = false;
    ffi_return_nullable = false;
    ffi_arity = arity;
    ffi_unit_params = unit_params;
    ffi_location = location;
  }
