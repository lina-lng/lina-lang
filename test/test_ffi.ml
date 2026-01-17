(** Unit tests for FFI attribute parsing, validation, and code generation.

    Tests cover:
    - Attribute parsing from AST to semantic form
    - Validation error detection (duplicates, conflicts, arity)
    - Valid attribute combinations
    - Code generation for each FFI kind *)

open Common

(** {1 Helper Functions} *)

(** Create a parsed attribute for testing *)
let make_attr name payload =
  Parsing_ffi.Attributes.{
    attribute_name = name;
    attribute_payload = payload;
    attribute_location = Location.none;
  }

(** Parse a list of attributes using the FFI check module *)
let parse_attrs attrs =
  let open Typing_ffi.Check in
  let result = build_ffi_spec
    ~attrs
    ~primitive:"test"
    ~arity:1
    ~location:Location.none
  in
  result

(** Parse with custom arity *)
let parse_attrs_with_arity arity attrs =
  let open Typing_ffi.Check in
  let result = build_ffi_spec
    ~attrs
    ~primitive:"test"
    ~arity
    ~location:Location.none
  in
  result

(** Print FFI spec for testing *)
let print_ffi_spec spec =
  let open Typing_ffi.Types in
  let kind_str = match spec.ffi_kind with
    | FFIKindModule path -> Printf.sprintf "Module(%s)" path
    | FFIKindGlobal [] -> "Global"
    | FFIKindGlobal path -> Printf.sprintf "Global(%s)" (String.concat "." path)
    | FFIKindMethod -> "Method"
    | FFIKindGetter -> "Getter"
    | FFIKindSetter -> "Setter"
    | FFIKindIndexGetter -> "IndexGetter"
    | FFIKindIndexSetter -> "IndexSetter"
    | FFIKindConstructor -> "Constructor"
  in
  Printf.printf "kind=%s lua_name=%s variadic=%b nullable=%b arity=%d\n"
    kind_str spec.ffi_lua_name spec.ffi_is_variadic spec.ffi_return_nullable spec.ffi_arity

(** Print error for testing *)
let print_error err =
  print_endline (Typing_ffi.Check.error_message err)

(** {1 Attribute Parsing Tests} *)

let%expect_test "parse @module attribute" =
  let attrs = [make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "socket"))] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Module(socket) lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @val attribute" =
  let attrs = [make_attr "val" None] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @scope single string" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "math"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global(math) lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @scope string list" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadStringList ["table"; "insert"]));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global(table.insert) lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @send attribute" =
  let attrs = [make_attr "send" None] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Method lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @get attribute" =
  let attrs = [make_attr "get" None] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Getter lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @set attribute with arity 2" =
  let attrs = [make_attr "set" None] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Setter lua_name=test variadic=false nullable=false arity=2 |}]

let%expect_test "parse @get_index attribute with arity 2" =
  let attrs = [make_attr "get_index" None] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=IndexGetter lua_name=test variadic=false nullable=false arity=2 |}]

let%expect_test "parse @set_index attribute with arity 3" =
  let attrs = [make_attr "set_index" None] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=IndexSetter lua_name=test variadic=false nullable=false arity=3 |}]

let%expect_test "parse @new attribute" =
  let attrs = [make_attr "new" None] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "parse @variadic attribute" =
  let attrs = [
    make_attr "val" None;
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=true nullable=false arity=1 |}]

let%expect_test "parse @return(nullable) attribute" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=false nullable=true arity=1 |}]

let%expect_test "parse @return(nullable) with string payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadString "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=false nullable=true arity=1 |}]

let%expect_test "parse @as attribute" =
  let attrs = [
    make_attr "val" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "lua_print"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=lua_print variadic=false nullable=false arity=1 |}]

(** {1 Validation Error Tests - Duplicate Attributes} *)

let%expect_test "error: duplicate @val" =
  let attrs = [
    make_attr "val" None;
    make_attr "val" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @val appears multiple times |}]

let%expect_test "error: duplicate @module" =
  let attrs = [
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "a"));
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "b"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @module appears multiple times |}]

let%expect_test "error: duplicate @send" =
  let attrs = [
    make_attr "send" None;
    make_attr "send" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @send appears multiple times |}]

(** {1 Validation Error Tests - Conflicting Attributes} *)

let%expect_test "error: @module and @val conflict" =
  let attrs = [
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "x"));
    make_attr "val" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @module and @val cannot be used together |}]

let%expect_test "error: @send and @get conflict" =
  let attrs = [
    make_attr "send" None;
    make_attr "get" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @send and @get cannot be used together |}]

let%expect_test "error: @send and @set conflict" =
  let attrs = [
    make_attr "send" None;
    make_attr "set" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @send and @set cannot be used together |}]

let%expect_test "error: @get and @set conflict" =
  let attrs = [
    make_attr "get" None;
    make_attr "set" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @get and @set cannot be used together |}]

let%expect_test "error: @get_index and @set_index conflict" =
  let attrs = [
    make_attr "get_index" None;
    make_attr "set_index" None;
  ] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @get_index and @set_index cannot be used together |}]

let%expect_test "error: @new and @send conflict" =
  let attrs = [
    make_attr "new" None;
    make_attr "send" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @new and @send cannot be used together |}]

let%expect_test "error: @new and @get conflict" =
  let attrs = [
    make_attr "new" None;
    make_attr "get" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @new and @get cannot be used together |}]

(** {1 Validation Error Tests - Arity Mismatches} *)

let%expect_test "error: @send with arity 0" =
  let attrs = [make_attr "send" None] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @send requires 1 argument(s), but external has 0 |}]

let%expect_test "error: @get with arity 2" =
  let attrs = [make_attr "get" None] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @get requires 1 argument(s), but external has 2 |}]

let%expect_test "error: @get with arity 0" =
  let attrs = [make_attr "get" None] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @get requires 1 argument(s), but external has 0 |}]

let%expect_test "error: @set with arity 1" =
  let attrs = [make_attr "set" None] in
  (match parse_attrs_with_arity 1 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @set requires 2 argument(s), but external has 1 |}]

let%expect_test "error: @set with arity 3" =
  let attrs = [make_attr "set" None] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @set requires 2 argument(s), but external has 3 |}]

let%expect_test "error: @get_index with arity 1" =
  let attrs = [make_attr "get_index" None] in
  (match parse_attrs_with_arity 1 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @get_index requires 2 argument(s), but external has 1 |}]

let%expect_test "error: @set_index with arity 2" =
  let attrs = [make_attr "set_index" None] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @set_index requires 3 argument(s), but external has 2 |}]

(** {1 Validation Error Tests - Invalid Payloads} *)

let%expect_test "error: @module without payload" =
  let attrs = [make_attr "module" None] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @module: expected string argument: @module("path") |}]

let%expect_test "error: @module with string list" =
  let attrs = [make_attr "module" (Some (Parsing_ffi.Attributes.PayloadStringList ["a"; "b"]))] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @module: expected single string, not a list |}]

let%expect_test "error: @scope without payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @scope: expected string or string list: @scope("name") or @scope(("a", "b")) |}]

let%expect_test "error: @return with invalid payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadString "invalid"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @return: expected "nullable", got "invalid" |}]

let%expect_test "error: @return with invalid ident payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "invalid"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @return: expected nullable, got invalid |}]

let%expect_test "error: @return without payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @return: expected argument: @return(nullable) |}]

let%expect_test "error: @as without payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "as" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @as: expected string argument: @as("lua_name") |}]

let%expect_test "error: @val with payload" =
  let attrs = [make_attr "val" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @val: expected no argument |}]

let%expect_test "error: @send with payload" =
  let attrs = [make_attr "send" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @send: expected no argument |}]

(** {1 Valid Combination Tests} *)

let%expect_test "valid: @module with @as" =
  let attrs = [
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "socket"));
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "connect_tcp"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Module(socket) lua_name=connect_tcp variadic=false nullable=false arity=1 |}]

let%expect_test "valid: @val with @scope" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "math"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global(math) lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "valid: @send with @variadic" =
  let attrs = [
    make_attr "send" None;
    make_attr "variadic" None;
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Method lua_name=test variadic=true nullable=false arity=2 |}]

let%expect_test "valid: @send with @return(nullable)" =
  let attrs = [
    make_attr "send" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Method lua_name=test variadic=false nullable=true arity=2 |}]

let%expect_test "valid: @module with @return(nullable)" =
  let attrs = [
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "db"));
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Module(db) lua_name=test variadic=false nullable=true arity=1 |}]

let%expect_test "valid: @val with @scope and @as" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "os"));
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "get_time"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global(os) lua_name=get_time variadic=false nullable=false arity=1 |}]

let%expect_test "valid: multiple modifiers" =
  let attrs = [
    make_attr "send" None;
    make_attr "variadic" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "method_name"));
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Method lua_name=method_name variadic=true nullable=true arity=2 |}]

(** {1 Unknown Attribute Tests} *)

let%expect_test "unknown attributes are ignored" =
  let attrs = [
    make_attr "val" None;
    make_attr "unknown" None;
    make_attr "future_feature" (Some (Parsing_ffi.Attributes.PayloadString "x"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=false nullable=false arity=1 |}]

(** {1 Code Generation Tests}

    These tests verify FFI code generation through full compilation.
    The compilation pipeline: Source -> Parse -> Type -> Lambda -> Lua *)

(** Suppress all warnings in tests to avoid noise in expected output. *)
let test_options = Driver.Pipeline.{
  default_options with
  warning_config = Common.Warning_config.disable_all Common.Warning_config.default;
}

(** Compile source and return Lua code or error *)
let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string test_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let%expect_test "codegen: @module generates require call" =
  print_endline (compile {|
@module("socket")
external create_tcp : unit -> int = "tcp"

let sock = create_tcp ()
|});
  [%expect {|
    local function create_tcp(arg0)
      return require("socket").tcp(arg0)
    end;
    local sock = create_tcp(nil)
    |}]

let%expect_test "codegen: @val generates global function call" =
  print_endline (compile {|
@val
external print_value : int -> unit = "print"

let _ = print_value 42
|});
  [%expect {|
    local function print_value(arg0)
      return print(arg0)
    end;
    local top = print_value(42)
    |}]

let%expect_test "codegen: @val @scope generates scoped call" =
  print_endline (compile {|
@val @scope("math")
external floor : float -> int = "floor"

let x = floor 3.14
|});
  [%expect {|
    local function floor(arg0)
      return math.floor(arg0)
    end;
    local x = floor(3.14)
    |}]

let%expect_test "codegen: @val @scope with nested path" =
  print_endline (compile {|
@val @scope(("table", "pack"))
external pack_values : int -> int = "call"

let x = pack_values 1
|});
  [%expect {|
    local function pack_values(arg0)
      return table.pack.call(arg0)
    end;
    local x = pack_values(1)
    |}]

let%expect_test "codegen: @send generates method call" =
  print_endline (compile {|
type socket

@send
external close : socket -> unit = "close"

@val
external get_socket : unit -> socket = "get_socket"

let sock = get_socket ()
let _ = close sock
|});
  [%expect {|
    local function close(arg0)
      return arg0:close()
    end;

    local function get_socket(arg0_1)
      return get_socket(arg0_1)
    end;
    local sock = get_socket(nil);
    local top = close(sock)
    |}]

let%expect_test "codegen: @send with extra args" =
  print_endline (compile {|
type socket

@send
external send_data : socket -> string -> int -> unit = "send"

@val
external get_socket : unit -> socket = "get_socket"

let sock = get_socket ()
let _ = send_data sock "hello" 5
|});
  [%expect {|
    local send_data = function(arg0)
      return function(arg1)
        return function(arg2)
          return arg0:send(arg1, arg2)
        end
      end
    end;

    local function get_socket(arg0_1)
      return get_socket(arg0_1)
    end;
    local sock = get_socket(nil);
    local top = send_data(sock)("hello")(5)
    |}]

let%expect_test "codegen: @get generates property access" =
  print_endline (compile {|
@get
external get_length : string -> int = "length"

let len = get_length "hello"
|});
  [%expect {|
    local function get_length(arg0)
      return arg0.length
    end;
    local len = get_length("hello")
    |}]

let%expect_test "codegen: @set generates assignment with IIFE" =
  print_endline (compile {|
type obj

@set
external set_value : obj -> int -> unit = "value"

@val
external create_obj : unit -> obj = "create_obj"

let o = create_obj ()
let _ = set_value o 42
|});
  [%expect {|
    local set_value = function(arg0)
      return function(arg1)
        return (function()
          arg0.value = arg1;
          return nil
        end)()
      end
    end;

    local function create_obj(arg0_1)
      return create_obj(arg0_1)
    end;
    local o = create_obj(nil);
    local top = set_value(o)(42)
    |}]

let%expect_test "codegen: @get_index generates bracket access" =
  print_endline (compile {|
type arr

@get_index
external array_get : arr -> int -> int = ""

@val
external create_array : unit -> arr = "create_array"

let a = create_array ()
let x = array_get a 0
|});
  [%expect {|
    local array_get = function(arg0)
      return function(arg1)
        return arg0[arg1]
      end
    end;

    local function create_array(arg0_1)
      return create_array(arg0_1)
    end;
    local a = create_array(nil);
    local x = array_get(a)(0)
    |}]

let%expect_test "codegen: @set_index generates bracket assignment" =
  print_endline (compile {|
type arr

@set_index
external array_set : arr -> int -> int -> unit = ""

@val
external create_array : unit -> arr = "create_array"

let a = create_array ()
let _ = array_set a 0 99
|});
  [%expect {|
    local array_set = function(arg0)
      return function(arg1)
        return function(arg2)
          return (function()
            arg0[arg1] = arg2;
            return nil
          end)()
        end
      end
    end;

    local function create_array(arg0_1)
      return create_array(arg0_1)
    end;
    local a = create_array(nil);
    local top = array_set(a)(0)(99)
    |}]

let%expect_test "codegen: @new generates constructor call" =
  print_endline (compile {|
type socket

@new
external create_socket : int -> socket = "Socket"

let sock = create_socket 8080
|});
  [%expect {|
    local function create_socket(arg0)
      return Socket.new(arg0)
    end;
    local sock = create_socket(8080)
    |}]

let%expect_test "codegen: @as overrides Lua name" =
  print_endline (compile {|
@val @as("rawprint")
external print_raw : string -> unit = "print"

let _ = print_raw "test"
|});
  [%expect {|
    local function print_raw(arg0)
      return rawprint(arg0)
    end;
    local top = print_raw("test")
    |}]

let%expect_test "codegen: @return(nullable) wraps result in option" =
  print_endline (compile {|
type 'a option = None | Some of 'a

@val @return(nullable)
external find_item : string -> int option = "find"

let result = find_item "key"
|});
  [%expect {|
    local function find_item(arg0)
      return (function()
        local _ffi_result = find(arg0);
        if _ffi_result == nil then
          return {_tag = 0}
        else
          return {_tag = 1, _0 = _ffi_result}
        end
      end)()
    end;
    local result = find_item("key")
    |}]

let%expect_test "codegen: @send @return(nullable) combines correctly" =
  print_endline (compile {|
type 'a option = None | Some of 'a
type obj

@send @return(nullable)
external lookup : obj -> string -> int option = "get"

@val
external create_obj : unit -> obj = "create_obj"

let o = create_obj ()
let result = lookup o "key"
|});
  [%expect {|
    local lookup = function(arg0)
      return function(arg1)
        return (function()
          local _ffi_result = arg0:get(arg1);
          if _ffi_result == nil then
            return {_tag = 0}
          else
            return {_tag = 1, _0 = _ffi_result}
          end
        end)()
      end
    end;

    local function create_obj(arg0_1)
      return create_obj(arg0_1)
    end;
    local o = create_obj(nil);
    local result = lookup(o)("key")
    |}]

let%expect_test "codegen: @variadic marks function as variadic" =
  print_endline (compile {|
@val @variadic
external concat_all : string -> string = "concat"

let result = concat_all "a"
|});
  [%expect {|
    local function concat_all(arg0)
      return concat(table.unpack(arg0))
    end;
    local result = concat_all("a")
    |}]

let%expect_test "codegen: @module @as combination" =
  print_endline (compile {|
@module("socket") @as("connect_tcp")
external tcp_connect : string -> int -> int = "tcp"

let conn = tcp_connect "localhost" 8080
|});
  [%expect {|
    local tcp_connect = function(arg0)
      return function(arg1)
        return require("socket").connect_tcp(arg0, arg1)
      end
    end;
    local conn = tcp_connect("localhost")(8080)
    |}]

(** {1 Variadic Code Generation Tests}

    Tests that @variadic correctly wraps the last argument with table.unpack() *)

let%expect_test "codegen: @variadic with multiple args wraps only last" =
  print_endline (compile {|
type 'a array

@val @variadic
external format : string -> 'a array -> string = "string.format"

@val
external get_args : unit -> int array = "get_args"

let args = get_args ()
let result = format "%d %d" args
|});
  [%expect {|
    local format = function(arg0)
      return function(arg1)
        return string.format(arg0, table.unpack(arg1))
      end
    end;

    local function get_args(arg0_1)
      return get_args(arg0_1)
    end;
    local args = get_args(nil);
    local result = format("%d %d")(args)
    |}]

let%expect_test "codegen: @variadic with @send preserves receiver" =
  print_endline (compile {|
type obj
type 'a array

@send @variadic
external call_method : obj -> string -> 'a array -> string = "format"

@val
external get_obj : unit -> obj = "get_obj"

@val
external get_args : unit -> int array = "get_args"

let o = get_obj ()
let args = get_args ()
let result = call_method o "template" args
|});
  [%expect {|
    local call_method = function(arg0)
      return function(arg1)
        return function(arg2)
          return arg0:format(arg1, table.unpack(arg2))
        end
      end
    end;

    local function get_obj(arg0_1)
      return get_obj(arg0_1)
    end;

    local function get_args(arg0_2)
      return get_args(arg0_2)
    end;
    local o = get_obj(nil);
    local args = get_args(nil);
    local result = call_method(o)("template")(args)
    |}]

let%expect_test "codegen: @variadic with @module" =
  print_endline (compile {|
type 'a array

@module("utils") @variadic
external log_args : string -> 'a array -> unit = "log"

@val
external get_data : unit -> int array = "get_data"

let data = get_data ()
let _ = log_args "message" data
|});
  [%expect {|
    local log_args = function(arg0)
      return function(arg1)
        return require("utils").log(arg0, table.unpack(arg1))
      end
    end;

    local function get_data(arg0_1)
      return get_data(arg0_1)
    end;
    local data = get_data(nil);
    local top = log_args("message")(data)
    |}]

let%expect_test "codegen: @variadic with @new constructor" =
  print_endline (compile {|
type buffer
type 'a array

@new @variadic
external create_buffer : 'a array -> buffer = "Buffer"

@val
external get_args : unit -> int array = "get_args"

let args = get_args ()
let buf = create_buffer args
|});
  [%expect {|
    local function create_buffer(arg0)
      return Buffer.new(table.unpack(arg0))
    end;

    local function get_args(arg0_1)
      return get_args(arg0_1)
    end;
    local args = get_args(nil);
    local buf = create_buffer(args)
    |}]

let%expect_test "codegen: @variadic with @return(nullable) combines correctly" =
  print_endline (compile {|
type 'a option = None | Some of 'a
type 'b array

@val @variadic @return(nullable)
external safe_format : string -> 'b array -> string option = "safe_format"

@val
external get_args : unit -> int array = "get_args"

let args = get_args ()
let result = safe_format "%s" args
|});
  [%expect {|
    local safe_format = function(arg0)
      return function(arg1)
        return (function()
          local _ffi_result = safe_format(arg0, table.unpack(arg1));
          if _ffi_result == nil then
            return {_tag = 0}
          else
            return {_tag = 1, _0 = _ffi_result}
          end
        end)()
      end
    end;

    local function get_args(arg0_1)
      return get_args(arg0_1)
    end;
    local args = get_args(nil);
    local result = safe_format("%s")(args)
    |}]

(** {1 Variadic Validation Error Tests} *)

let%expect_test "error: @variadic with arity 0" =
  let attrs = [
    make_attr "val" None;
    make_attr "variadic" None;
  ] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| @variadic requires 1 argument(s), but external has 0 |}]

(** {1 Additional Duplicate Attribute Tests} *)

let%expect_test "error: duplicate @get" =
  let attrs = [
    make_attr "get" None;
    make_attr "get" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @get appears multiple times |}]

let%expect_test "error: duplicate @set" =
  let attrs = [
    make_attr "set" None;
    make_attr "set" None;
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @set appears multiple times |}]

let%expect_test "error: duplicate @get_index" =
  let attrs = [
    make_attr "get_index" None;
    make_attr "get_index" None;
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @get_index appears multiple times |}]

let%expect_test "error: duplicate @set_index" =
  let attrs = [
    make_attr "set_index" None;
    make_attr "set_index" None;
  ] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @set_index appears multiple times |}]

let%expect_test "error: duplicate @new" =
  let attrs = [
    make_attr "new" None;
    make_attr "new" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @new appears multiple times |}]

let%expect_test "error: duplicate @scope" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "a"));
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "b"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @scope appears multiple times |}]

let%expect_test "error: duplicate @return" =
  let attrs = [
    make_attr "val" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @return appears multiple times |}]

let%expect_test "error: duplicate @as" =
  let attrs = [
    make_attr "val" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "a"));
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "b"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @as appears multiple times |}]

let%expect_test "error: duplicate @variadic" =
  let attrs = [
    make_attr "val" None;
    make_attr "variadic" None;
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Duplicate FFI attribute: @variadic appears multiple times |}]

(** {1 Additional Conflict Tests} *)

let%expect_test "error: @new and @get_index conflict" =
  let attrs = [
    make_attr "new" None;
    make_attr "get_index" None;
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @new and @get_index cannot be used together |}]

let%expect_test "error: @new and @set_index conflict" =
  let attrs = [
    make_attr "new" None;
    make_attr "set_index" None;
  ] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @new and @set_index cannot be used together |}]

let%expect_test "error: @new and @set conflict" =
  let attrs = [
    make_attr "new" None;
    make_attr "set" None;
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Conflicting FFI attributes: @new and @set cannot be used together |}]

(** {1 Additional Payload Validation Tests} *)

let%expect_test "error: @get with payload" =
  let attrs = [make_attr "get" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @get: expected no argument |}]

let%expect_test "error: @set with payload" =
  let attrs = [make_attr "set" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @set: expected no argument |}]

let%expect_test "error: @get_index with payload" =
  let attrs = [make_attr "get_index" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @get_index: expected no argument |}]

let%expect_test "error: @set_index with payload" =
  let attrs = [make_attr "set_index" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @set_index: expected no argument |}]

let%expect_test "error: @new with payload" =
  let attrs = [make_attr "new" (Some (Parsing_ffi.Attributes.PayloadString "x"))] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @new: expected no argument |}]

let%expect_test "error: @variadic with payload" =
  let attrs = [
    make_attr "val" None;
    make_attr "variadic" (Some (Parsing_ffi.Attributes.PayloadString "x"));
  ] in
  (match parse_attrs attrs with
   | Ok _ -> print_endline "ERROR: should have failed"
   | Error e -> print_error e);
  [%expect {| Invalid payload for @variadic: expected no argument |}]

(** {1 Additional Valid Arity Tests} *)

let%expect_test "valid: @send with arity 3" =
  let attrs = [make_attr "send" None] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Method lua_name=test variadic=false nullable=false arity=3 |}]

let%expect_test "valid: @new with arity 0" =
  let attrs = [make_attr "new" None] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=false nullable=false arity=0 |}]

let%expect_test "valid: @module with arity 0" =
  let attrs = [make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "socket"))] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Module(socket) lua_name=test variadic=false nullable=false arity=0 |}]

let%expect_test "valid: @val with arity 0" =
  let attrs = [make_attr "val" None] in
  (match parse_attrs_with_arity 0 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global lua_name=test variadic=false nullable=false arity=0 |}]

(** {1 Additional Valid Combination Tests} *)

let%expect_test "valid: @module with @variadic" =
  let attrs = [
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "utils"));
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Module(utils) lua_name=test variadic=true nullable=false arity=1 |}]

let%expect_test "valid: @new with @module" =
  let attrs = [
    make_attr "new" None;
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "socket"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=false nullable=false arity=1 |}]

let%expect_test "valid: @new with @as" =
  let attrs = [
    make_attr "new" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "create"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=create variadic=false nullable=false arity=1 |}]

let%expect_test "valid: @new with @variadic" =
  let attrs = [
    make_attr "new" None;
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=true nullable=false arity=1 |}]

let%expect_test "valid: @get with @as" =
  let attrs = [
    make_attr "get" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "length"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Getter lua_name=length variadic=false nullable=false arity=1 |}]

let%expect_test "valid: @set with @as" =
  let attrs = [
    make_attr "set" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "value"));
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Setter lua_name=value variadic=false nullable=false arity=2 |}]

let%expect_test "valid: @get_index with @as" =
  let attrs = [
    make_attr "get_index" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "at"));
  ] in
  (match parse_attrs_with_arity 2 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=IndexGetter lua_name=at variadic=false nullable=false arity=2 |}]

let%expect_test "valid: @set_index with @as" =
  let attrs = [
    make_attr "set_index" None;
    make_attr "as" (Some (Parsing_ffi.Attributes.PayloadString "put"));
  ] in
  (match parse_attrs_with_arity 3 attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=IndexSetter lua_name=put variadic=false nullable=false arity=3 |}]

let%expect_test "valid: @get with @return(nullable)" =
  let attrs = [
    make_attr "get" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Getter lua_name=test variadic=false nullable=true arity=1 |}]

let%expect_test "valid: @new with @return(nullable)" =
  let attrs = [
    make_attr "new" None;
    make_attr "return" (Some (Parsing_ffi.Attributes.PayloadIdent "nullable"));
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=false nullable=true arity=1 |}]

let%expect_test "valid: @new with @module and @variadic" =
  let attrs = [
    make_attr "new" None;
    make_attr "module" (Some (Parsing_ffi.Attributes.PayloadString "buffer"));
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Constructor lua_name=test variadic=true nullable=false arity=1 |}]

let%expect_test "valid: @val with @scope and @variadic" =
  let attrs = [
    make_attr "val" None;
    make_attr "scope" (Some (Parsing_ffi.Attributes.PayloadString "string"));
    make_attr "variadic" None;
  ] in
  (match parse_attrs attrs with
   | Ok spec -> print_ffi_spec spec
   | Error e -> print_error e);
  [%expect {| kind=Global(string) lua_name=test variadic=true nullable=false arity=1 |}]
