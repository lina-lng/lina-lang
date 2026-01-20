(** FFI attribute validation and semantic analysis.

    This module converts parsed FFI attributes into validated semantic
    representations, checking for conflicts and arity requirements. *)

open Common

(** Error type for FFI validation failures. *)
type ffi_error =
  | ConflictingAttributes of string * string
    (** Two attributes that cannot be used together *)
  | MissingRequiredAttribute of string
    (** An attribute that must be present *)
  | InvalidPayload of string * string
    (** Attribute name and description of expected payload *)
  | ArityMismatch of string * int * int
    (** Attribute name, expected arity, actual arity *)
  | DuplicateAttribute of string
    (** Same attribute appears multiple times *)
  | PolymorphicExternal
    (** Warning: external has polymorphic type *)
[@@deriving show]

(** Convert an error to a human-readable message. *)
let error_message = function
  | ConflictingAttributes (attr1, attr2) ->
    Printf.sprintf "Conflicting FFI attributes: @%s and @%s cannot be used together" attr1 attr2
  | MissingRequiredAttribute attr ->
    Printf.sprintf "Missing required FFI attribute: @%s" attr
  | InvalidPayload (attr, expected) ->
    Printf.sprintf "Invalid payload for @%s: expected %s" attr expected
  | ArityMismatch (attr, expected, actual) ->
    Printf.sprintf "@%s requires %d argument(s), but external has %d" attr expected actual
  | DuplicateAttribute attr ->
    Printf.sprintf "Duplicate FFI attribute: @%s appears multiple times" attr
  | PolymorphicExternal ->
    "Warning: external declaration has polymorphic type (may cause runtime errors)"

(** Parse a single attribute from AST form to semantic form.
    Returns None for unknown attributes (allows future extension). *)
let parse_attribute (attr : Parsing_ffi.Attributes.attribute) : (Types.ffi_attribute option, ffi_error) result =
  let open Parsing_ffi.Attributes in
  match attr.attribute_name, attr.attribute_payload with
  | "module", Some (PayloadString s) ->
    Ok (Some (Types.FFIModule s))
  | "module", None ->
    Error (InvalidPayload ("module", "string argument: @module(\"path\")"))
  | "module", Some (PayloadStringList _) ->
    Error (InvalidPayload ("module", "single string, not a list"))

  | "val", None ->
    Ok (Some Types.FFIVal)
  | "val", Some _ ->
    Error (InvalidPayload ("val", "no argument"))

  | "scope", Some (PayloadString s) ->
    Ok (Some (Types.FFIScope [s]))
  | "scope", Some (PayloadStringList ss) ->
    Ok (Some (Types.FFIScope ss))
  | "scope", None ->
    Error (InvalidPayload ("scope", "string or string list: @scope(\"name\") or @scope((\"a\", \"b\"))"))

  | "send", None ->
    Ok (Some Types.FFISend)
  | "send", Some _ ->
    Error (InvalidPayload ("send", "no argument"))

  | "get", None ->
    Ok (Some Types.FFIGet)
  | "get", Some _ ->
    Error (InvalidPayload ("get", "no argument"))

  | "set", None ->
    Ok (Some Types.FFISet)
  | "set", Some _ ->
    Error (InvalidPayload ("set", "no argument"))

  | "get_index", None ->
    Ok (Some Types.FFIGetIndex)
  | "get_index", Some _ ->
    Error (InvalidPayload ("get_index", "no argument"))

  | "set_index", None ->
    Ok (Some Types.FFISetIndex)
  | "set_index", Some _ ->
    Error (InvalidPayload ("set_index", "no argument"))

  | "new", None ->
    Ok (Some Types.FFINew)
  | "new", Some _ ->
    Error (InvalidPayload ("new", "no argument"))

  | "variadic", None ->
    Ok (Some Types.FFIVariadic)
  | "variadic", Some _ ->
    Error (InvalidPayload ("variadic", "no argument"))

  | "return", Some (PayloadString "nullable")
  | "return", Some (PayloadIdent "nullable") ->
    Ok (Some Types.FFIReturnNullable)
  | "return", Some (PayloadString "pcall")
  | "return", Some (PayloadIdent "pcall") ->
    Ok (Some Types.FFIReturnPcall)
  | "return", Some (PayloadString s) ->
    Error (InvalidPayload ("return", Printf.sprintf "\"nullable\" or \"pcall\", got \"%s\"" s))
  | "return", Some (PayloadIdent s) ->
    Error (InvalidPayload ("return", Printf.sprintf "nullable or pcall, got %s" s))
  | "return", None ->
    Error (InvalidPayload ("return", "argument: @return(nullable) or @return(pcall)"))
  | "return", Some (PayloadStringList _) ->
    Error (InvalidPayload ("return", "single identifier \"nullable\" or \"pcall\""))

  | "as", Some (PayloadString s) ->
    Ok (Some (Types.FFIAs s))
  | "as", None ->
    Error (InvalidPayload ("as", "string argument: @as(\"lua_name\")"))
  | "as", Some (PayloadStringList _) ->
    Error (InvalidPayload ("as", "single string, not a list"))

  (* Unknown attribute - ignore for forward compatibility *)
  | _, _ ->
    Ok None

(** Parse all attributes from AST form to semantic form. *)
let parse_attributes (attrs : Parsing_ffi.Attributes.attribute list) : (Types.ffi_attribute list, ffi_error) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | attr :: rest ->
      match parse_attribute attr with
      | Error e -> Error e
      | Ok None -> loop acc rest
      | Ok (Some ffi_attr) -> loop (ffi_attr :: acc) rest
  in
  loop [] attrs

(** Check for duplicate attributes. *)
let check_duplicates (attrs : Types.ffi_attribute list) : (unit, ffi_error) result =
  let attr_name = function
    | Types.FFIModule _ -> "module"
    | Types.FFIVal -> "val"
    | Types.FFIScope _ -> "scope"
    | Types.FFISend -> "send"
    | Types.FFIGet -> "get"
    | Types.FFISet -> "set"
    | Types.FFIGetIndex -> "get_index"
    | Types.FFISetIndex -> "set_index"
    | Types.FFINew -> "new"
    | Types.FFIVariadic -> "variadic"
    | Types.FFIReturnNullable -> "return"
    | Types.FFIReturnPcall -> "return"
    | Types.FFIAs _ -> "as"
  in
  let seen = Hashtbl.create 16 in
  let rec check = function
    | [] -> Ok ()
    | attr :: rest ->
      let name = attr_name attr in
      if Hashtbl.mem seen name then
        Error (DuplicateAttribute name)
      else begin
        Hashtbl.add seen name ();
        check rest
      end
  in
  check attrs

(** Check for conflicting attribute combinations. *)
let check_conflicts (attrs : Types.ffi_attribute list) : (unit, ffi_error) result =
  let has_module = List.exists (function Types.FFIModule _ -> true | _ -> false) attrs in
  let has_val = List.exists (function Types.FFIVal -> true | _ -> false) attrs in
  let has_send = List.exists (function Types.FFISend -> true | _ -> false) attrs in
  let has_get = List.exists (function Types.FFIGet -> true | _ -> false) attrs in
  let has_set = List.exists (function Types.FFISet -> true | _ -> false) attrs in
  let has_get_index = List.exists (function Types.FFIGetIndex -> true | _ -> false) attrs in
  let has_set_index = List.exists (function Types.FFISetIndex -> true | _ -> false) attrs in
  let has_new = List.exists (function Types.FFINew -> true | _ -> false) attrs in

  (* @module and @val are mutually exclusive *)
  if has_module && has_val then
    Error (ConflictingAttributes ("module", "val"))
  (* Method/property attributes are mutually exclusive *)
  else if has_send && has_get then
    Error (ConflictingAttributes ("send", "get"))
  else if has_send && has_set then
    Error (ConflictingAttributes ("send", "set"))
  else if has_send && has_get_index then
    Error (ConflictingAttributes ("send", "get_index"))
  else if has_send && has_set_index then
    Error (ConflictingAttributes ("send", "set_index"))
  else if has_get && has_set then
    Error (ConflictingAttributes ("get", "set"))
  else if has_get && has_get_index then
    Error (ConflictingAttributes ("get", "get_index"))
  else if has_get && has_set_index then
    Error (ConflictingAttributes ("get", "set_index"))
  else if has_set && has_get_index then
    Error (ConflictingAttributes ("set", "get_index"))
  else if has_set && has_set_index then
    Error (ConflictingAttributes ("set", "set_index"))
  else if has_get_index && has_set_index then
    Error (ConflictingAttributes ("get_index", "set_index"))
  (* @new can combine with @module but not with method/property attrs *)
  else if has_new && has_send then
    Error (ConflictingAttributes ("new", "send"))
  else if has_new && has_get then
    Error (ConflictingAttributes ("new", "get"))
  else if has_new && has_set then
    Error (ConflictingAttributes ("new", "set"))
  else if has_new && has_get_index then
    Error (ConflictingAttributes ("new", "get_index"))
  else if has_new && has_set_index then
    Error (ConflictingAttributes ("new", "set_index"))
  else
    Ok ()

(** Determine the FFI kind from attributes.
    Defaults to global call if no specific kind is specified. *)
let determine_kind (attrs : Types.ffi_attribute list) : Types.ffi_kind =
  let module_path = List.find_map (function Types.FFIModule s -> Some s | _ -> None) attrs in
  let scope_path = List.find_map (function Types.FFIScope ss -> Some ss | _ -> None) attrs in
  let has_send = List.exists (function Types.FFISend -> true | _ -> false) attrs in
  let has_get = List.exists (function Types.FFIGet -> true | _ -> false) attrs in
  let has_set = List.exists (function Types.FFISet -> true | _ -> false) attrs in
  let has_get_index = List.exists (function Types.FFIGetIndex -> true | _ -> false) attrs in
  let has_set_index = List.exists (function Types.FFISetIndex -> true | _ -> false) attrs in
  let has_new = List.exists (function Types.FFINew -> true | _ -> false) attrs in

  if has_send then Types.FFIKindMethod
  else if has_get then Types.FFIKindGetter
  else if has_set then Types.FFIKindSetter
  else if has_get_index then Types.FFIKindIndexGetter
  else if has_set_index then Types.FFIKindIndexSetter
  else if has_new then Types.FFIKindConstructor
  else match module_path with
    | Some path -> Types.FFIKindModule path
    | None ->
      let scope = Option.value scope_path ~default:[] in
      Types.FFIKindGlobal scope

(** Check arity constraints for the FFI kind. *)
let check_arity (kind : Types.ffi_kind) (arity : int) : (unit, ffi_error) result =
  match kind with
  | Types.FFIKindMethod ->
    if arity < 1 then
      Error (ArityMismatch ("send", 1, arity))
    else
      Ok ()
  | Types.FFIKindGetter ->
    if arity <> 1 then
      Error (ArityMismatch ("get", 1, arity))
    else
      Ok ()
  | Types.FFIKindSetter ->
    if arity <> 2 then
      Error (ArityMismatch ("set", 2, arity))
    else
      Ok ()
  | Types.FFIKindIndexGetter ->
    if arity <> 2 then
      Error (ArityMismatch ("get_index", 2, arity))
    else
      Ok ()
  | Types.FFIKindIndexSetter ->
    if arity <> 3 then
      Error (ArityMismatch ("set_index", 3, arity))
    else
      Ok ()
  | Types.FFIKindModule _ | Types.FFIKindGlobal _ | Types.FFIKindConstructor ->
    Ok ()

(** Get the Lua name from attributes, falling back to primitive name. *)
let get_lua_name (attrs : Types.ffi_attribute list) (primitive : string) : string =
  match List.find_map (function Types.FFIAs s -> Some s | _ -> None) attrs with
  | Some name -> name
  | None -> primitive

(** Check if return type should be wrapped in option. *)
let is_return_nullable (attrs : Types.ffi_attribute list) : bool =
  List.exists (function Types.FFIReturnNullable -> true | _ -> false) attrs

(** Check if call should be wrapped in pcall and return result. *)
let is_return_pcall (attrs : Types.ffi_attribute list) : bool =
  List.exists (function Types.FFIReturnPcall -> true | _ -> false) attrs

(** Check if function is variadic. *)
let is_variadic (attrs : Types.ffi_attribute list) : bool =
  List.exists (function Types.FFIVariadic -> true | _ -> false) attrs

(** Build a complete FFI specification from a parsed external declaration.

    @param attrs The parsed attributes from the AST
    @param primitive The primitive string (Lua function name)
    @param arity The number of arguments (computed from the type)
    @param unit_params Which parameters are unit type (should not be passed to Lua)
    @param location Source location for error reporting
    @return The validated FFI spec or an error *)
let build_ffi_spec
    ~(attrs : Parsing_ffi.Attributes.attribute list)
    ~(primitive : string)
    ~(arity : int)
    ~(unit_params : bool list)
    ~(location : Location.t)
  : (Types.ffi_spec, ffi_error) result =
  (* Parse attributes *)
  match parse_attributes attrs with
  | Error e -> Error e
  | Ok ffi_attrs ->
    (* Check for duplicates *)
    match check_duplicates ffi_attrs with
    | Error e -> Error e
    | Ok () ->
      (* Check for conflicts *)
      match check_conflicts ffi_attrs with
      | Error e -> Error e
      | Ok () ->
        let kind = determine_kind ffi_attrs in
        (* Check arity constraints *)
        match check_arity kind arity with
        | Error e -> Error e
        | Ok () ->
          (* Check variadic requires at least 1 argument *)
          let variadic = is_variadic ffi_attrs in
          if variadic && arity < 1 then
            Error (ArityMismatch ("variadic", 1, arity))
          else
            Ok {
              Types.ffi_kind = kind;
              ffi_lua_name = get_lua_name ffi_attrs primitive;
              ffi_is_variadic = variadic;
              ffi_return_nullable = is_return_nullable ffi_attrs;
              ffi_return_pcall = is_return_pcall ffi_attrs;
              ffi_arity = arity;
              ffi_unit_params = unit_params;
              ffi_location = location;
            }
