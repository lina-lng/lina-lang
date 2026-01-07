(** Unit tests for Type_utils module.

    Tests cover:
    - Type parameter substitution
    - Constructor instantiation
    - Path prefix substitution
    - Path substitution in types, schemes, signatures, and module types *)

open Typing

(** Helper to create a type variable for testing *)
let make_type_var id level =
  let tv = {
    Types.id;
    level;
    link = None;
  } in
  (tv, Types.TypeVariable tv)

(** {1 Type Parameter Substitution Tests} *)

let%expect_test "substitute_type_params: simple variable replacement" =
  let tv, ty_var = make_type_var 0 1 in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_type_params [tv] [int_ty] ty_var in
  (match result with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) -> print_endline "int"
   | _ -> print_endline "ERROR");
  [%expect {| int |}]

let%expect_test "substitute_type_params: nested arrow type" =
  let tv, ty_var = make_type_var 0 1 in
  let arrow_ty = Types.TypeArrow (ty_var, ty_var) in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_type_params [tv] [int_ty] arrow_ty in
  (match result with
   | Types.TypeArrow (
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []),
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])) ->
     print_endline "int -> int"
   | _ -> print_endline "ERROR");
  [%expect {| int -> int |}]

let%expect_test "substitute_type_params: tuple type" =
  let tv1, ty_var1 = make_type_var 0 1 in
  let tv2, ty_var2 = make_type_var 1 1 in
  let tuple_ty = Types.TypeTuple [ty_var1; ty_var2] in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let string_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, []) in
  let result = Type_utils.substitute_type_params [tv1; tv2] [int_ty; string_ty] tuple_ty in
  (match result with
   | Types.TypeTuple [
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []);
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])] ->
     print_endline "int * string"
   | _ -> print_endline "ERROR");
  [%expect {| int * string |}]

let%expect_test "substitute_type_params: type constructor with args" =
  let tv, ty_var = make_type_var 0 1 in
  let option_ty = Types.TypeConstructor (Types.PathLocal "option", [ty_var]) in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_type_params [tv] [int_ty] option_ty in
  (match result with
   | Types.TypeConstructor (Types.PathLocal "option",
       [Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])]) ->
     print_endline "int option"
   | _ -> print_endline "ERROR");
  [%expect {| int option |}]

let%expect_test "substitute_type_params: empty params list (identity)" =
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_type_params [] [] int_ty in
  (match result with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) -> print_endline "unchanged"
   | _ -> print_endline "ERROR");
  [%expect {| unchanged |}]

let%expect_test "substitute_type_params: unrelated variable unchanged" =
  let tv1, _ = make_type_var 0 1 in
  let _, ty_var2 = make_type_var 1 1 in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_type_params [tv1] [int_ty] ty_var2 in
  (match result with
   | Types.TypeVariable tv when tv.id = 1 -> print_endline "unchanged var"
   | _ -> print_endline "ERROR");
  [%expect {| unchanged var |}]

(** {1 Constructor Instantiation Tests} *)

let%expect_test "instantiate_constructor: nullary constructor" =
  (* A constructor like None : 'a option with no argument *)
  Types.reset_level ();
  let param_tv = { Types.id = 0; level = 0; link = None } in
  let result_type = Types.TypeConstructor (Types.PathLocal "option", [Types.TypeVariable param_tv]) in
  let ctor_info = {
    Types.constructor_name = "None";
    constructor_tag_index = 0;
    constructor_type_name = "option";
    constructor_argument_type = None;
    constructor_result_type = result_type;
    constructor_type_parameters = [param_tv];
  } in
  let (arg_opt, result_ty) = Type_utils.instantiate_constructor ctor_info in
  print_endline (match arg_opt with None -> "no arg" | Some _ -> "has arg");
  (match result_ty with
   | Types.TypeConstructor (Types.PathLocal "option", [Types.TypeVariable _]) ->
     print_endline "option type"
   | _ -> print_endline "ERROR");
  [%expect {|
    no arg
    option type |}]

let%expect_test "instantiate_constructor: unary constructor" =
  (* A constructor like Some : 'a -> 'a option *)
  Types.reset_level ();
  let param_tv = { Types.id = 0; level = 0; link = None } in
  let result_type = Types.TypeConstructor (Types.PathLocal "option", [Types.TypeVariable param_tv]) in
  let ctor_info = {
    Types.constructor_name = "Some";
    constructor_tag_index = 1;
    constructor_type_name = "option";
    constructor_argument_type = Some (Types.TypeVariable param_tv);
    constructor_result_type = result_type;
    constructor_type_parameters = [param_tv];
  } in
  let (arg_opt, result_ty) = Type_utils.instantiate_constructor ctor_info in
  print_endline (match arg_opt with None -> "no arg" | Some (Types.TypeVariable _) -> "var arg" | _ -> "other");
  (match result_ty with
   | Types.TypeConstructor (Types.PathLocal "option", [Types.TypeVariable _]) ->
     print_endline "option type"
   | _ -> print_endline "ERROR");
  [%expect {|
    var arg
    option type |}]

(** {1 Path Prefix Substitution Tests} *)

let%expect_test "substitute_path_prefix: simple replacement" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let result = Type_utils.substitute_path_prefix ~old_path ~new_path old_path in
  (match result with
   | Types.PathIdent id when Common.Identifier.name id = "N" -> print_endline "N"
   | _ -> print_endline "ERROR");
  [%expect {| N |}]

let%expect_test "substitute_path_prefix: nested path M.t -> N.t" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let input_path = Types.PathDot (old_path, "t") in
  let result = Type_utils.substitute_path_prefix ~old_path ~new_path input_path in
  (match result with
   | Types.PathDot (Types.PathIdent id, "t") when Common.Identifier.name id = "N" ->
     print_endline "N.t"
   | _ -> print_endline "ERROR");
  [%expect {| N.t |}]

let%expect_test "substitute_path_prefix: unrelated path unchanged" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let other_path = Types.PathIdent (Common.Identifier.create "X") in
  let result = Type_utils.substitute_path_prefix ~old_path ~new_path other_path in
  (match result with
   | Types.PathIdent id when Common.Identifier.name id = "X" -> print_endline "unchanged"
   | _ -> print_endline "ERROR");
  [%expect {| unchanged |}]

let%expect_test "substitute_path_prefix: builtin path unchanged" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let builtin_path = Types.PathBuiltin Types.BuiltinInt in
  let result = Type_utils.substitute_path_prefix ~old_path ~new_path builtin_path in
  (match result with
   | Types.PathBuiltin Types.BuiltinInt -> print_endline "int unchanged"
   | _ -> print_endline "ERROR");
  [%expect {| int unchanged |}]

(** {1 Path Substitution in Types Tests} *)

let%expect_test "substitute_path_in_type: TypeConstructor with path" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let type_path = Types.PathDot (old_path, "t") in
  let ty = Types.TypeConstructor (type_path, []) in
  let result = Type_utils.substitute_path_in_type ~old_path ~new_path ty in
  (match result with
   | Types.TypeConstructor (Types.PathDot (Types.PathIdent id, "t"), [])
     when Common.Identifier.name id = "N" ->
     print_endline "N.t"
   | _ -> print_endline "ERROR");
  [%expect {| N.t |}]

let%expect_test "substitute_path_in_type: nested in arrow" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let type_path = Types.PathDot (old_path, "t") in
  let m_t = Types.TypeConstructor (type_path, []) in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let arrow_ty = Types.TypeArrow (m_t, int_ty) in
  let result = Type_utils.substitute_path_in_type ~old_path ~new_path arrow_ty in
  (match result with
   | Types.TypeArrow (
       Types.TypeConstructor (Types.PathDot (Types.PathIdent id, "t"), []),
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []))
     when Common.Identifier.name id = "N" ->
     print_endline "N.t -> int"
   | _ -> print_endline "ERROR");
  [%expect {| N.t -> int |}]

(** {1 Path Substitution in Scheme Tests} *)

let%expect_test "substitute_path_in_scheme: preserves quantifiers" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let type_path = Types.PathDot (old_path, "t") in
  let tv = { Types.id = 0; level = 1; link = None } in
  let body = Types.TypeArrow (
    Types.TypeVariable tv,
    Types.TypeConstructor (type_path, [])
  ) in
  let scheme = { Types.quantified_variables = [tv]; body } in
  let result = Type_utils.substitute_path_in_scheme ~old_path ~new_path scheme in
  print_int (List.length result.quantified_variables);
  print_newline ();
  (match result.body with
   | Types.TypeArrow (Types.TypeVariable _,
       Types.TypeConstructor (Types.PathDot (Types.PathIdent id, "t"), []))
     when Common.Identifier.name id = "N" ->
     print_endline "quantifier preserved, path updated"
   | _ -> print_endline "ERROR");
  [%expect {|
    1
    quantifier preserved, path updated |}]

(** {1 Path Substitution in Module Type Tests} *)

let%expect_test "substitute_path_in_module_type: signature" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let type_path = Types.PathDot (old_path, "t") in
  let ty = Types.TypeConstructor (type_path, []) in
  let scheme = { Types.quantified_variables = []; body = ty } in
  let sig_item = Module_types.SigValue ("x", { val_type = scheme; val_location = Common.Location.none }) in
  let mty = Module_types.ModTypeSig [sig_item] in
  let result = Type_utils.substitute_path_in_module_type ~old_path ~new_path mty in
  (match result with
   | Module_types.ModTypeSig [Module_types.SigValue ("x", { val_type; _ })] ->
     (match val_type.body with
      | Types.TypeConstructor (Types.PathDot (Types.PathIdent id, "t"), [])
        when Common.Identifier.name id = "N" ->
        print_endline "path updated in signature"
      | _ -> print_endline "ERROR: path not updated")
   | _ -> print_endline "ERROR: wrong structure");
  [%expect {| path updated in signature |}]

let%expect_test "substitute_path_in_module_type: functor" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  let param_id = Common.Identifier.create "X" in
  let param = {
    Module_types.param_name = "X";
    param_id;
    param_type = Module_types.ModTypeSig [];
  } in
  let type_path = Types.PathDot (old_path, "t") in
  let ty = Types.TypeConstructor (type_path, []) in
  let scheme = { Types.quantified_variables = []; body = ty } in
  let result_sig = Module_types.ModTypeSig [
    Module_types.SigValue ("v", { val_type = scheme; val_location = Common.Location.none })
  ] in
  let mty = Module_types.ModTypeFunctor (param, result_sig) in
  let result = Type_utils.substitute_path_in_module_type ~old_path ~new_path mty in
  (match result with
   | Module_types.ModTypeFunctor (_, Module_types.ModTypeSig [
       Module_types.SigValue ("v", { val_type; _ })]) ->
     (match val_type.body with
      | Types.TypeConstructor (Types.PathDot (Types.PathIdent id, "t"), [])
        when Common.Identifier.name id = "N" ->
        print_endline "path updated in functor result"
      | _ -> print_endline "ERROR")
   | _ -> print_endline "ERROR");
  [%expect {| path updated in functor result |}]

(** {1 Structural Sharing Tests} *)

let%expect_test "substitute_path_in_type: unchanged path returns same structure" =
  let old_path = Types.PathIdent (Common.Identifier.create "M") in
  let new_path = Types.PathIdent (Common.Identifier.create "N") in
  (* Use builtin type that won't be affected by substitution *)
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let result = Type_utils.substitute_path_in_type ~old_path ~new_path int_ty in
  (* Should be physically equal due to structural sharing optimization *)
  print_endline (if result == int_ty then "same object" else "different object");
  [%expect {| same object |}]
