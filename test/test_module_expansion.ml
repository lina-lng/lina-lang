(** Tests for the Module_expansion module.

    Tests robust module type expansion with cycle detection. *)

open Typing

(** Helper to format module type for display. *)
let rec format_module_type = function
  | Module_types.ModTypeSig items ->
    let items_str = List.map format_sig_item items |> String.concat "; " in
    Printf.sprintf "sig [%s]" items_str
  | Module_types.ModTypeFunctor (param, result) ->
    Printf.sprintf "functor (%s : %s) -> %s"
      param.Module_types.parameter_name
      (format_module_type param.Module_types.parameter_type)
      (format_module_type result)
  | Module_types.ModTypeIdent path ->
    Printf.sprintf "ident(%s)" (Types.path_to_string path)

and format_sig_item = function
  | Module_types.SigValue (name, _) -> Printf.sprintf "val %s" name
  | Module_types.SigType (name, _) -> Printf.sprintf "type %s" name
  | Module_types.SigModule (name, mty) ->
    Printf.sprintf "module %s : %s" name (format_module_type mty)
  | Module_types.SigModuleType (name, _) -> Printf.sprintf "module type %s" name

(** Helper to create a simple signature with a value. *)
let make_sig_with_value name ty =
  Module_types.ModTypeSig [
    Module_types.SigValue (name, {
      Module_types.value_type = ty;
      value_location = Common.Location.none;
    })
  ]

(** Helper to create a simple signature with a type. *)
let make_sig_with_type name =
  let decl = {
    Types.declaration_name = name;
    declaration_parameters = [];
    declaration_manifest = None;
    declaration_kind = Types.DeclarationAbstract;
    declaration_variances = [];
  } in
  Module_types.ModTypeSig [Module_types.SigType (name, decl)]

(** {1 create_state tests} *)

let%expect_test "create_state: basic creation works" =
  (* Just verify create_state doesn't throw and the state can be used *)
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let sig_ = make_sig_with_type "test" in
  let result = Module_expansion.try_expand state sig_ in
  begin match result with
  | Some _ -> print_endline "state works"
  | None -> print_endline "unexpected failure"
  end;
  [%expect {| state works |}]

(** {1 expand tests with ModTypeSig} *)

let%expect_test "expand: signature is preserved" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ty_scheme = { Types.quantified_variables = []; body = Types.type_int } in
  let sig_ = make_sig_with_value "x" ty_scheme in
  let expanded = Module_expansion.expand state sig_ in
  print_endline (format_module_type expanded);
  [%expect {| sig [val x] |}]

let%expect_test "expand: nested module in signature" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let inner = make_sig_with_type "t" in
  let outer = Module_types.ModTypeSig [
    Module_types.SigModule ("Inner", inner)
  ] in
  let expanded = Module_expansion.expand state outer in
  print_endline (format_module_type expanded);
  [%expect {| sig [module Inner : sig [type t]] |}]

(** {1 expand tests with ModTypeFunctor} *)

let%expect_test "expand: functor type is preserved" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let param_sig = make_sig_with_type "t" in
  let result_sig = make_sig_with_type "s" in
  let functor_type = Module_types.ModTypeFunctor (
    { Module_types.parameter_name = "X";
      parameter_id = Common.Identifier.create "X";
      parameter_type = param_sig },
    result_sig
  ) in
  let expanded = Module_expansion.expand state functor_type in
  print_endline (format_module_type expanded);
  [%expect {| functor (X : sig [type t]) -> sig [type s] |}]

(** {1 expand tests with ModTypeIdent} *)

let%expect_test "expand: ident is resolved" =
  let target_sig = make_sig_with_type "resolved" in
  let lookup path =
    match path with
    | Types.PathLocal "S" -> Some target_sig
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let expanded = Module_expansion.expand state ident_type in
  print_endline (format_module_type expanded);
  [%expect {| sig [type resolved] |}]

let%expect_test "expand: unresolvable ident raises" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "Unknown") in
  let result =
    try
      let _ = Module_expansion.expand state ident_type in
      "no error"
    with Module_expansion.Expansion_error err ->
      Module_expansion.format_error err
  in
  print_endline result;
  [%expect {| Module type not found: Unknown |}]

(** {1 Cycle detection tests} *)

let%expect_test "expand: direct cycle is detected" =
  (* S = S (direct self-reference) *)
  let lookup path =
    match path with
    | Types.PathLocal "S" ->
      Some (Module_types.ModTypeIdent (Types.PathLocal "S"))
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let result =
    try
      let _ = Module_expansion.expand state ident_type in
      "no error"
    with Module_expansion.Expansion_error err ->
      Module_expansion.format_error err
  in
  print_endline result;
  [%expect {|
    Cyclic module type definition detected: S -> S

    Module type definitions cannot reference themselves, directly or indirectly. |}]

let%expect_test "expand: indirect cycle is detected" =
  (* S = T, T = S (indirect cycle) *)
  let lookup path =
    match path with
    | Types.PathLocal "S" ->
      Some (Module_types.ModTypeIdent (Types.PathLocal "T"))
    | Types.PathLocal "T" ->
      Some (Module_types.ModTypeIdent (Types.PathLocal "S"))
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let result =
    try
      let _ = Module_expansion.expand state ident_type in
      "no error"
    with Module_expansion.Expansion_error _ ->
      (* Just check that a cycle is detected - exact path may vary *)
      "cycle detected"
  in
  print_endline result;
  [%expect {| cycle detected |}]

(** {1 try_expand tests} *)

let%expect_test "try_expand: success returns Some" =
  let target_sig = make_sig_with_type "t" in
  let lookup path =
    match path with
    | Types.PathLocal "S" -> Some target_sig
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let result = Module_expansion.try_expand state ident_type in
  begin match result with
  | Some mty -> print_endline (format_module_type mty)
  | None -> print_endline "None"
  end;
  [%expect {| sig [type t] |}]

let%expect_test "try_expand: failure returns None" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "Unknown") in
  let result = Module_expansion.try_expand state ident_type in
  begin match result with
  | Some _ -> print_endline "Some"
  | None -> print_endline "None"
  end;
  [%expect {| None |}]

let%expect_test "try_expand: cycle returns None" =
  let lookup path =
    match path with
    | Types.PathLocal "S" ->
      Some (Module_types.ModTypeIdent (Types.PathLocal "S"))
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let result = Module_expansion.try_expand state ident_type in
  begin match result with
  | Some _ -> print_endline "Some"
  | None -> print_endline "None"
  end;
  [%expect {| None |}]

(** {1 expand_shallow tests} *)

let%expect_test "expand_shallow: one level only" =
  let inner_sig = make_sig_with_type "inner" in
  let lookup path =
    match path with
    | Types.PathLocal "S" ->
      Some (Module_types.ModTypeIdent (Types.PathLocal "T"))
    | Types.PathLocal "T" -> Some inner_sig
    | _ -> None
  in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let ident_type = Module_types.ModTypeIdent (Types.PathLocal "S") in
  let expanded = Module_expansion.expand_shallow state ident_type in
  (* Should only expand one level: S -> T, not T -> sig *)
  print_endline (format_module_type expanded);
  [%expect {| ident(T) |}]

let%expect_test "expand_shallow: non-ident unchanged" =
  let lookup _ = None in
  let state = Module_expansion.create_state ~env_lookup:lookup in
  let sig_ = make_sig_with_type "t" in
  let expanded = Module_expansion.expand_shallow state sig_ in
  print_endline (format_module_type expanded);
  [%expect {| sig [type t] |}]

(** {1 format_error tests} *)

let%expect_test "format_error: ModuleTypeNotFound" =
  let err = Module_expansion.ModuleTypeNotFound (Types.PathLocal "Missing") in
  print_endline (Module_expansion.format_error err);
  [%expect {| Module type not found: Missing |}]

let%expect_test "format_error: CyclicModuleType" =
  let err = Module_expansion.CyclicModuleType [Types.PathLocal "A"; Types.PathLocal "B"] in
  print_endline (Module_expansion.format_error err);
  [%expect {|
    Cyclic module type definition detected: A -> B

    Module type definitions cannot reference themselves, directly or indirectly. |}]

let%expect_test "format_error: PathResolutionFailed" =
  let err = Module_expansion.PathResolutionFailed (Types.PathLocal "M", "not a module") in
  print_endline (Module_expansion.format_error err);
  [%expect {| Cannot resolve module path M: not a module |}]
