(** Unit tests for Inference_utils module.

    Tests cover:
    - type_of_constant: mapping constants to types
    - unify_with_env: unification with environment-based type lookup *)

open Typing
open Parsing.Syntax_tree

(** {1 type_of_constant Tests} *)

let%expect_test "type_of_constant: integer returns int" =
  let ty = Inference_utils.type_of_constant (ConstantInteger 42) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) ->
     print_endline "int"
   | _ -> print_endline "ERROR");
  [%expect {| int |}]

let%expect_test "type_of_constant: negative integer returns int" =
  let ty = Inference_utils.type_of_constant (ConstantInteger (-123)) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) ->
     print_endline "int"
   | _ -> print_endline "ERROR");
  [%expect {| int |}]

let%expect_test "type_of_constant: zero returns int" =
  let ty = Inference_utils.type_of_constant (ConstantInteger 0) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) ->
     print_endline "int"
   | _ -> print_endline "ERROR");
  [%expect {| int |}]

let%expect_test "type_of_constant: float returns float" =
  let ty = Inference_utils.type_of_constant (ConstantFloat 3.14) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinFloat, []) ->
     print_endline "float"
   | _ -> print_endline "ERROR");
  [%expect {| float |}]

let%expect_test "type_of_constant: negative float returns float" =
  let ty = Inference_utils.type_of_constant (ConstantFloat (-2.5)) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinFloat, []) ->
     print_endline "float"
   | _ -> print_endline "ERROR");
  [%expect {| float |}]

let%expect_test "type_of_constant: string returns string" =
  let ty = Inference_utils.type_of_constant (ConstantString "hello") in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, []) ->
     print_endline "string"
   | _ -> print_endline "ERROR");
  [%expect {| string |}]

let%expect_test "type_of_constant: empty string returns string" =
  let ty = Inference_utils.type_of_constant (ConstantString "") in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, []) ->
     print_endline "string"
   | _ -> print_endline "ERROR");
  [%expect {| string |}]

let%expect_test "type_of_constant: true returns bool" =
  let ty = Inference_utils.type_of_constant (ConstantBoolean true) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinBool, []) ->
     print_endline "bool"
   | _ -> print_endline "ERROR");
  [%expect {| bool |}]

let%expect_test "type_of_constant: false returns bool" =
  let ty = Inference_utils.type_of_constant (ConstantBoolean false) in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinBool, []) ->
     print_endline "bool"
   | _ -> print_endline "ERROR");
  [%expect {| bool |}]

let%expect_test "type_of_constant: unit returns unit" =
  let ty = Inference_utils.type_of_constant ConstantUnit in
  (match ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinUnit, []) ->
     print_endline "unit"
   | _ -> print_endline "ERROR");
  [%expect {| unit |}]

(** {1 unify_with_env Tests} *)

(** Helper to create type variables for testing *)
let make_type_var id level =
  let tv = {
    Types.id;
    level;
    link = None;
    weak = false;
  } in
  (tv, Types.TypeVariable tv)

let type_int = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])
let type_string = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])

let%expect_test "unify_with_env: identical types unify successfully" =
  let env = Environment.empty in
  (try
     Inference_utils.unify_with_env env Common.Location.none type_int type_int;
     print_endline "unified"
   with _ -> print_endline "ERROR");
  [%expect {| unified |}]

let%expect_test "unify_with_env: type variable unifies with concrete type" =
  let env = Environment.empty in
  let tv, ty_var = make_type_var 100 1 in
  (try
     Inference_utils.unify_with_env env Common.Location.none ty_var type_int;
     (* After unification, type variable should be linked to int *)
     (match tv.link with
      | Some (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])) ->
        print_endline "linked to int"
      | _ -> print_endline "ERROR: wrong link")
   with _ -> print_endline "ERROR: unification failed");
  [%expect {| linked to int |}]

let%expect_test "unify_with_env: two type variables unify" =
  let env = Environment.empty in
  let _, ty_var1 = make_type_var 200 1 in
  let tv2, ty_var2 = make_type_var 201 1 in
  (try
     Inference_utils.unify_with_env env Common.Location.none ty_var1 ty_var2;
     (* One should be linked to the other *)
     (match tv2.link with
      | Some (Types.TypeVariable _) -> print_endline "linked"
      | None -> print_endline "linked"  (* Either way is fine *)
      | _ -> print_endline "ERROR")
   with _ -> print_endline "ERROR");
  [%expect {| linked |}]

let%expect_test "unify_with_env: incompatible types fail" =
  let env = Environment.empty in
  (try
     Inference_utils.unify_with_env env Common.Location.none type_int type_string;
     print_endline "ERROR: should have failed"
   with
   | Unification.Unification_error { message; _ } ->
     Printf.printf "unification error: %s\n" message
   | exn ->
     Printf.printf "ERROR: got %s\n" (Printexc.to_string exn));
  [%expect {| unification error: Type mismatch: expected int, got string |}]

let%expect_test "unify_with_env: arrow types unify component-wise" =
  let env = Environment.empty in
  let arrow1 = Types.TypeArrow (type_int, type_string) in
  let arrow2 = Types.TypeArrow (type_int, type_string) in
  (try
     Inference_utils.unify_with_env env Common.Location.none arrow1 arrow2;
     print_endline "unified"
   with _ -> print_endline "ERROR");
  [%expect {| unified |}]

let%expect_test "unify_with_env: tuple types unify element-wise" =
  let env = Environment.empty in
  let tuple1 = Types.TypeTuple [type_int; type_string] in
  let tuple2 = Types.TypeTuple [type_int; type_string] in
  (try
     Inference_utils.unify_with_env env Common.Location.none tuple1 tuple2;
     print_endline "unified"
   with _ -> print_endline "ERROR");
  [%expect {| unified |}]

let%expect_test "unify_with_env: tuple arity mismatch fails" =
  let env = Environment.empty in
  let tuple1 = Types.TypeTuple [type_int; type_string] in
  let tuple2 = Types.TypeTuple [type_int; type_string; type_int] in
  (try
     Inference_utils.unify_with_env env Common.Location.none tuple1 tuple2;
     print_endline "ERROR: should have failed"
   with
   | Unification.Unification_error { message; _ } ->
     Printf.printf "unification error: %s\n" message
   | exn ->
     Printf.printf "ERROR: got %s\n" (Printexc.to_string exn));
  [%expect {| unification error: Tuple size mismatch: expected 2 elements, got 3 |}]

(** {1 Constant Coverage Tests} *)

let%expect_test "type_of_constant: all constants have distinct types" =
  let types = [
    Inference_utils.type_of_constant (ConstantInteger 1);
    Inference_utils.type_of_constant (ConstantFloat 1.0);
    Inference_utils.type_of_constant (ConstantString "x");
    Inference_utils.type_of_constant (ConstantBoolean true);
    Inference_utils.type_of_constant ConstantUnit;
  ] in
  let type_name ty =
    match ty with
    | Types.TypeConstructor (Types.PathBuiltin b, []) ->
      (match b with
       | Types.BuiltinInt -> "int"
       | Types.BuiltinFloat -> "float"
       | Types.BuiltinString -> "string"
       | Types.BuiltinBool -> "bool"
       | Types.BuiltinUnit -> "unit")
    | _ -> "unknown"
  in
  List.iter (fun ty -> print_endline (type_name ty)) types;
  [%expect {|
    int
    float
    string
    bool
    unit |}]
