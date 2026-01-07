(** Unit tests for Type_utils.map_row_types function.

    Tests cover:
    - Shallow mapping over row fields
    - Row tail transformation
    - Field name preservation *)

open Typing

(** Helper to create a type variable for testing *)
let make_type_var id level =
  let tv = {
    Types.id;
    level;
    link = None;
  } in
  (tv, Types.TypeVariable tv)

(** Helper type constructors *)
let type_int = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])
let type_string = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])
let type_bool = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinBool, [])

(** {1 Basic Mapping Tests} *)

let%expect_test "map_row_types: maps over empty row" =
  let row = {
    Types.row_fields = [];
    row_more = Types.TypeRowEmpty;
  } in
  let count = ref 0 in
  let _ = Type_utils.map_row_types (fun ty -> incr count; ty) row in
  (* Only row_more is visited *)
  print_int !count;
  [%expect {| 1 |}]

let%expect_test "map_row_types: maps over single field" =
  let _, ty_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [("x", Types.RowFieldPresent ty_var)];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun _ -> type_int) row in
  (match result.row_fields with
   | [("x", Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])))] ->
     print_endline "x: int"
   | _ -> print_endline "ERROR");
  [%expect {| x: int |}]

let%expect_test "map_row_types: maps over multiple fields" =
  let tv1, ty_var1 = make_type_var 0 1 in
  let tv2, ty_var2 = make_type_var 1 1 in
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent ty_var1);
      ("y", Types.RowFieldPresent ty_var2);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun ty ->
    match ty with
    | Types.TypeVariable v when v.id = tv1.id -> type_int
    | Types.TypeVariable v when v.id = tv2.id -> type_string
    | _ -> ty
  ) row in
  (match result.row_fields with
   | [("x", Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])));
      ("y", Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])))] ->
     print_endline "x: int, y: string"
   | _ -> print_endline "ERROR");
  [%expect {| x: int, y: string |}]

(** {1 Row Tail Tests} *)

let%expect_test "map_row_types: transforms row_more correctly" =
  let _, row_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [("x", Types.RowFieldPresent type_int)];
    row_more = row_var;
  } in
  let result = Type_utils.map_row_types (fun ty ->
    match ty with
    | Types.TypeVariable _ -> Types.TypeRowEmpty
    | _ -> ty
  ) row in
  (match result.row_more with
   | Types.TypeRowEmpty -> print_endline "closed row"
   | _ -> print_endline "ERROR");
  [%expect {| closed row |}]

let%expect_test "map_row_types: preserves TypeRowEmpty in row_more" =
  let row = {
    Types.row_fields = [("x", Types.RowFieldPresent type_int)];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun ty -> ty) row in
  (match result.row_more with
   | Types.TypeRowEmpty -> print_endline "still empty"
   | _ -> print_endline "ERROR");
  [%expect {| still empty |}]

(** {1 Field Name Preservation Tests} *)

let%expect_test "map_row_types: preserves field names" =
  let row = {
    Types.row_fields = [
      ("alpha", Types.RowFieldPresent type_int);
      ("beta", Types.RowFieldPresent type_string);
      ("gamma", Types.RowFieldPresent type_bool);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun _ -> type_int) row in
  let names = List.map fst result.row_fields in
  List.iter print_endline names;
  [%expect {|
    alpha
    beta
    gamma |}]

let%expect_test "map_row_types: preserves field order" =
  let row = {
    Types.row_fields = [
      ("c", Types.RowFieldPresent type_int);
      ("a", Types.RowFieldPresent type_string);
      ("b", Types.RowFieldPresent type_bool);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun ty -> ty) row in
  let names = List.map fst result.row_fields in
  print_endline (String.concat "," names);
  [%expect {| c,a,b |}]

(** {1 Shallow vs Deep Mapping Tests} *)

let%expect_test "map_row_types: is shallow (doesn't recurse into nested types)" =
  (* Field type is int -> string, mapping should only touch the arrow itself *)
  let arrow_ty = Types.TypeArrow (type_int, type_string) in
  let row = {
    Types.row_fields = [("f", Types.RowFieldPresent arrow_ty)];
    row_more = Types.TypeRowEmpty;
  } in
  let visit_count = ref 0 in
  let _ = Type_utils.map_row_types (fun ty ->
    incr visit_count;
    ty
  ) row in
  (* Should visit only the arrow type (1) and row_more (1), not int/string inside arrow *)
  print_int !visit_count;
  [%expect {| 2 |}]

let%expect_test "map_row_types: identity function preserves structure" =
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent type_int);
      ("y", Types.RowFieldPresent (Types.TypeArrow (type_int, type_string)));
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun ty -> ty) row in
  (* Check structure is preserved *)
  let check = match result.row_fields with
    | [("x", Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])));
       ("y", Types.RowFieldPresent (Types.TypeArrow (
          Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []),
          Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, []))))] ->
      true
    | _ -> false
  in
  print_endline (if check then "preserved" else "ERROR");
  [%expect {| preserved |}]

(** {1 Transform All Fields Tests} *)

let%expect_test "map_row_types: can transform all fields to same type" =
  let _, tv1 = make_type_var 0 1 in
  let _, tv2 = make_type_var 1 1 in
  let _, tv3 = make_type_var 2 1 in
  let row = {
    Types.row_fields = [
      ("a", Types.RowFieldPresent tv1);
      ("b", Types.RowFieldPresent tv2);
      ("c", Types.RowFieldPresent tv3);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun _ -> type_string) row in
  let all_string = List.for_all (fun (_, field) ->
    match field with
    | Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])) -> true
    | _ -> false
  ) result.row_fields in
  print_endline (if all_string then "all string" else "ERROR");
  [%expect {| all string |}]

(** {1 Complex Row Tests} *)

let%expect_test "map_row_types: handles record in field" =
  let inner_row = {
    Types.row_fields = [("inner", Types.RowFieldPresent type_int)];
    row_more = Types.TypeRowEmpty;
  } in
  let inner_record = Types.TypeRecord inner_row in
  let row = {
    Types.row_fields = [("nested", Types.RowFieldPresent inner_record)];
    row_more = Types.TypeRowEmpty;
  } in
  (* Transform should touch outer record but not descend into inner *)
  let result = Type_utils.map_row_types (fun ty ->
    match ty with
    | Types.TypeRecord _ -> type_int  (* Replace any record with int *)
    | _ -> ty
  ) row in
  (match result.row_fields with
   | [("nested", Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])))] ->
     print_endline "nested replaced with int"
   | _ -> print_endline "ERROR");
  [%expect {| nested replaced with int |}]

let%expect_test "map_row_types: many fields" =
  let fields = List.init 10 (fun i ->
    (Printf.sprintf "field%d" i, Types.RowFieldPresent type_int)
  ) in
  let row = {
    Types.row_fields = fields;
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_utils.map_row_types (fun _ -> type_string) row in
  Printf.printf "field count: %d" (List.length result.row_fields);
  [%expect {| field count: 10 |}]
