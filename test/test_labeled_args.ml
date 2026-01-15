(** Tests for labeled argument handling and type-directed reordering *)

open Typing
open Types

(* Helper to create a simple type variable for testing *)
let make_type ty_name =
  TypeConstructor (Types.PathBuiltin ty_name, [])

let int_ty = make_type "int"
let string_ty = make_type "string"
let bool_ty = make_type "bool"

(* Dummy expression for testing *)
let dummy_expr ty : Typed_tree.typed_expression = {
  expression_desc = TypedExpressionConstant (Parsing.Syntax_tree.ConstantInt 0);
  expression_type = ty;
  expression_location = Common.Location.none;
}

(* ==================== reorder_arguments tests ==================== *)

let%expect_test "reorder_arguments: all unlabeled args in order" =
  let params = [
    (Nolabel, int_ty);
    (Nolabel, string_ty);
    (Nolabel, bool_ty);
  ] in
  let args = [
    (Nolabel, dummy_expr int_ty);
    (Nolabel, dummy_expr string_ty);
    (Nolabel, dummy_expr bool_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 3
    Unused: 0
      Nolabel: Filled
      Nolabel: Filled
      Nolabel: Filled
    |}]

let%expect_test "reorder_arguments: labeled args in order" =
  let params = [
    (Labelled "x", int_ty);
    (Labelled "y", string_ty);
  ] in
  let args = [
    (Labelled "x", dummy_expr int_ty);
    (Labelled "y", dummy_expr string_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 2
    Unused: 0
      ~x: Filled
      ~y: Filled
    |}]

let%expect_test "reorder_arguments: labeled args out of order" =
  let params = [
    (Labelled "x", int_ty);
    (Labelled "y", string_ty);
  ] in
  let args = [
    (Labelled "y", dummy_expr string_ty);
    (Labelled "x", dummy_expr int_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 2
    Unused: 0
      ~x: Filled
      ~y: Filled
    |}]

let%expect_test "reorder_arguments: mixed labeled and unlabeled" =
  let params = [
    (Labelled "a", int_ty);
    (Nolabel, string_ty);
    (Labelled "c", bool_ty);
  ] in
  let args = [
    (Labelled "c", dummy_expr bool_ty);
    (Labelled "a", dummy_expr int_ty);
    (Nolabel, dummy_expr string_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 3
    Unused: 0
      ~a: Filled
      Nolabel: Filled
      ~c: Filled
    |}]

let%expect_test "reorder_arguments: partial application - missing labeled arg" =
  let params = [
    (Labelled "x", int_ty);
    (Labelled "y", string_ty);
    (Labelled "z", bool_ty);
  ] in
  let args = [
    (Labelled "z", dummy_expr bool_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 3
    Unused: 0
      ~x: Needed
      ~y: Needed
      ~z: Filled
    |}]

let%expect_test "reorder_arguments: partial application - unlabeled partial" =
  let params = [
    (Nolabel, int_ty);
    (Nolabel, string_ty);
    (Nolabel, bool_ty);
  ] in
  let args = [
    (Nolabel, dummy_expr int_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 3
    Unused: 0
      Nolabel: Filled
      Nolabel: Needed
      Nolabel: Needed
    |}]

let%expect_test "reorder_arguments: unused labeled arg" =
  let params = [
    (Labelled "x", int_ty);
  ] in
  let args = [
    (Labelled "x", dummy_expr int_ty);
    (Labelled "y", dummy_expr string_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, _) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    Printf.printf "  Unused: %s\n" label_str
  ) unused;
  [%expect{|
    Slots: 1
    Unused: 1
      Unused: ~y
    |}]

let%expect_test "reorder_arguments: labelled fills optional" =
  let params = [
    (Optional "name", string_ty);
    (Nolabel, int_ty);
  ] in
  let args = [
    (Labelled "name", dummy_expr string_ty);
    (Nolabel, dummy_expr int_ty);
  ] in
  let slots, unused = Expression_infer.reorder_arguments params args in
  Printf.printf "Slots: %d\n" (List.length slots);
  Printf.printf "Unused: %d\n" (List.length unused);
  List.iter (fun (label, slot) ->
    let label_str = match label with
      | Nolabel -> "Nolabel"
      | Labelled s -> "~" ^ s
      | Optional s -> "?" ^ s
    in
    let slot_str = match slot with
      | `Filled _ -> "Filled"
      | `Needed _ -> "Needed"
    in
    Printf.printf "  %s: %s\n" label_str slot_str
  ) slots;
  [%expect{|
    Slots: 2
    Unused: 0
      ?name: Filled
      Nolabel: Filled
    |}]
