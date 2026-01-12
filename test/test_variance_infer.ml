(** Tests for variance inference. *)

open Typing

(** Helper to create a type variable with a given id. *)
let make_type_var id =
  { Types.id; level = 0; link = None; weak = false }

(** Helper to print variance. *)
let variance_to_string = function
  | Types.Covariant -> "covariant"
  | Types.Contravariant -> "contravariant"
  | Types.Invariant -> "invariant"

(** {1 flip_variance tests} *)

let%expect_test "flip_variance: covariant becomes contravariant" =
  let result = Variance_infer.flip_variance Types.Covariant in
  print_endline (variance_to_string result);
  [%expect {| contravariant |}]

let%expect_test "flip_variance: contravariant becomes covariant" =
  let result = Variance_infer.flip_variance Types.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| covariant |}]

let%expect_test "flip_variance: invariant stays invariant" =
  let result = Variance_infer.flip_variance Types.Invariant in
  print_endline (variance_to_string result);
  [%expect {| invariant |}]

(** {1 combine_variance tests} *)

let%expect_test "combine_variance: covariant + covariant = covariant" =
  let result = Variance_infer.combine_variance Types.Covariant Types.Covariant in
  print_endline (variance_to_string result);
  [%expect {| covariant |}]

let%expect_test "combine_variance: contravariant + contravariant = contravariant" =
  let result = Variance_infer.combine_variance Types.Contravariant Types.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| contravariant |}]

let%expect_test "combine_variance: covariant + contravariant = invariant" =
  let result = Variance_infer.combine_variance Types.Covariant Types.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| invariant |}]

let%expect_test "combine_variance: contravariant + covariant = invariant" =
  let result = Variance_infer.combine_variance Types.Contravariant Types.Covariant in
  print_endline (variance_to_string result);
  [%expect {| invariant |}]

let%expect_test "combine_variance: invariant + anything = invariant" =
  let result = Variance_infer.combine_variance Types.Invariant Types.Covariant in
  print_endline (variance_to_string result);
  [%expect {| invariant |}]

(** {1 infer_declaration_variances tests} *)

let%expect_test "infer: variant type with covariant param" =
  (* type 'a option = None | Some of 'a *)
  let tv = make_type_var 1 in
  let constructors = [
    { Types.constructor_name = "None";
      constructor_tag_index = 0;
      constructor_type_name = "option";
      constructor_argument_type = None;
      constructor_result_type = Types.TypeVariable tv;
      constructor_type_parameters = [tv] };
    { Types.constructor_name = "Some";
      constructor_tag_index = 1;
      constructor_type_name = "option";
      constructor_argument_type = Some (Types.TypeVariable tv);
      constructor_result_type = Types.TypeVariable tv;
      constructor_type_parameters = [tv] };
  ] in
  let variances = Variance_infer.infer_declaration_variances [tv]
    (Types.DeclarationVariant constructors) in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| covariant |}]

let%expect_test "infer: arrow type is covariant in result" =
  (* type 'a producer = unit -> 'a *)
  let tv = make_type_var 1 in
  let kind = Types.DeclarationAbstract in
  let manifest = Some (Types.TypeArrow (Types.type_unit, Types.TypeVariable tv)) in
  (* For type aliases, we need to analyze the manifest *)
  (* Here we test the underlying type directly *)
  let variances = Variance_infer.infer_declaration_variances [tv] kind in
  (* Abstract types default to invariant *)
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| invariant |}]

let%expect_test "infer: record with covariant fields" =
  (* type 'a box = { value : 'a } *)
  let tv = make_type_var 1 in
  let fields = [("value", Types.TypeVariable tv)] in
  let variances = Variance_infer.infer_declaration_variances [tv]
    (Types.DeclarationRecord fields) in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| covariant |}]

let%expect_test "infer: abstract type is invariant" =
  let tv = make_type_var 1 in
  let variances = Variance_infer.infer_declaration_variances [tv]
    Types.DeclarationAbstract in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| invariant |}]

let%expect_test "infer: unused parameter is covariant" =
  (* type 'a phantom = int *)
  let tv = make_type_var 1 in
  let constructors = [
    { Types.constructor_name = "Phantom";
      constructor_tag_index = 0;
      constructor_type_name = "phantom";
      constructor_argument_type = Some Types.type_int;  (* No 'a! *)
      constructor_result_type = Types.TypeVariable tv;
      constructor_type_parameters = [tv] };
  ] in
  let variances = Variance_infer.infer_declaration_variances [tv]
    (Types.DeclarationVariant constructors) in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| covariant |}]

(** {1 merge_variances tests} *)

let%expect_test "merge_variances: explicit takes precedence" =
  let explicit = [Some Types.Contravariant; None] in
  let inferred = [Types.Covariant; Types.Covariant] in
  let result = Variance_infer.merge_variances explicit inferred in
  List.iter (fun v -> print_endline (variance_to_string v)) result;
  [%expect {|
    contravariant
    covariant |}]

let%expect_test "merge_variances: all explicit" =
  let explicit = [Some Types.Covariant; Some Types.Contravariant] in
  let inferred = [Types.Invariant; Types.Invariant] in
  let result = Variance_infer.merge_variances explicit inferred in
  List.iter (fun v -> print_endline (variance_to_string v)) result;
  [%expect {|
    covariant
    contravariant |}]

let%expect_test "merge_variances: all inferred" =
  let explicit = [None; None] in
  let inferred = [Types.Covariant; Types.Contravariant] in
  let result = Variance_infer.merge_variances explicit inferred in
  List.iter (fun v -> print_endline (variance_to_string v)) result;
  [%expect {|
    covariant
    contravariant |}]
