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
  | Types.Bivariant -> "bivariant"

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

let%expect_test "infer: abstract type defaults to invariant" =
  (* type 'a producer - abstract types default to invariant (safest) *)
  let tv = make_type_var 1 in
  let kind = Types.DeclarationAbstract in
  (* Note: Abstract types with manifests would be processed separately
     during type declaration processing, not by infer_declaration_variances *)
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
  [%expect {| bivariant |}]

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

(** {1 Bivariant tests} *)

let%expect_test "flip_variance: bivariant stays bivariant" =
  let result = Variance_infer.flip_variance Types.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| bivariant |}]

let%expect_test "combine_variance: bivariant + covariant = covariant" =
  let result = Variance_infer.combine_variance Types.Bivariant Types.Covariant in
  print_endline (variance_to_string result);
  [%expect {| covariant |}]

let%expect_test "combine_variance: covariant + bivariant = covariant" =
  let result = Variance_infer.combine_variance Types.Covariant Types.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| covariant |}]

let%expect_test "combine_variance: bivariant + bivariant = bivariant" =
  let result = Variance_infer.combine_variance Types.Bivariant Types.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| bivariant |}]

let%expect_test "combine_variance: bivariant + invariant = invariant" =
  let result = Variance_infer.combine_variance Types.Bivariant Types.Invariant in
  print_endline (variance_to_string result);
  [%expect {| invariant |}]

let%expect_test "infer: unused parameter is bivariant" =
  (* type 'a phantom = int - 'a doesn't appear in the definition *)
  let tv = make_type_var 1 in
  let constructors = [
    { Types.constructor_name = "Phantom";
      constructor_tag_index = 0;
      constructor_type_name = "phantom";
      constructor_argument_type = Some Types.type_int;  (* No 'a! *)
      constructor_result_type = Types.TypeConstructor (Types.PathLocal "phantom", [Types.TypeVariable tv]);
      constructor_type_parameters = [tv] };
  ] in
  let variances = Variance_infer.infer_declaration_variances [tv]
    (Types.DeclarationVariant constructors) in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| bivariant |}]

let%expect_test "infer: record without param is bivariant" =
  (* type 'a phantom_record = { value : int } *)
  let tv = make_type_var 1 in
  let fields = [("value", Types.type_int)] in
  let variances = Variance_infer.infer_declaration_variances [tv]
    (Types.DeclarationRecord fields) in
  List.iter (fun v -> print_endline (variance_to_string v)) variances;
  [%expect {| bivariant |}]

(** {1 Validation tests} *)

let%expect_test "validate: matching variances are compatible" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Covariant]
    ~inferred:[Types.Covariant]
  in
  print_endline (match result with Ok () -> "ok" | Error _ -> "error");
  [%expect {| ok |}]

let%expect_test "validate: covariant annotation on contravariant use is error" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Covariant]
    ~inferred:[Types.Contravariant]
  in
  begin match result with
  | Ok () -> print_endline "ok"
  | Error err -> print_endline (Variance_infer.format_variance_error err)
  end;
  [%expect {| Type parameter ''a is declared as covariant (+) but is used as contravariant (input position only) in the definition |}]

let%expect_test "validate: contravariant annotation on covariant use is error" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Contravariant]
    ~inferred:[Types.Covariant]
  in
  begin match result with
  | Ok () -> print_endline "ok"
  | Error err -> print_endline (Variance_infer.format_variance_error err)
  end;
  [%expect {| Type parameter ''a is declared as contravariant (-) but is used as covariant (output position only) in the definition |}]

let%expect_test "validate: invariant annotation is always compatible" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Invariant]
    ~inferred:[Types.Covariant]
  in
  print_endline (match result with Ok () -> "ok" | Error _ -> "error");
  [%expect {| ok |}]

let%expect_test "validate: no annotation is always compatible" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[None]
    ~inferred:[Types.Invariant]
  in
  print_endline (match result with Ok () -> "ok" | Error _ -> "error");
  [%expect {| ok |}]

let%expect_test "validate: bivariant inferred accepts any annotation" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Covariant]
    ~inferred:[Types.Bivariant]
  in
  print_endline (match result with Ok () -> "ok" | Error _ -> "error");
  [%expect {| ok |}]

let%expect_test "validate: bivariant annotation on covariant is compatible" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Bivariant]
    ~inferred:[Types.Covariant]
  in
  print_endline (match result with Ok () -> "ok" | Error _ -> "error");
  [%expect {| ok |}]

let%expect_test "validate: bivariant annotation on invariant is error" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"]
    ~explicit:[Some Types.Bivariant]
    ~inferred:[Types.Invariant]
  in
  begin match result with
  | Ok () -> print_endline "ok"
  | Error err -> print_endline (Variance_infer.format_variance_error err)
  end;
  [%expect {| Type parameter ''a is declared as bivariant (_) but is used as invariant (both input and output positions) in the definition |}]

let%expect_test "validate: multiple parameters" =
  let result = Variance_infer.validate_annotations
    ~param_names:["'a"; "'b"]
    ~explicit:[Some Types.Covariant; Some Types.Contravariant]
    ~inferred:[Types.Covariant; Types.Covariant]  (* 'b is actually covariant *)
  in
  begin match result with
  | Ok () -> print_endline "ok"
  | Error err -> print_endline (Variance_infer.format_variance_error err)
  end;
  [%expect {| Type parameter ''b is declared as contravariant (-) but is used as covariant (output position only) in the definition |}]
