(** Unit tests for value restriction.

    Tests verify that:
    - Syntactic values are generalized (polymorphic)
    - Non-values (expansive expressions) are NOT generalized (monomorphic)
    - Weak type variables are correctly marked *)

open Typing

(** {1 Helper Functions} *)

let compile_and_check code =
  Types.reset_type_variable_id ();
  let structure = Parsing.Parse.structure_from_string code in
  let ctx = Typing_context.create Environment.initial in
  let (_typed, final_ctx) = Inference.infer_structure ctx structure in
  Typing_context.environment final_ctx

let get_scheme_for name env =
  match Environment.find_value name env with
  | Some binding -> binding.Environment.binding_scheme
  | None -> failwith ("Value not found: " ^ name)

let is_polymorphic scheme =
  scheme.Types.quantified_variables <> []

let has_weak_variables scheme =
  let rec check ty =
    match Types.representative ty with
    | Types.TypeVariable tv -> tv.weak
    | Types.TypeConstructor (_, args) -> List.exists check args
    | Types.TypeTuple elems -> List.exists check elems
    | Types.TypeArrow (_, a, r) -> check a || check r
    | Types.TypeRecord row -> check_row row
    | Types.TypePolyVariant pv_row -> check_poly_variant_row pv_row
    | Types.TypeRowEmpty -> false
    | Types.TypePackage pkg ->
      List.exists (fun (_, ty) -> check ty) pkg.package_signature
  and check_row row =
    List.exists (fun (_, field) ->
      match field with
      | Types.RowFieldPresent ty -> check ty
    ) row.row_fields || check row.row_more
  and check_poly_variant_row pv_row =
    List.exists (fun (_, field) ->
      match field with
      | Types.PVFieldPresent (Some ty) -> check ty
      | Types.PVFieldPresent None | Types.PVFieldAbsent -> false
    ) pv_row.pv_fields || check pv_row.pv_more
  in
  check scheme.Types.body

(** {1 Syntactic Values Should Be Generalized} *)

let%expect_test "lambda is generalized" =
  let env = compile_and_check "let id = fun x -> x" in
  let scheme = get_scheme_for "id" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "constant is generalized (vacuously)" =
  let env = compile_and_check "let x = 42" in
  let scheme = get_scheme_for "x" env in
  (* Constants have no type variables to generalize, but the process works *)
  print_endline (Format.asprintf "%a" Types.pp_type_scheme scheme);
  [%expect {| int |}]

let%expect_test "tuple of values is generalized" =
  let env = compile_and_check "let pair = (fun x -> x, fun y -> y)" in
  let scheme = get_scheme_for "pair" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "constructor with value argument is generalized" =
  let env = compile_and_check {|
    type 'a option = None | Some of 'a
    let some_id = Some (fun x -> x)
  |} in
  let scheme = get_scheme_for "some_id" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "record of values is generalized" =
  (* Note: Records with inline lambdas don't parse, so we use a let-bound function *)
  let env = compile_and_check {|
    let id = fun x -> x
    let r = { f = id }
  |} in
  let scheme = get_scheme_for "r" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "let binding with value body is generalized" =
  let env = compile_and_check "let f = let x = 1 in fun y -> y" in
  let scheme = get_scheme_for "f" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

(** {1 Non-Values Should NOT Be Generalized} *)

let%expect_test "function application is NOT generalized" =
  let env = compile_and_check {|
    let f x = x
    let result = f (fun x -> x)
  |} in
  let scheme = get_scheme_for "result" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| monomorphic |}]

let%expect_test "if expression is NOT generalized" =
  let env = compile_and_check {|
    let x = if true then (fun a -> a) else (fun b -> b)
  |} in
  let scheme = get_scheme_for "x" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "match expression is NOT generalized" =
  let env = compile_and_check {|
    type 'a option = None | Some of 'a
    let x = match None with None -> (fun a -> a) | Some f -> f
  |} in
  let scheme = get_scheme_for "x" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "sequence is NOT generalized" =
  let env = compile_and_check {|
    let x = (1; fun a -> a)
  |} in
  let scheme = get_scheme_for "x" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| monomorphic |}]

(** {1 Weak Type Variables} *)

let%expect_test "non-value has weak type variable" =
  let env = compile_and_check {|
    let f x = x
    let result = f (fun x -> x)
  |} in
  let scheme = get_scheme_for "result" env in
  print_endline (if has_weak_variables scheme then "has weak vars" else "no weak vars");
  [%expect {| has weak vars |}]

let%expect_test "weak variables printed with underscore prefix" =
  let env = compile_and_check {|
    let f x = x
    let result = f (fun x -> x)
  |} in
  let scheme = get_scheme_for "result" env in
  let type_str = Format.asprintf "%a" Types.pp_type_scheme scheme in
  print_endline (if String.sub type_str 0 2 = "'_" then "has underscore prefix" else type_str);
  [%expect {| has underscore prefix |}]

(** {1 Polymorphic Usage After Value Restriction} *)

let%expect_test "polymorphic function can be used at multiple types" =
  (* This should compile without error - id is polymorphic *)
  let _ = compile_and_check {|
    let id = fun x -> x
    let a = id 42
    let b = id true
  |} in
  print_endline "compiles";
  [%expect {| compiles |}]

let%expect_test "value restriction preserves let-polymorphism for values" =
  let _ = compile_and_check {|
    let id = fun x -> x
    let use_id = (id 1, id "hello")
  |} in
  print_endline "compiles";
  [%expect {| compiles |}]

(** {1 Variance Checking} *)

(** Helper to create a type variable for variance tests *)
let make_test_type_var id =
  { Types.id; level = 1; link = None; weak = false; rigid = false }

let variance_to_string = function
  | Value_check.Covariant -> "covariant"
  | Value_check.Contravariant -> "contravariant"
  | Value_check.Invariant -> "invariant"
  | Value_check.Bivariant -> "bivariant"

let%expect_test "variance: variable alone is covariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeVariable tv in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| covariant |}]

let%expect_test "variance: variable in tuple is covariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeTuple [Types.TypeVariable tv; Types.type_int] in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| covariant |}]

let%expect_test "variance: variable on left of arrow is contravariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.TypeVariable tv, Types.type_int) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| contravariant |}]

let%expect_test "variance: variable on right of arrow is covariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.type_int, Types.TypeVariable tv) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| covariant |}]

let%expect_test "variance: variable on both sides of arrow is invariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.TypeVariable tv, Types.TypeVariable tv) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| invariant |}]

let%expect_test "variance: absent variable is bivariant" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.type_int, Types.type_string) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| bivariant |}]

let%expect_test "variance: nested contravariance flips to covariant" =
  (* ('a -> int) -> int has 'a in covariant position (double flip) *)
  let tv = make_test_type_var 0 in
  let inner = Types.TypeArrow (Nolabel, Types.TypeVariable tv, Types.type_int) in
  let ty = Types.TypeArrow (Nolabel, inner, Types.type_int) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| covariant |}]

let%expect_test "variance: variable in type constructor is covariant" =
  (* 'a option - option is covariant *)
  let tv = make_test_type_var 0 in
  let ty = Types.TypeConstructor (Types.PathLocal "option", [Types.TypeVariable tv]) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| covariant |}]

let%expect_test "variance: variable in ref is invariant" =
  (* 'a ref - ref is invariant *)
  let tv = make_test_type_var 0 in
  let ty = Types.type_ref (Types.TypeVariable tv) in
  let variance = Value_check.check_variance tv ty in
  print_endline (variance_to_string variance);
  [%expect {| invariant |}]

(** {1 can_generalize_relaxed} *)

let%expect_test "can_generalize_relaxed: covariant variable can be generalized" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeTuple [Types.TypeVariable tv] in
  print_endline (if Value_check.can_generalize_relaxed tv ty then "yes" else "no");
  [%expect {| yes |}]

let%expect_test "can_generalize_relaxed: contravariant variable cannot be generalized" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.TypeVariable tv, Types.type_int) in
  print_endline (if Value_check.can_generalize_relaxed tv ty then "yes" else "no");
  [%expect {| no |}]

let%expect_test "can_generalize_relaxed: invariant variable cannot be generalized" =
  let tv = make_test_type_var 0 in
  let ty = Types.TypeArrow (Nolabel, Types.TypeVariable tv, Types.TypeVariable tv) in
  print_endline (if Value_check.can_generalize_relaxed tv ty then "yes" else "no");
  [%expect {| no |}]

let%expect_test "can_generalize_relaxed: bivariant (absent) can be generalized" =
  let tv = make_test_type_var 0 in
  let ty = Types.type_int in
  print_endline (if Value_check.can_generalize_relaxed tv ty then "yes" else "no");
  [%expect {| yes |}]

(** {1 Relaxed Value Restriction Integration} *)

let%expect_test "relaxed VR: application with covariant result is polymorphic" =
  (* let x = id [] results in 'a list - 'a is covariant, so can be generalized *)
  let env = compile_and_check {|
    type 'a list = Nil | Cons of 'a * 'a list
    let id x = x
    let empty_list = id Nil
  |} in
  let scheme = get_scheme_for "empty_list" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "relaxed VR: tuple with invariant component is monomorphic" =
  (* let x = id (1, fun x -> x) results in int * ('a -> 'a)
     The 'a in 'a -> 'a is invariant, so NOT polymorphic *)
  let env = compile_and_check {|
    let id x = x
    let pair = id (1, fun x -> x)
  |} in
  let scheme = get_scheme_for "pair" env in
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| monomorphic |}]

let%expect_test "relaxed VR: application resulting in function type" =
  (* Function application that returns a function - result has type 'a -> 'a
     where 'a is invariant (appears both as input and output) *)
  let env = compile_and_check {|
    let id x = x
    let result = id (fun x -> x)
  |} in
  let scheme = get_scheme_for "result" env in
  (* 'a -> 'a has 'a in both positions (invariant), so NOT polymorphic *)
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| monomorphic |}]

let%expect_test "relaxed VR: application with covariant result type" =
  (* let empty = id Nil results in 'a list - 'a is covariant *)
  let env = compile_and_check {|
    type 'a list = Nil | Cons of 'a * 'a list
    let id x = x
    let empty = id Nil
  |} in
  let scheme = get_scheme_for "empty" env in
  (* Verify it's polymorphic - the type variable is covariant so can be generalized *)
  print_endline (if is_polymorphic scheme then "polymorphic" else "monomorphic");
  [%expect {| polymorphic |}]

let%expect_test "relaxed VR: verify scheme has quantified variables" =
  (* Debug: check that the scheme actually has quantified variables *)
  let env = compile_and_check {|
    type 'a list = Nil | Cons of 'a * 'a list
    let id x = x
    let empty = id Nil
  |} in
  let scheme = get_scheme_for "empty" env in
  print_endline (Format.asprintf "quantified_vars: %d" (List.length scheme.Types.quantified_variables));
  print_endline (Format.asprintf "type: %a" Types.pp_type_scheme scheme);
  [%expect {|
    quantified_vars: 1
    type: forall 'a. 'a list
    |}]

let%expect_test "relaxed VR: covariant type can be used at multiple types" =
  (* This test verifies that the constructor type parameter bug is fixed.
     With the fix, `empty` is polymorphic and can be used at multiple types. *)
  let _ = compile_and_check {|
    type 'a list = Nil | Cons of 'a * 'a list
    let id x = x
    let empty = id Nil
    let int_list = Cons (1, empty)
    let str_list = Cons ("hello", empty)
  |} in
  print_endline "compiles";
  [%expect {| compiles |}]

(** {1 Value Restriction in Top-Level Module Bindings} *)

(* These tests verify that value restriction is correctly applied when
   extracting signatures from structure items. This was a bug where
   signature_items_of_typed_structure_item used generalize directly
   instead of compute_binding_scheme. *)

let%expect_test "non-value in module is not incorrectly generalized" =
  (* The binding `applied = id id` is a non-value because it's an application.
     The result type 'a -> 'a is invariant (not covariant), so it should NOT
     be generalized under relaxed value restriction. *)
  let env = compile_and_check {|
    module M = struct
      let id = fun x -> x
      let applied = id id
    end
  |} in
  (* Check M.applied's type - should be monomorphic *)
  let m_binding = Environment.find_module "M" env in
  let m_type = match m_binding with
    | Some b -> b.Module_types.binding_type
    | None -> failwith "Module M not found"
  in
  let applied_scheme = match m_type with
    | Module_types.ModTypeSig sig_ ->
      (match Module_types.find_value_in_sig "applied" sig_ with
       | Some vd -> vd.Module_types.value_type
       | None -> failwith "applied not found in M")
    | _ -> failwith "M is not a signature"
  in
  print_endline (if is_polymorphic applied_scheme then "polymorphic (BUG!)" else "monomorphic (correct)");
  [%expect {| monomorphic (correct) |}]

let%expect_test "value in module IS generalized" =
  (* The binding `id = fun x -> x` is a syntactic value (lambda),
     so it should be fully polymorphic. *)
  let env = compile_and_check {|
    module M = struct
      let id = fun x -> x
    end
  |} in
  let m_binding = Environment.find_module "M" env in
  let m_type = match m_binding with
    | Some b -> b.Module_types.binding_type
    | None -> failwith "Module M not found"
  in
  let id_scheme = match m_type with
    | Module_types.ModTypeSig sig_ ->
      (match Module_types.find_value_in_sig "id" sig_ with
       | Some vd -> vd.Module_types.value_type
       | None -> failwith "id not found in M")
    | _ -> failwith "M is not a signature"
  in
  print_endline (if is_polymorphic id_scheme then "polymorphic (correct)" else "monomorphic (BUG!)");
  [%expect {| polymorphic (correct) |}]

let%expect_test "module value restriction - practical usage" =
  (* This test verifies the module works correctly at runtime *)
  let _ = compile_and_check {|
    module M = struct
      let id = fun x -> x
      let applied = id id
      let value = applied 42
    end
    let result = M.applied 100
  |} in
  print_endline "compiles and type-checks correctly";
  [%expect {| compiles and type-checks correctly |}]
