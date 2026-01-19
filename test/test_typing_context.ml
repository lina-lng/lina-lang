(** Unit tests for Typing_context module.

    Tests cover:
    - Context creation and environment access
    - Level management (enter/leave)
    - Type variable creation at different levels
    - Generalization and instantiation *)

open Typing

(** {1 Context Creation Tests} *)

let%expect_test "create initializes with level 1" =
  let env = Environment.initial in
  let ctx = Typing_context.create env in
  print_int (Typing_context.current_level ctx);
  [%expect {| 1 |}]

let%expect_test "create preserves environment" =
  let env = Environment.initial in
  let ctx = Typing_context.create env in
  let retrieved_env = Typing_context.environment ctx in
  (* Environments should be the same object *)
  print_endline (if env == retrieved_env then "same" else "different");
  [%expect {| same |}]

let%expect_test "with_environment changes environment but preserves level" =
  let env1 = Environment.initial in
  let ctx = Typing_context.create env1 in
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.enter_level ctx in
  (* Level is now 3 *)
  let env2 = Environment.initial in
  let ctx' = Typing_context.with_environment env2 ctx in
  print_int (Typing_context.current_level ctx');
  [%expect {| 3 |}]

(** {1 Level Management Tests} *)

let%expect_test "enter_level increments level" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in
  print_int (Typing_context.current_level ctx);
  [%expect {| 2 |}]

let%expect_test "leave_level decrements level" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.leave_level ctx in
  print_int (Typing_context.current_level ctx);
  [%expect {| 2 |}]

let%expect_test "multiple enter/leave operations" =
  let ctx = Typing_context.create Environment.initial in
  (* Start at 1, enter 3 times = 4, leave 2 times = 2 *)
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.enter_level ctx in
  let ctx = Typing_context.leave_level ctx in
  let ctx = Typing_context.leave_level ctx in
  print_int (Typing_context.current_level ctx);
  [%expect {| 2 |}]

let%expect_test "leave_level at level 1 goes to 0" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.leave_level ctx in
  print_int (Typing_context.current_level ctx);
  [%expect {| 0 |}]

(** {1 Type Variable ID Generation Tests} *)

let%expect_test "fresh_type_variable_id returns unique IDs" =
  let ctx = Typing_context.create Environment.initial in
  let id1, ctx = Typing_context.fresh_type_variable_id ctx in
  let id2, ctx = Typing_context.fresh_type_variable_id ctx in
  let id3, _ = Typing_context.fresh_type_variable_id ctx in
  Printf.printf "id1=%d id2=%d id3=%d\n" id1 id2 id3;
  print_endline (if id1 <> id2 && id2 <> id3 && id1 <> id3 then "all unique" else "duplicates!");
  [%expect {|
    id1=35 id2=36 id3=37
    all unique
    |}]

let%expect_test "fresh_type_variable_id is monotonically increasing" =
  let ctx = Typing_context.create Environment.initial in
  let id1, ctx = Typing_context.fresh_type_variable_id ctx in
  let id2, ctx = Typing_context.fresh_type_variable_id ctx in
  let id3, _ = Typing_context.fresh_type_variable_id ctx in
  print_endline (if id1 < id2 && id2 < id3 then "monotonic" else "not monotonic");
  [%expect {| monotonic |}]

(** {1 Type Variable Creation Tests} *)

let%expect_test "new_type_variable creates TypeVariable" =
  let ctx = Typing_context.create Environment.initial in
  let ty, _ = Typing_context.new_type_variable ctx in
  (match ty with
   | Types.TypeVariable _ -> print_endline "is TypeVariable"
   | _ -> print_endline "ERROR: not TypeVariable");
  [%expect {| is TypeVariable |}]

let%expect_test "new_type_variable creates var at current level" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in (* level 2 *)
  let ty, _ = Typing_context.new_type_variable ctx in
  (match ty with
   | Types.TypeVariable tv ->
     Printf.printf "level=%d\n" tv.level
   | _ -> print_endline "ERROR: expected TypeVariable");
  [%expect {| level=2 |}]

let%expect_test "new_type_variable_at_level creates var at specified level" =
  let ctx = Typing_context.create Environment.initial in
  let ty, _ = Typing_context.new_type_variable_at_level ctx 5 in
  (match ty with
   | Types.TypeVariable tv ->
     Printf.printf "level=%d\n" tv.level
   | _ -> print_endline "ERROR: expected TypeVariable");
  [%expect {| level=5 |}]

let%expect_test "new_type_variable preserves var_id counter across contexts" =
  let ctx = Typing_context.create Environment.initial in
  let _, ctx = Typing_context.new_type_variable ctx in
  let _, ctx = Typing_context.new_type_variable ctx in
  let env2 = Environment.initial in
  let ctx = Typing_context.with_environment env2 ctx in
  let id, _ = Typing_context.fresh_type_variable_id ctx in
  Printf.printf "next_id=%d\n" id;
  [%expect {| next_id=46 |}]

(** {1 Generalization Tests} *)

let%expect_test "generalize quantifies vars at current level" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in (* level 2 *)
  let ty, ctx = Typing_context.new_type_variable ctx in (* var at level 2 *)
  let ctx = Typing_context.leave_level ctx in (* back to level 1 *)
  let scheme = Typing_context.generalize ctx ty in
  Printf.printf "quantified_count=%d\n" (List.length scheme.quantified_variables);
  [%expect {| quantified_count=1 |}]

let%expect_test "generalize does not quantify vars below current level" =
  let ctx = Typing_context.create Environment.initial in
  let ty, ctx = Typing_context.new_type_variable ctx in (* var at level 1 *)
  let ctx = Typing_context.enter_level ctx in (* level 2 *)
  let scheme = Typing_context.generalize ctx ty in
  Printf.printf "quantified_count=%d\n" (List.length scheme.quantified_variables);
  [%expect {| quantified_count=0 |}]

let%expect_test "generalize handles arrow types" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in (* level 2 *)
  let ty1, ctx = Typing_context.new_type_variable ctx in
  let ty2, ctx = Typing_context.new_type_variable ctx in
  let arrow_ty = Types.TypeArrow (Nolabel, ty1, ty2) in
  let ctx = Typing_context.leave_level ctx in
  let scheme = Typing_context.generalize ctx arrow_ty in
  Printf.printf "quantified_count=%d\n" (List.length scheme.quantified_variables);
  [%expect {| quantified_count=2 |}]

(** {1 Instantiation Tests} *)

let%expect_test "instantiate creates fresh variables" =
  let ctx = Typing_context.create Environment.initial in
  let ctx = Typing_context.enter_level ctx in
  let ty, ctx = Typing_context.new_type_variable ctx in
  let ctx = Typing_context.leave_level ctx in
  let scheme = Typing_context.generalize ctx ty in
  let inst_ty1, ctx = Typing_context.instantiate ctx scheme in
  let inst_ty2, _ = Typing_context.instantiate ctx scheme in
  (* inst_ty1 and inst_ty2 should be different type variables *)
  let are_different = match (inst_ty1, inst_ty2) with
    | Types.TypeVariable tv1, Types.TypeVariable tv2 -> tv1.id <> tv2.id
    | _ -> false
  in
  print_endline (if are_different then "different" else "same");
  [%expect {| different |}]

let%expect_test "instantiate preserves type structure" =
  let ctx = Typing_context.create Environment.initial in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let scheme = { Types.quantified_variables = []; body = int_ty } in
  let inst_ty, _ = Typing_context.instantiate ctx scheme in
  (match inst_ty with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) ->
     print_endline "int preserved"
   | _ -> print_endline "ERROR: type changed");
  [%expect {| int preserved |}]

let%expect_test "instantiate of non-polymorphic type returns same structure" =
  let ctx = Typing_context.create Environment.initial in
  let int_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) in
  let string_ty = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, []) in
  let arrow_ty = Types.TypeArrow (Nolabel, int_ty, string_ty) in
  let scheme = { Types.quantified_variables = []; body = arrow_ty } in
  let inst_ty, _ = Typing_context.instantiate ctx scheme in
  (match inst_ty with
   | Types.TypeArrow (Nolabel, Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []),
                      Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])) ->
     print_endline "arrow preserved"
   | _ -> print_endline "ERROR: type changed");
  [%expect {| arrow preserved |}]

(** {1 Type Lookup Tests} *)

let%expect_test "type_lookup returns None for builtin types" =
  let ctx = Typing_context.create Environment.initial in
  let result = Typing_context.type_lookup ctx (Types.PathBuiltin Types.BuiltinInt) in
  print_endline (match result with None -> "None" | Some _ -> "Some");
  [%expect {| None |}]

let%expect_test "module_type_lookup returns None for unknown paths" =
  let ctx = Typing_context.create Environment.initial in
  let result = Typing_context.module_type_lookup ctx (Types.PathLocal "unknown") in
  print_endline (match result with None -> "None" | Some _ -> "Some");
  [%expect {| None |}]
