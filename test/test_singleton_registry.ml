(** Unit tests for Singleton_registry module.

    Tests cover:
    - empty: empty registry state
    - register: adding singletons to registry
    - var_name: generating Lua variable names
    - generate_preamble: generating Lua statements for singletons *)

open Lua

(** {1 empty Tests} *)

let%expect_test "empty: generates no preamble" =
  let reg = Singleton_registry.empty in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 0 |}]

(** {1 var_name Tests} *)

let%expect_test "var_name: generates correct format for option None" =
  let name = Singleton_registry.var_name "option" 0 in
  print_endline name;
  [%expect {| _Ctor_option_0 |}]

let%expect_test "var_name: generates correct format for option Some" =
  let name = Singleton_registry.var_name "option" 1 in
  print_endline name;
  [%expect {| _Ctor_option_1 |}]

let%expect_test "var_name: generates correct format for bool False" =
  let name = Singleton_registry.var_name "bool" 0 in
  print_endline name;
  [%expect {| _Ctor_bool_0 |}]

let%expect_test "var_name: generates correct format for bool True" =
  let name = Singleton_registry.var_name "bool" 1 in
  print_endline name;
  [%expect {| _Ctor_bool_1 |}]

let%expect_test "var_name: handles multi-word type names" =
  let name = Singleton_registry.var_name "my_result" 0 in
  print_endline name;
  [%expect {| _Ctor_my_result_0 |}]

let%expect_test "var_name: handles large tag indices" =
  let name = Singleton_registry.var_name "direction" 10 in
  print_endline name;
  [%expect {| _Ctor_direction_10 |}]

(** {1 register Tests} *)

let%expect_test "register: single registration generates one statement" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 1 |}]

let%expect_test "register: multiple registrations generate multiple statements" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "result" 0 in
  let reg = Singleton_registry.register reg "result" 1 in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 3 |}]

let%expect_test "register: duplicate registration is idempotent" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "option" 0 in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 1 |}]

let%expect_test "register: same type different tags are distinct" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "option" 1 in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 2 |}]

let%expect_test "register: different types same tag are distinct" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "result" 0 in
  let preamble = Singleton_registry.generate_preamble reg in
  print_int (List.length preamble);
  [%expect {| 2 |}]

(** {1 generate_preamble Tests} *)

let%expect_test "generate_preamble: generates correct Lua statement structure" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "option" 0 in
  let preamble = Singleton_registry.generate_preamble reg in
  (match preamble with
   | [Lua_ast.StatementLocal ([name], [Lua_ast.ExpressionTable fields])] ->
     print_endline name;
     (match fields with
      | [Lua_ast.FieldNamed ("_tag", Lua_ast.ExpressionNumber tag)] ->
        Printf.printf "_tag = %d\n" (int_of_float tag)
      | _ -> print_endline "ERROR: wrong fields")
   | _ -> print_endline "ERROR: wrong statement structure");
  [%expect {|
    _Ctor_option_0
    _tag = 0 |}]

let%expect_test "generate_preamble: tag value matches registration" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "direction" 3 in
  let preamble = Singleton_registry.generate_preamble reg in
  (match preamble with
   | [Lua_ast.StatementLocal ([name], [Lua_ast.ExpressionTable fields])] ->
     print_endline name;
     (match fields with
      | [Lua_ast.FieldNamed ("_tag", Lua_ast.ExpressionNumber tag)] ->
        Printf.printf "_tag = %d\n" (int_of_float tag)
      | _ -> print_endline "ERROR: wrong fields")
   | _ -> print_endline "ERROR: wrong statement structure");
  [%expect {|
    _Ctor_direction_3
    _tag = 3 |}]

let%expect_test "generate_preamble: multiple singletons have correct tags" =
  let reg = Singleton_registry.empty in
  let reg = Singleton_registry.register reg "color" 0 in
  let reg = Singleton_registry.register reg "color" 1 in
  let reg = Singleton_registry.register reg "color" 2 in
  let preamble = Singleton_registry.generate_preamble reg in
  (* Extract and sort by name to get predictable output *)
  let items = List.filter_map (fun stmt ->
    match stmt with
    | Lua_ast.StatementLocal ([name], [Lua_ast.ExpressionTable [Lua_ast.FieldNamed ("_tag", Lua_ast.ExpressionNumber tag)]]) ->
      Some (name, int_of_float tag)
    | _ -> None
  ) preamble in
  let sorted = List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) items in
  List.iter (fun (name, tag) -> Printf.printf "%s: _tag = %d\n" name tag) sorted;
  [%expect {|
    _Ctor_color_0: _tag = 0
    _Ctor_color_1: _tag = 1
    _Ctor_color_2: _tag = 2 |}]

(** {1 Integration-like Tests} *)

let%expect_test "full workflow: register, generate, verify names" =
  let reg = Singleton_registry.empty in
  (* Simulate registering None and Left constructors *)
  let reg = Singleton_registry.register reg "option" 0 in
  let reg = Singleton_registry.register reg "either" 0 in
  let preamble = Singleton_registry.generate_preamble reg in
  let names = List.filter_map (fun stmt ->
    match stmt with
    | Lua_ast.StatementLocal ([name], _) -> Some name
    | _ -> None
  ) preamble in
  let sorted = List.sort String.compare names in
  List.iter print_endline sorted;
  [%expect {|
    _Ctor_either_0
    _Ctor_option_0 |}]

let%expect_test "register: functional style (immutable updates)" =
  (* Verify that register returns new registry, doesn't mutate *)
  let reg1 = Singleton_registry.empty in
  let reg2 = Singleton_registry.register reg1 "option" 0 in
  let len1 = List.length (Singleton_registry.generate_preamble reg1) in
  let len2 = List.length (Singleton_registry.generate_preamble reg2) in
  Printf.printf "reg1: %d, reg2: %d\n" len1 len2;
  [%expect {| reg1: 0, reg2: 1 |}]
