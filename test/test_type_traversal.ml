(** Unit tests for Type_traversal module.

    Tests cover:
    - iter: depth-first pre-order traversal
    - map: bottom-up transformation
    - fold: accumulating traversal
    - row operations: iter_row, map_row, fold_row *)

open Typing

(** Helper to create a type variable for testing *)
let make_type_var id level =
  let tv = {
    Types.id;
    level;
    link = None;
    weak = false;
  } in
  (tv, Types.TypeVariable tv)

(** Helper to create a simple type constructor *)
let type_int = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])
let type_string = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])
let type_bool = Types.TypeConstructor (Types.PathBuiltin Types.BuiltinBool, [])

(** {1 iter Tests} *)

let%expect_test "iter: visits TypeVariable" =
  let _, ty_var = make_type_var 0 1 in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) ty_var;
  print_int !count;
  [%expect {| 1 |}]

let%expect_test "iter: visits TypeConstructor with no args" =
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) type_int;
  print_int !count;
  [%expect {| 1 |}]

let%expect_test "iter: visits TypeConstructor with args" =
  let _, ty_var = make_type_var 0 1 in
  let option_ty = Types.TypeConstructor (Types.PathLocal "option", [ty_var]) in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) option_ty;
  print_int !count;
  [%expect {| 2 |}]

let%expect_test "iter: visits nested TypeArrow" =
  let arrow_ty = Types.TypeArrow (type_int, Types.TypeArrow (type_string, type_bool)) in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) arrow_ty;
  (* outer arrow, int, inner arrow, string, bool = 5 nodes *)
  print_int !count;
  [%expect {| 5 |}]

let%expect_test "iter: visits TypeTuple elements" =
  let tuple_ty = Types.TypeTuple [type_int; type_string; type_bool] in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) tuple_ty;
  (* tuple + 3 elements = 4 nodes *)
  print_int !count;
  [%expect {| 4 |}]

let%expect_test "iter: visits TypeRecord fields and row_more" =
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent type_int);
      ("y", Types.RowFieldPresent type_string);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let record_ty = Types.TypeRecord row in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) record_ty;
  (* record + x:int + y:string + row_empty = 4 nodes *)
  print_int !count;
  [%expect {| 4 |}]

let%expect_test "iter: handles TypeRowEmpty" =
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) Types.TypeRowEmpty;
  print_int !count;
  [%expect {| 1 |}]

let%expect_test "iter: follows union-find links" =
  let tv, ty_var = make_type_var 0 1 in
  (* Link type variable to int *)
  tv.link <- Some type_int;
  let visited_types = ref [] in
  Type_traversal.iter (fun ty ->
    match ty with
    | Types.TypeVariable _ -> visited_types := "var" :: !visited_types
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) ->
      visited_types := "int" :: !visited_types
    | _ -> ()
  ) ty_var;
  (* Should follow link and visit int, not var *)
  List.iter print_endline (List.rev !visited_types);
  [%expect {| int |}]

let%expect_test "iter: visits in pre-order (parent before children)" =
  let arrow_ty = Types.TypeArrow (type_int, type_string) in
  let order = ref [] in
  Type_traversal.iter (fun ty ->
    match ty with
    | Types.TypeArrow _ -> order := "arrow" :: !order
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) ->
      order := "int" :: !order
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, _) ->
      order := "string" :: !order
    | _ -> ()
  ) arrow_ty;
  List.iter print_endline (List.rev !order);
  [%expect {|
    arrow
    int
    string |}]

(** {1 map Tests} *)

let%expect_test "map: transforms TypeVariable" =
  let _, ty_var = make_type_var 0 1 in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable _ -> type_int
    | _ -> ty
  ) ty_var in
  (match result with
   | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []) ->
     print_endline "transformed to int"
   | _ -> print_endline "ERROR");
  [%expect {| transformed to int |}]

let%expect_test "map: transforms TypeConstructor args" =
  let _, ty_var = make_type_var 0 1 in
  let option_ty = Types.TypeConstructor (Types.PathLocal "option", [ty_var]) in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable _ -> type_int
    | _ -> ty
  ) option_ty in
  (match result with
   | Types.TypeConstructor (Types.PathLocal "option",
       [Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])]) ->
     print_endline "int option"
   | _ -> print_endline "ERROR");
  [%expect {| int option |}]

let%expect_test "map: transforms nested TypeArrow" =
  let _, ty_var = make_type_var 0 1 in
  let arrow_ty = Types.TypeArrow (ty_var, ty_var) in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable _ -> type_int
    | _ -> ty
  ) arrow_ty in
  (match result with
   | Types.TypeArrow (
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []),
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])) ->
     print_endline "int -> int"
   | _ -> print_endline "ERROR");
  [%expect {| int -> int |}]

let%expect_test "map: transforms TypeTuple elements" =
  let tv1, ty_var1 = make_type_var 0 1 in
  let tv2, ty_var2 = make_type_var 1 1 in
  let tuple_ty = Types.TypeTuple [ty_var1; ty_var2] in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable tv when tv.id = tv1.id -> type_int
    | Types.TypeVariable tv when tv.id = tv2.id -> type_string
    | _ -> ty
  ) tuple_ty in
  (match result with
   | Types.TypeTuple [
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []);
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])] ->
     print_endline "int * string"
   | _ -> print_endline "ERROR");
  [%expect {| int * string |}]

let%expect_test "map: transforms TypeRecord fields" =
  let _, ty_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [("x", Types.RowFieldPresent ty_var)];
    row_more = Types.TypeRowEmpty;
  } in
  let record_ty = Types.TypeRecord row in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable _ -> type_int
    | _ -> ty
  ) record_ty in
  (match result with
   | Types.TypeRecord { row_fields = [("x", Types.RowFieldPresent
       (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])))]; _ } ->
     print_endline "{ x: int }"
   | _ -> print_endline "ERROR");
  [%expect {| { x: int } |}]

let%expect_test "map: identity function preserves structure" =
  let arrow_ty = Types.TypeArrow (type_int, type_string) in
  let result = Type_traversal.map (fun ty -> ty) arrow_ty in
  (match result with
   | Types.TypeArrow (
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, []),
       Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, [])) ->
     print_endline "preserved"
   | _ -> print_endline "ERROR");
  [%expect {| preserved |}]

let%expect_test "map: applies in bottom-up order" =
  (* Transform all types to int, then wrap in option at the top *)
  let arrow_ty = Types.TypeArrow (type_int, type_string) in
  let order = ref [] in
  let _ = Type_traversal.map (fun ty ->
    (match ty with
     | Types.TypeArrow _ -> order := "arrow" :: !order
     | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) ->
       order := "int" :: !order
     | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, _) ->
       order := "string" :: !order
     | _ -> ());
    ty
  ) arrow_ty in
  (* Bottom-up visits result type first (right-to-left in argument order),
     then arg type, then the arrow itself. Result type is processed first
     because it's the last child in the AST. *)
  List.iter print_endline (List.rev !order);
  [%expect {|
    string
    int
    arrow |}]

(** {1 fold Tests} *)

let%expect_test "fold: accumulates over all nodes" =
  let arrow_ty = Types.TypeArrow (type_int, Types.TypeArrow (type_string, type_bool)) in
  let count = Type_traversal.fold (fun acc _ -> acc + 1) 0 arrow_ty in
  (* outer arrow + int + inner arrow + string + bool = 5 *)
  print_int count;
  [%expect {| 5 |}]

let%expect_test "fold: visits in depth-first pre-order" =
  let arrow_ty = Types.TypeArrow (type_int, type_string) in
  let order = Type_traversal.fold (fun acc ty ->
    match ty with
    | Types.TypeArrow _ -> acc @ ["arrow"]
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) ->
      acc @ ["int"]
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, _) ->
      acc @ ["string"]
    | _ -> acc
  ) [] arrow_ty in
  List.iter print_endline order;
  [%expect {|
    arrow
    int
    string |}]

let%expect_test "fold: handles TypeRowEmpty" =
  let count = Type_traversal.fold (fun acc _ -> acc + 1) 0 Types.TypeRowEmpty in
  print_int count;
  [%expect {| 1 |}]

let%expect_test "fold: collects type variables" =
  let _, ty_var1 = make_type_var 0 1 in
  let _, ty_var2 = make_type_var 1 1 in
  let arrow_ty = Types.TypeArrow (ty_var1, Types.TypeArrow (type_int, ty_var2)) in
  let vars = Type_traversal.fold (fun acc ty ->
    match ty with
    | Types.TypeVariable tv -> tv.id :: acc
    | _ -> acc
  ) [] arrow_ty in
  let sorted = List.sort compare vars in
  List.iter (fun id -> Printf.printf "%d\n" id) sorted;
  [%expect {|
    0
    1 |}]

let%expect_test "fold: accumulates type constructor count" =
  let complex_ty = Types.TypeTuple [
    type_int;
    Types.TypeArrow (type_string, type_bool);
    Types.TypeConstructor (Types.PathLocal "option", [type_int]);
  ] in
  let constructor_count = Type_traversal.fold (fun acc ty ->
    match ty with
    | Types.TypeConstructor _ -> acc + 1
    | _ -> acc
  ) 0 complex_ty in
  print_int constructor_count;
  [%expect {| 5 |}]

(** {1 iter_row Tests} *)

let%expect_test "iter_row: handles multiple fields" =
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent type_int);
      ("y", Types.RowFieldPresent type_string);
      ("z", Types.RowFieldPresent type_bool);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let count = ref 0 in
  Type_traversal.iter_row (fun _ -> incr count) row;
  (* 3 field types + row_more = 4 nodes *)
  print_int !count;
  [%expect {| 4 |}]

let%expect_test "iter_row: handles row variable in row_more" =
  let _, row_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [("x", Types.RowFieldPresent type_int)];
    row_more = row_var;
  } in
  let visited = ref [] in
  Type_traversal.iter_row (fun ty ->
    match ty with
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) ->
      visited := "int" :: !visited
    | Types.TypeVariable _ ->
      visited := "var" :: !visited
    | _ -> ()
  ) row;
  List.iter print_endline (List.rev !visited);
  [%expect {|
    int
    var |}]

(** {1 map_row Tests} *)

let%expect_test "map_row: transforms all fields" =
  let _, ty_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent ty_var);
      ("y", Types.RowFieldPresent ty_var);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_traversal.map_row (fun ty ->
    match ty with
    | Types.TypeVariable _ -> type_int
    | _ -> ty
  ) row in
  let field_check = List.for_all (fun (_, field) ->
    match field with
    | Types.RowFieldPresent (Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, [])) -> true
    | _ -> false
  ) result.row_fields in
  print_endline (if field_check then "all int" else "ERROR");
  [%expect {| all int |}]

let%expect_test "map_row: preserves field names" =
  let row = {
    Types.row_fields = [
      ("alpha", Types.RowFieldPresent type_int);
      ("beta", Types.RowFieldPresent type_string);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let result = Type_traversal.map_row (fun ty -> ty) row in
  let names = List.map fst result.row_fields in
  List.iter print_endline names;
  [%expect {|
    alpha
    beta |}]

let%expect_test "map_row: transforms row_more" =
  let _, row_var = make_type_var 0 1 in
  let row = {
    Types.row_fields = [];
    row_more = row_var;
  } in
  let result = Type_traversal.map_row (fun ty ->
    match ty with
    | Types.TypeVariable _ -> Types.TypeRowEmpty
    | _ -> ty
  ) row in
  (match result.row_more with
   | Types.TypeRowEmpty -> print_endline "closed row"
   | _ -> print_endline "ERROR");
  [%expect {| closed row |}]

(** {1 fold_row Tests} *)

let%expect_test "fold_row: counts all types in row" =
  let row = {
    Types.row_fields = [
      ("a", Types.RowFieldPresent type_int);
      ("b", Types.RowFieldPresent type_string);
      ("c", Types.RowFieldPresent type_bool);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let count = Type_traversal.fold_row (fun acc _ -> acc + 1) 0 row in
  print_int count;
  [%expect {| 4 |}]

let%expect_test "fold_row: collects field type names" =
  let row = {
    Types.row_fields = [
      ("x", Types.RowFieldPresent type_int);
      ("y", Types.RowFieldPresent type_string);
    ];
    row_more = Types.TypeRowEmpty;
  } in
  let types = Type_traversal.fold_row (fun acc ty ->
    match ty with
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinInt, _) -> "int" :: acc
    | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinString, _) -> "string" :: acc
    | Types.TypeRowEmpty -> "empty" :: acc
    | _ -> acc
  ) [] row in
  List.iter print_endline (List.rev types);
  [%expect {|
    int
    string
    empty |}]

(** {1 Complex Traversal Tests} *)

let%expect_test "iter: deeply nested type" =
  (* Build: int -> (string * bool) -> (option int) *)
  let option_int = Types.TypeConstructor (Types.PathLocal "option", [type_int]) in
  let tuple = Types.TypeTuple [type_string; type_bool] in
  let complex = Types.TypeArrow (type_int, Types.TypeArrow (tuple, option_int)) in
  let count = ref 0 in
  Type_traversal.iter (fun _ -> incr count) complex;
  (* outer arrow, int, inner arrow, tuple, string, bool, option, int = 8 *)
  print_int !count;
  [%expect {| 8 |}]

let%expect_test "map: substitution in complex type" =
  let tv, ty_var = make_type_var 42 1 in
  (* Build: 'a -> ('a * 'a) -> 'a option *)
  let option_a = Types.TypeConstructor (Types.PathLocal "option", [ty_var]) in
  let tuple = Types.TypeTuple [ty_var; ty_var] in
  let complex = Types.TypeArrow (ty_var, Types.TypeArrow (tuple, option_a)) in
  let result = Type_traversal.map (fun ty ->
    match ty with
    | Types.TypeVariable v when v.id = tv.id -> type_int
    | _ -> ty
  ) complex in
  (* Verify all type variables replaced with int *)
  let has_var = ref false in
  Type_traversal.iter (fun ty ->
    match ty with
    | Types.TypeVariable v when v.id = tv.id -> has_var := true
    | _ -> ()
  ) result;
  print_endline (if !has_var then "ERROR: still has var" else "all substituted");
  [%expect {| all substituted |}]

let%expect_test "fold: compute depth of type" =
  (* int has depth 1, int -> int has depth 2, int -> (int -> int) has depth 3 *)
  let rec type_depth ty =
    Type_traversal.fold (fun acc sub_ty ->
      if sub_ty == ty then acc else max acc (1 + type_depth sub_ty)
    ) 1 ty
  in
  print_int (type_depth type_int);
  print_newline ();
  print_int (type_depth (Types.TypeArrow (type_int, type_int)));
  print_newline ();
  print_int (type_depth (Types.TypeArrow (type_int, Types.TypeArrow (type_int, type_int))));
  [%expect {|
    1
    2
    3 |}]

(* Suppress unused variable warnings for test helpers *)
let _ = make_type_var
