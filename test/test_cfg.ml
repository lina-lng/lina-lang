(** Tests for the Control Flow Graph module.

    Tests the CFG construction and reachability analysis used
    for dead code detection. *)

let get_typed_expr source =
  let full_source =
    Analysis_test_helpers.diverging_prelude ^ "\nlet result = " ^ source
  in
  let typed_ast = Analysis_test_helpers.type_check full_source in

  let rec find_result = function
    | [] -> failwith "No result binding found"
    | item :: rest -> (
        match item.Typing.Typed_tree.structure_item_desc with
        | Typing.Typed_tree.TypedStructureValue (_, [ binding ]) -> (
            match
              binding.Typing.Typed_tree.binding_pattern.Typing.Typed_tree
              .pattern_desc
            with
            | Typing.Typed_tree.TypedPatternVariable id
              when Common.Identifier.name id = "result" ->
                binding.Typing.Typed_tree.binding_expression
            | _ -> find_result rest)
        | _ -> find_result rest)
  in
  find_result typed_ast

let%expect_test "simple expression creates single segment" =
  let expr = get_typed_expr "1 + 2" in
  let cfg = Analysis.Cfg.build expr in
  Printf.printf "Entry: %d\n" cfg.Analysis.Cfg.entry;
  Printf.printf "Exit count: %d\n" (List.length cfg.Analysis.Cfg.exit);
  Printf.printf "Segment count: %d\n" (Hashtbl.length cfg.Analysis.Cfg.segments);
  [%expect {|
    Entry: 0
    Exit count: 1
    Segment count: 1
    |}]

let%expect_test "sequence creates connected segments" =
  let expr = get_typed_expr "let x = 1 in x; x + 1" in
  let cfg = Analysis.Cfg.build expr in
  Printf.printf "Segment count: %d\n" (Hashtbl.length cfg.Analysis.Cfg.segments);
  Printf.printf "Has multiple segments: %b\n" (Hashtbl.length cfg.Analysis.Cfg.segments > 1);
  [%expect {|
    Segment count: 5
    Has multiple segments: true
    |}]

let%expect_test "if expression creates branching CFG" =
  let expr = get_typed_expr "if true then 1 else 2" in
  let cfg = Analysis.Cfg.build expr in
  Printf.printf "Segment count: %d\n" (Hashtbl.length cfg.Analysis.Cfg.segments);
  Printf.printf "Has branching: %b\n" (Hashtbl.length cfg.Analysis.Cfg.segments >= 3);
  [%expect {|
    Segment count: 4
    Has branching: true
    |}]

let%expect_test "all segments reachable in normal code" =
  let expr = get_typed_expr "let x = 1 in x + 2" in
  let cfg = Analysis.Cfg.build expr in
  let unreachable = Analysis.Cfg.find_unreachable cfg in
  Printf.printf "Unreachable count: %d\n" (List.length unreachable);
  [%expect {|
    Unreachable count: 0
    |}]

let%expect_test "code after raise is unreachable" =
  let expr = get_typed_expr "raise \"error\"; 42" in
  let cfg = Analysis.Cfg.build expr in
  let unreachable = Analysis.Cfg.find_unreachable cfg in
  Printf.printf "Unreachable count: %d\n" (List.length unreachable);
  Printf.printf "Has unreachable: %b\n" (List.length unreachable > 0);
  [%expect {|
    Unreachable count: 1
    Has unreachable: true
    |}]

let%expect_test "both if branches diverging makes join unreachable" =
  let expr = get_typed_expr "(if true then raise \"a\" else raise \"b\"); 42" in
  let cfg = Analysis.Cfg.build expr in
  let unreachable = Analysis.Cfg.find_unreachable cfg in
  Printf.printf "Has unreachable: %b\n" (List.length unreachable > 0);
  [%expect {|
    Has unreachable: true
    |}]

let%expect_test "one if branch diverging keeps join reachable" =
  let expr = get_typed_expr "(if true then raise \"a\" else ()); 42" in
  let cfg = Analysis.Cfg.build expr in
  let unreachable = Analysis.Cfg.find_unreachable cfg in
  Printf.printf "Unreachable count: %d\n" (List.length unreachable);
  [%expect {|
    Unreachable count: 0
    |}]

let%expect_test "mark_reachable marks segments in place" =
  let expr = get_typed_expr "let x = 1 in x + 2" in
  let cfg = Analysis.Cfg.build expr in
  Analysis.Cfg.mark_reachable cfg;
  let all_reachable =
    Hashtbl.fold
      (fun _id seg acc -> acc && seg.Analysis.Cfg.reachable)
      cfg.Analysis.Cfg.segments true
  in
  Printf.printf "All segments reachable: %b\n" all_reachable;
  [%expect {|
    All segments reachable: true
    |}]
