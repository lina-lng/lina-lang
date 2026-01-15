open QCheck
open Parsing

(** Generator for simple identifiers that won't conflict with keywords *)
let gen_ident =
  let first_char = Gen.oneof [ Gen.char_range 'a' 'z' ] in
  let rest =
    Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 5)
  in
  Gen.map2 (fun c s -> String.make 1 c ^ s ^ "_") first_char rest

(** Generator for simple integer expressions *)
let gen_int_expr = Gen.map (fun n -> string_of_int (abs n mod 1000)) Gen.int

(** Generator for simple expressions *)
let rec gen_simple_expr depth =
  if depth <= 0 then Gen.oneof [ gen_int_expr; gen_ident ]
  else
    Gen.oneof
      [
        gen_int_expr;
        gen_ident;
        (* Parenthesized expression *)
        Gen.map
          (fun e -> "(" ^ e ^ ")")
          (Gen.delay (fun () -> gen_simple_expr (depth - 1)));
      ]

(** Generator for binary expressions *)
let rec gen_binop_expr depth =
  let ops = [| "+"; "-"; "*"; "/" |] in
  let gen_op = Gen.map (fun i -> ops.(i mod Array.length ops)) Gen.nat in
  if depth <= 0 then gen_simple_expr 0
  else
    Gen.oneof_weighted
      [
        (3, gen_simple_expr 1);
        ( 1,
          Gen.map3
            (fun left op right -> left ^ " " ^ op ^ " " ^ right)
            (Gen.delay (fun () -> gen_binop_expr (depth - 1)))
            gen_op
            (Gen.delay (fun () -> gen_binop_expr (depth - 1))) );
      ]

(** Test that valid expressions parse without error *)
let test_valid_expr_parses =
  Test.make ~name:"valid expressions parse without error" ~count:100
    (make (gen_binop_expr 3)) (fun src ->
      try
        let _ = Parse.expression_from_string src in
        true
      with _ -> false)

(** Test that integer literals roundtrip correctly *)
let test_int_roundtrip =
  Test.make ~name:"integer literals roundtrip" ~count:100
    (make (Gen.int_range 0 1000000)) (fun n ->
      try
        let src = string_of_int n in
        let expr = Parse.expression_from_string src in
        match expr.value with
        | Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger i) -> i = n
        | _ -> false
      with _ -> false)

(** Test that hex literals parse correctly *)
let test_hex_literals =
  Test.make ~name:"hex literals parse correctly" ~count:100
    (make (Gen.int_range 0 65535)) (fun n ->
      try
        let src = Printf.sprintf "0x%X" n in
        let expr = Parse.expression_from_string src in
        match expr.value with
        | Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger i) -> i = n
        | _ -> false
      with _ -> false)

(** Test that binary literals parse correctly *)
let test_binary_literals =
  Test.make ~name:"binary literals parse correctly" ~count:100
    (make (Gen.int_range 0 255)) (fun n ->
      try
        let rec to_binary n =
          if n = 0 then "0" else if n = 1 then "1" else to_binary (n / 2) ^ string_of_int (n mod 2)
        in
        let src = "0b" ^ to_binary n in
        let expr = Parse.expression_from_string src in
        match expr.value with
        | Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger i) -> i = n
        | _ -> false
      with _ -> false)

(** Test multiplication precedence over addition *)
let test_mult_precedence =
  Test.make ~name:"multiplication has higher precedence than addition" ~count:50
    (make (Gen.triple gen_ident gen_ident gen_ident)) (fun (a, b, c) ->
      try
        (* a + b * c should parse as a + (b * c) *)
        let src = Printf.sprintf "%s + %s * %s" a b c in
        let expr = Parse.expression_from_string src in
        match expr.value with
        | Syntax_tree.ExpressionApply
            ( { value = Syntax_tree.ExpressionVariable "+"; _ },
              [
                (_, { value = Syntax_tree.ExpressionVariable left; _ });
                (_, {
                  value =
                    Syntax_tree.ExpressionApply
                      ({ value = Syntax_tree.ExpressionVariable "*"; _ }, _);
                  _;
                });
              ] ) ->
            left = a
        | _ -> false
      with _ -> false)

(** Test application has higher precedence than binary operators *)
let test_app_precedence =
  Test.make ~name:"application has higher precedence than operators" ~count:50
    (make (Gen.triple gen_ident gen_ident gen_ident)) (fun (f, x, y) ->
      try
        (* f x + y should parse as (f x) + y *)
        let src = Printf.sprintf "%s %s + %s" f x y in
        let expr = Parse.expression_from_string src in
        match expr.value with
        | Syntax_tree.ExpressionApply
            ( { value = Syntax_tree.ExpressionVariable "+"; _ },
              [
                (_, {
                  value =
                    Syntax_tree.ExpressionApply
                      ({ value = Syntax_tree.ExpressionVariable func; _ }, _);
                  _;
                });
                (_, { value = Syntax_tree.ExpressionVariable right; _ });
              ] ) ->
            func = f && right = y
        | _ -> false
      with _ -> false)

let () =
  let suite =
    [
      test_valid_expr_parses;
      test_int_roundtrip;
      test_hex_literals;
      test_binary_literals;
      test_mult_precedence;
      test_app_precedence;
    ]
  in
  exit (QCheck_runner.run_tests ~verbose:true suite)
