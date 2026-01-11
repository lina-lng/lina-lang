(** Tests for format_accessors.ml - typed child accessors for CST nodes.

    These tests verify that the accessor functions return expected values
    by parsing source code and checking the extracted components. *)

open Cst
open Lina_format.Format_accessors

(** {1 Helper Functions} *)

let parse source = Cst_parser.parse "test" source

let show_token tok = tok.Red_tree.green.Green_tree.text

let show_node node = Red_tree.text node

(** Helper to find the first node of a specific kind in the tree *)
let rec find_first_node_by_kind kind node =
  if Syntax_kind.equal (Red_tree.kind node) kind then Some node
  else
    let children = Red_tree.child_nodes node in
    List.find_map (find_first_node_by_kind kind) children

(** {1 Basic Accessor Tests} *)

let%expect_test "parse and show root text" =
  let root = parse "let x = 42" in
  print_string (Red_tree.text root);
  [%expect {| let x = 42 |}]

let%expect_test "find value definition node" =
  let root = parse "let x = 42" in
  (match find_first_node_by_kind Syntax_kind.NK_VALUE_DEFINITION root with
   | Some node ->
       print_string "found value definition";
       print_newline ();
       print_string (String.trim (Red_tree.text node))
   | None -> print_string "not found");
  [%expect {|
    found value definition
    let x = 42 |}]

let%expect_test "Value_definition accessors" =
  let root = parse "let rec factorial n = n" in
  (match find_first_node_by_kind Syntax_kind.NK_VALUE_DEFINITION root with
   | Some node ->
       Printf.printf "has let_keyword: %b\n"
         (Option.is_some (Value_definition.let_keyword node));
       Printf.printf "has rec_keyword: %b\n"
         (Option.is_some (Value_definition.rec_keyword node));
       Printf.printf "num bindings: %d\n"
         (List.length (Value_definition.bindings node))
   | None -> print_string "not found");
  [%expect {|
    has let_keyword: true
    has rec_keyword: true
    num bindings: 1 |}]

let%expect_test "Type_definition accessors" =
  let root = parse "type 'a list = Nil | Cons of 'a" in
  (match find_first_node_by_kind Syntax_kind.NK_TYPE_DEFINITION root with
   | Some node ->
       Printf.printf "has type_keyword: %b\n"
         (Option.is_some (Type_definition.type_keyword node));
       Printf.printf "num declarations: %d\n"
         (List.length (Type_definition.declarations node))
   | None -> print_string "not found");
  [%expect {|
    has type_keyword: true
    num declarations: 1 |}]

let%expect_test "Structure accessors" =
  let root = parse "module M = struct let x = 1 end" in
  (match find_first_node_by_kind Syntax_kind.NK_STRUCTURE root with
   | Some node ->
       Printf.printf "has struct_keyword: %b\n"
         (Option.is_some (Structure.struct_keyword node));
       Printf.printf "has end_keyword: %b\n"
         (Option.is_some (Structure.end_keyword node));
       Printf.printf "num items: %d\n" (List.length (Structure.items node))
   | None -> print_string "not found");
  [%expect {|
    has struct_keyword: true
    has end_keyword: true
    num items: 1 |}]

(** {1 Expression Accessors - wrapped in let bindings} *)

let%expect_test "If_expr accessors" =
  (* Wrap expression in a let binding since standalone expressions aren't valid *)
  let root = parse "let result = if x then 1 else 2" in
  (match find_first_node_by_kind Syntax_kind.NK_IF_EXPR root with
   | Some node ->
       Printf.printf "has if_keyword: %b\n"
         (Option.is_some (If_expr.if_keyword node));
       Printf.printf "has then_keyword: %b\n"
         (Option.is_some (If_expr.then_keyword node));
       Printf.printf "has else_keyword: %b\n"
         (Option.is_some (If_expr.else_keyword node));
       Printf.printf "has condition: %b\n"
         (Option.is_some (If_expr.condition node));
       Printf.printf "has then_branch: %b\n"
         (Option.is_some (If_expr.then_branch node));
       Printf.printf "has else_branch: %b\n"
         (Option.is_some (If_expr.else_branch node))
   | None -> print_string "not found");
  [%expect {|
    has if_keyword: true
    has then_keyword: true
    has else_keyword: true
    has condition: true
    has then_branch: true
    has else_branch: true |}]

let%expect_test "Match_expr accessors" =
  let root = parse "let result = match x with | None -> 0 | Some y -> y" in
  (match find_first_node_by_kind Syntax_kind.NK_MATCH_EXPR root with
   | Some node ->
       Printf.printf "has match_keyword: %b\n"
         (Option.is_some (Match_expr.match_keyword node));
       Printf.printf "has with_keyword: %b\n"
         (Option.is_some (Match_expr.with_keyword node));
       Printf.printf "has scrutinee: %b\n"
         (Option.is_some (Match_expr.scrutinee node));
       Printf.printf "num arms: %d\n" (List.length (Match_expr.arms node))
   | None -> print_string "not found");
  [%expect {|
    has match_keyword: true
    has with_keyword: true
    has scrutinee: true
    num arms: 2 |}]

let%expect_test "Infix_expr accessors" =
  let root = parse "let sum = 1 + 2" in
  (match find_first_node_by_kind Syntax_kind.NK_INFIX_EXPR root with
   | Some node ->
       Printf.printf "has left: %b\n" (Option.is_some (Infix_expr.left node));
       Printf.printf "has operator: %b\n"
         (Option.is_some (Infix_expr.operator node));
       Printf.printf "has right: %b\n" (Option.is_some (Infix_expr.right node));
       (match Infix_expr.operator node with
        | Some op -> Printf.printf "operator text: %s\n" (show_token op)
        | None -> ())
   | None -> print_string "not found");
  [%expect {|
    has left: true
    has operator: true
    has right: true
    operator text: + |}]

let%expect_test "Record_expr accessors" =
  let root = parse "let r = { x = 1; y = 2 }" in
  (match find_first_node_by_kind Syntax_kind.NK_RECORD_EXPR root with
   | Some node ->
       Printf.printf "has lbrace: %b\n"
         (Option.is_some (Record_expr.lbrace node));
       Printf.printf "has rbrace: %b\n"
         (Option.is_some (Record_expr.rbrace node));
       Printf.printf "num fields: %d\n" (List.length (Record_expr.fields node))
   | None -> print_string "not found");
  [%expect {|
    has lbrace: true
    has rbrace: true
    num fields: 2 |}]

let%expect_test "Tuple_expr accessors" =
  let root = parse "let t = (1, 2, 3)" in
  (match find_first_node_by_kind Syntax_kind.NK_TUPLE_EXPR root with
   | Some node ->
       Printf.printf "has lparen: %b\n"
         (Option.is_some (Tuple_expr.lparen node));
       Printf.printf "has rparen: %b\n"
         (Option.is_some (Tuple_expr.rparen node));
       Printf.printf "num elements: %d\n"
         (List.length (Tuple_expr.elements node))
   | None -> print_string "not found");
  [%expect {| not found |}]

let%expect_test "Apply_expr accessors" =
  let root = parse "let result = f x y" in
  (match find_first_node_by_kind Syntax_kind.NK_APPLY_EXPR root with
   | Some node ->
       Printf.printf "has func: %b\n" (Option.is_some (Apply_expr.func node));
       Printf.printf "num arguments: %d\n"
         (List.length (Apply_expr.arguments node))
   | None -> print_string "not found");
  [%expect {|
    has func: true
    num arguments: 1 |}]

let%expect_test "Function_expr accessors" =
  let root = parse "let f = fun x y -> x + y" in
  (match find_first_node_by_kind Syntax_kind.NK_FUNCTION_EXPR root with
   | Some node ->
       Printf.printf "has fun_keyword: %b\n"
         (Option.is_some (Function_expr.fun_keyword node));
       Printf.printf "has arrow: %b\n"
         (Option.is_some (Function_expr.arrow node));
       Printf.printf "has body: %b\n" (Option.is_some (Function_expr.body node));
       Printf.printf "num parameters: %d\n"
         (List.length (Function_expr.parameters node))
   | None -> print_string "not found");
  [%expect {|
    has fun_keyword: true
    has arrow: true
    has body: true
    num parameters: 2 |}]

(** {1 Match Arm Accessors} *)

let%expect_test "Match_arm accessors" =
  let root = parse "let result = match x with | Some y -> y + 1" in
  (match find_first_node_by_kind Syntax_kind.NK_MATCH_ARM root with
   | Some node ->
       Printf.printf "has bar: %b\n" (Option.is_some (Match_arm.bar node));
       Printf.printf "has pattern: %b\n"
         (Option.is_some (Match_arm.pattern node));
       Printf.printf "has arrow: %b\n" (Option.is_some (Match_arm.arrow node));
       Printf.printf "has body: %b\n" (Option.is_some (Match_arm.body node))
   | None -> print_string "not found");
  [%expect {|
    has bar: true
    has pattern: true
    has arrow: true
    has body: true |}]

(** {1 Binding Accessors} *)

let%expect_test "Binding accessors" =
  let root = parse "let f x y = x + y" in
  (match find_first_node_by_kind Syntax_kind.NK_BINDING root with
   | Some node ->
       Printf.printf "has pattern: %b\n"
         (Option.is_some (Binding.pattern node));
       Printf.printf "has equals: %b\n" (Option.is_some (Binding.equals node));
       Printf.printf "has body: %b\n" (Option.is_some (Binding.body node));
       Printf.printf "num parameters: %d\n"
         (List.length (Binding.parameters node))
   | None -> print_string "not found");
  [%expect {|
    has pattern: true
    has equals: true
    has body: true
    num parameters: 2 |}]
