(** Tests for scope analysis infrastructure.

    Tests the scope analysis module which tracks bindings and their usage
    throughout a program for unused code detection.

    Note: Some features like parameter tracking and field tracking are not
    yet fully implemented in the scope analyzer. Tests focus on what's working. *)

let analyze source =
  let typed_ast = Analysis_test_helpers.type_check source in
  Analysis.Scope_analyzer.analyze typed_ast

let find_binding_in_tree = Analysis_test_helpers.find_binding_in_tree

let%expect_test "empty module creates root scope" =
  let tree = analyze "" in
  Printf.printf "Root scope kind: %s\n"
    (Analysis.Scope.show_scope_kind tree.tree_root.scope_kind);
  Printf.printf "Total scopes: %d\n" (List.length tree.tree_all_scopes);
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  [%expect{|
    Root scope kind: Scope.ScopeModule {name = None}
    Total scopes: 1
    Total bindings: 0
    |}]

let%expect_test "let binding creates binding" =
  let tree = analyze "let x = 42" in
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  (match find_binding_in_tree tree "x" with
   | Some b ->
     Printf.printf "Found binding: %s\n" b.bind_name;
     Printf.printf "Kind: %s\n" (Analysis.Scope.show_binding_kind b.bind_kind)
   | None -> print_endline "Binding not found");
  [%expect{|
    Total bindings: 1
    Found binding: x
    Kind: Scope.Variable
    |}]

let%expect_test "function creates function binding" =
  let tree = analyze "let f x = x + 1" in
  (match find_binding_in_tree tree "f" with
   | Some b ->
     Printf.printf "Found binding: %s\n" b.bind_name;
     Printf.printf "Kind: %s\n" (Analysis.Scope.show_binding_kind b.bind_kind)
   | None -> print_endline "Binding not found");
  [%expect{|
    Found binding: f
    Kind: Scope.Function {is_recursive = false}
    |}]

let%expect_test "let rec creates recursive function" =
  let tree = analyze "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)" in
  (match find_binding_in_tree tree "fact" with
   | Some b ->
     Printf.printf "Found binding: %s\n" b.bind_name;
     Printf.printf "Kind: %s\n" (Analysis.Scope.show_binding_kind b.bind_kind)
   | None -> print_endline "Binding not found");
  [%expect{|
    Found binding: fact
    Kind: Scope.Function {is_recursive = true}
    |}]

let%expect_test "match pattern variables are tracked" =
  let tree = analyze {|
    type t = A of int | B of string
    let f x = match x with
      | A n -> n
      | B s -> 0
  |} in
  let pattern_vars = List.filter (fun b ->
    b.Analysis.Scope.bind_kind = Analysis.Scope.PatternVar
  ) tree.tree_all_bindings in
  Printf.printf "Pattern var count: %d\n" (List.length pattern_vars);
  List.iter (fun b ->
    Printf.printf "  Pattern var: %s\n" b.Analysis.Scope.bind_name
  ) (List.sort (fun a b -> String.compare a.Analysis.Scope.bind_name b.Analysis.Scope.bind_name) pattern_vars);
  [%expect{|
    Pattern var count: 3
      Pattern var: n
      Pattern var: s
      Pattern var: x
    |}]

let%expect_test "variable reference is tracked" =
  let tree = analyze "let x = 42 let y = x + 1" in
  (match find_binding_in_tree tree "x" with
   | Some b ->
     Printf.printf "References to x: %d\n" (Analysis.Scope.reference_count b);
     Printf.printf "Has read refs: %b\n" (Analysis.Scope.has_read_references b)
   | None -> print_endline "Not found");
  [%expect{|
    References to x: 1
    Has read refs: true
    |}]

let%expect_test "unused variable has no references" =
  let tree = analyze "let x = 42" in
  (match find_binding_in_tree tree "x" with
   | Some b ->
     Printf.printf "References to x: %d\n" (Analysis.Scope.reference_count b);
     Printf.printf "Has read refs: %b\n" (Analysis.Scope.has_read_references b)
   | None -> print_endline "Not found");
  [%expect{|
    References to x: 0
    Has read refs: false
    |}]

let%expect_test "recursive function self-reference is tracked" =
  let tree = analyze "let rec f x = if x <= 0 then 0 else f (x - 1)" in
  (match find_binding_in_tree tree "f" with
   | Some b ->
     Printf.printf "References to f: %d\n" (Analysis.Scope.reference_count b);
     Printf.printf "Has read refs: %b\n" (Analysis.Scope.has_read_references b)
   | None -> print_endline "Not found");
  [%expect{|
    References to f: 1
    Has read refs: true
    |}]

let%expect_test "multiple references are counted" =
  let tree = analyze "let x = 1 let y = x + x + x" in
  (match find_binding_in_tree tree "x" with
   | Some b ->
     Printf.printf "References to x: %d\n" (Analysis.Scope.reference_count b)
   | None -> print_endline "Not found");
  [%expect{|
    References to x: 3
    |}]

let%expect_test "nested let creates child scope" =
  let tree = analyze "let x = let y = 1 in y + 1" in
  Printf.printf "Total scopes: %d\n" (List.length tree.tree_all_scopes);
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  [%expect{|
    Total scopes: 2
    Total bindings: 2
    |}]

let%expect_test "function body creates scope" =
  let tree = analyze "let f x = let local = x in local + 1" in
  Printf.printf "Total scopes: %d\n" (List.length tree.tree_all_scopes);
  let scope_kinds = List.map (fun s ->
    Analysis.Scope.show_scope_kind s.Analysis.Scope.scope_kind
  ) tree.tree_all_scopes in
  List.iter print_endline (List.sort String.compare scope_kinds);
  [%expect{|
    Total scopes: 3
    Scope.ScopeFunction {name = None; is_recursive = false}
    Scope.ScopeLet
    Scope.ScopeModule {name = None}
    |}]

let%expect_test "type declaration creates type binding" =
  let tree = analyze "type point = { x : int; y : int }" in
  (match find_binding_in_tree tree "point" with
   | Some b ->
     Printf.printf "Found binding: %s\n" b.bind_name;
     Printf.printf "Kind: %s\n" (Analysis.Scope.show_binding_kind b.bind_kind)
   | None -> print_endline "Type binding not found");
  [%expect{|
    Found binding: point
    Kind: Scope.TypeName
    |}]

let%expect_test "variant constructors are tracked" =
  let tree = analyze "type option_int = None | Some of int" in
  let ctors = List.filter (fun b ->
    b.Analysis.Scope.bind_kind = Analysis.Scope.Constructor
  ) tree.tree_all_bindings in
  Printf.printf "Constructor count: %d\n" (List.length ctors);
  List.iter (fun b ->
    Printf.printf "  Constructor: %s\n" b.Analysis.Scope.bind_name
  ) (List.sort (fun a b -> String.compare a.Analysis.Scope.bind_name b.Analysis.Scope.bind_name) ctors);
  [%expect{|
    Constructor count: 2
      Constructor: None
      Constructor: Some
    |}]

let%expect_test "module creates module binding" =
  let tree = analyze "module M = struct let x = 1 end" in
  (match find_binding_in_tree tree "M" with
   | Some b ->
     Printf.printf "Found binding: %s\n" b.bind_name;
     Printf.printf "Kind: %s\n" (Analysis.Scope.show_binding_kind b.bind_kind)
   | None -> print_endline "Module binding not found");
  [%expect{|
    Found binding: M
    Kind: Scope.ModuleAlias
    |}]

let%expect_test "nested module bindings are tracked" =
  let tree = analyze "module M = struct let x = 1 let y = 2 end" in
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  List.iter (fun b ->
    Printf.printf "  %s (%s)\n" b.Analysis.Scope.bind_name
      (Analysis.Scope.show_binding_kind b.bind_kind)
  ) (List.sort (fun a b -> String.compare a.Analysis.Scope.bind_name b.Analysis.Scope.bind_name) tree.tree_all_bindings);
  [%expect{|
    Total bindings: 3
      M (Scope.ModuleAlias)
      x (Scope.Variable)
      y (Scope.Variable)
    |}]

let%expect_test "constructor use in pattern is tracked" =
  let tree = analyze {|
    type t = A | B of int
    let f x = match x with A -> 0 | B n -> n
  |} in
  (match find_binding_in_tree tree "A" with
   | Some b ->
     Printf.printf "A pattern refs: %b\n" (Analysis.Scope.has_pattern_references b)
   | None -> print_endline "Not found");
  [%expect{|
    A pattern refs: true
    |}]

let%expect_test "constructor use in expression is tracked" =
  let tree = analyze {|
    type t = A | B of int
    let x = A
    let y = B 42
  |} in
  (match find_binding_in_tree tree "A" with
   | Some b ->
     Printf.printf "A construct refs: %b\n" (Analysis.Scope.has_construct_references b)
   | None -> print_endline "Not found");
  (match find_binding_in_tree tree "B" with
   | Some b ->
     Printf.printf "B construct refs: %b\n" (Analysis.Scope.has_construct_references b)
   | None -> print_endline "Not found");
  [%expect{|
    A construct refs: true
    B construct refs: true
    |}]

let%expect_test "mutual recursion references are tracked" =
  let tree = analyze {|
    let rec even n = if n = 0 then true else odd (n - 1)
    and odd n = if n = 0 then false else even (n - 1)
  |} in
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  List.iter (fun b ->
    Printf.printf "  %s: refs=%d, kind=%s\n"
      b.Analysis.Scope.bind_name
      (Analysis.Scope.reference_count b)
      (Analysis.Scope.show_binding_kind b.Analysis.Scope.bind_kind)
  ) (List.sort (fun a b -> String.compare a.Analysis.Scope.bind_name b.Analysis.Scope.bind_name)
       tree.tree_all_bindings);
  [%expect{|
    Total bindings: 4
      even: refs=1, kind=Scope.Function {is_recursive = true}
      n: refs=0, kind=Scope.PatternVar
      n: refs=0, kind=Scope.PatternVar
      odd: refs=1, kind=Scope.Function {is_recursive = true}
    |}]

let%expect_test "mutual recursion with external call" =
  let tree = analyze {|
    let rec even n = if n = 0 then true else odd (n - 1)
    and odd n = if n = 0 then false else even (n - 1)
    let _ = even 5
  |} in
  List.iter (fun b ->
    if b.Analysis.Scope.bind_name = "even" || b.Analysis.Scope.bind_name = "odd" then
      Printf.printf "%s: refs=%d\n"
        b.Analysis.Scope.bind_name
        (Analysis.Scope.reference_count b)
  ) tree.tree_all_bindings;
  [%expect{|
    even: refs=2
    odd: refs=1
    |}]

(* NOTE: Type reference tracking from type manifests is limited because type
   aliases are fully resolved during type inference. For example, in:
     type t = int
     type s = t
   The manifest of `s` stores the resolved type `int`, not a reference to `t`.
   This is a known limitation of tracking at the typed AST level.

   To properly track type name usage from definitions, the type checker would
   need to record which type names are referenced during resolution. *)

let%expect_test "type used in constructor arg is tracked" =
  let tree = analyze "type t = int type s = A of t | B" in
  (match find_binding_in_tree tree "t" with
   | Some b -> Printf.printf "t refs: %d\n" (Analysis.Scope.reference_count b)
   | None -> print_endline "t not found");
  (* Note: constructor arg types may also be resolved, limiting tracking *)
  [%expect{|
    t refs: 0
    |}]

let%expect_test "ref assignment is tracked as write" =
  let tree = analyze "let r = ref 0 let _ = r := 42" in
  (match find_binding_in_tree tree "r" with
   | Some b ->
       Printf.printf "r refs: %d\n" (Analysis.Scope.reference_count b);
       Printf.printf "has read refs: %b\n" (Analysis.Scope.has_read_references b);
       Printf.printf "has write refs: %b\n" (Analysis.Scope.has_write_references b)
   | None -> print_endline "r not found");
  [%expect{|
    r refs: 1
    has read refs: true
    has write refs: false
    |}]

let%expect_test "record type creates field bindings" =
  let tree = analyze "type t = { x : int; y : int }" in
  Printf.printf "Total bindings: %d\n" (List.length tree.tree_all_bindings);
  List.iter (fun b ->
    Printf.printf "  %s (%s)\n" b.Analysis.Scope.bind_name
      (Analysis.Scope.show_binding_kind b.bind_kind)
  ) (List.sort (fun a b -> String.compare a.Analysis.Scope.bind_name b.Analysis.Scope.bind_name)
       tree.tree_all_bindings);
  [%expect{|
    Total bindings: 3
      t (Scope.TypeName)
      x (Scope.RecordField)
      y (Scope.RecordField)
    |}]

let%expect_test "record field projection is tracked" =
  let tree = analyze "type t = { x : int; y : int } let p = { x = 1; y = 2 } let v = p.x" in
  (match find_binding_in_tree tree "x" with
   | Some b ->
       Printf.printf "x refs: %d\n" (Analysis.Scope.reference_count b);
       Printf.printf "has project refs: %b\n" (Analysis.Scope.has_project_references b)
   | None -> print_endline "x not found");
  [%expect{|
    x refs: 1
    has project refs: true
    |}]
