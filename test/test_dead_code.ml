(** Tests for dead code detection.

    Tests the dead_code module which finds unreachable code after
    diverging expressions (raise, failwith, exit, etc.).

    Note: Lina doesn't have built-in raise/failwith/exit, so these tests
    define those functions to test the detection mechanism.

    Dead code detection in Lina works for sequences (e1; e2), not for
    let bindings. Use semicolons to test sequence-based dead code. *)

let detect_dead_code source =
  let typed_ast = Analysis_test_helpers.type_check_with_prelude source in
  Analysis.Dead_code.detect_in_structure typed_ast

let finding_reasons findings =
  List.map (fun (f : Analysis.Dead_code.finding) -> f.reason) findings

let%expect_test "code after raise in sequence is dead" =
  let findings = detect_dead_code {|
    let f x =
      raise "error";
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  List.iter print_endline (finding_reasons findings);
  [%expect{|
    Finding count: 1
    code after diverging expression is unreachable
    |}]

let%expect_test "code before raise in sequence is alive" =
  let findings = detect_dead_code {|
    let f x =
      let y = x + 1 in
      raise "error";
      y
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "no dead code without raise" =
  let findings = detect_dead_code {|
    let f x =
      let y = x + 1 in
      y + 2
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "code after failwith in sequence is dead" =
  let findings = detect_dead_code {|
    let f x =
      failwith "error";
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  List.iter print_endline (finding_reasons findings);
  [%expect{|
    Finding count: 1
    code after diverging expression is unreachable
    |}]

let%expect_test "code after invalid_arg in sequence is dead" =
  let findings = detect_dead_code {|
    let f x =
      invalid_arg "bad arg";
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "code after exit in sequence is dead" =
  let findings = detect_dead_code {|
    let f x =
      exit 1;
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "both branches diverge makes code after if dead" =
  let findings = detect_dead_code {|
    let f x =
      (if x > 0 then raise "pos" else failwith "neg");
      42
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "one branch diverges does not make if dead" =
  let findings = detect_dead_code {|
    let f x =
      (if x > 0 then raise "error" else ());
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "all match branches diverge makes code after match dead" =
  let findings = detect_dead_code {|
    type t = A | B
    let f x =
      (match x with
        | A -> raise "A"
        | B -> failwith "B");
      42
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "one non-diverging match branch is fine" =
  let findings = detect_dead_code {|
    type t = A | B
    let f x =
      let y = match x with
        | A -> raise "error"
        | B -> 0
      in
      y + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "dead code in nested function" =
  let findings = detect_dead_code {|
    let outer x =
      let inner y =
        raise "error";
        y + 1
      in
      inner x
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "dead code in module" =
  let findings = detect_dead_code {|
    module M = struct
      let f x =
        failwith "error";
        x + 1
    end
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]

let%expect_test "multiple dead code locations" =
  let findings = detect_dead_code {|
    let f x =
      raise "error";
      x + 1
    let g y =
      failwith "oops";
      y * 2
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 2
    |}]

let%expect_test "finding converts to diagnostic" =
  let findings = detect_dead_code {|
    let f x =
      raise "error";
      x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  (match findings with
   | finding :: _ ->
       let diag =
         Analysis.Dead_code.to_diagnostic
           Analysis_test_helpers.Warning_config.default finding
       in
       Printf.printf "Message: %s\n" diag.Analysis_test_helpers.Compiler_error.message;
       Printf.printf "Has code: %b\n" (Option.is_some diag.code);
       (match diag.code with
        | Some c ->
            Printf.printf "Code: %s\n"
              (Analysis_test_helpers.Error_code.to_string c)
        | None -> ());
       Printf.printf "Has suggestion: %b\n" (List.length diag.suggestions > 0)
   | [] -> print_endline "No findings");
  [%expect {|
    Finding count: 1
    Message: code after diverging expression is unreachable
    Has code: true
    Code: W0013
    Has suggestion: true
    |}]

let%expect_test "normal function has no dead code" =
  let findings = detect_dead_code {|
    let add x y = x + y
    let mul x y = x * y
    let result = add 1 (mul 2 3)
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "complex control flow has no dead code" =
  let findings = detect_dead_code {|
    let classify x =
      if x > 0 then
        if x > 100 then "large" else "medium"
      else if x < 0 then
        "negative"
      else
        "zero"
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "recursive function has no dead code" =
  let findings = detect_dead_code {|
    let rec sum n =
      if n <= 0 then 0
      else n + sum (n - 1)
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "raise as expression result is not dead code" =
  let findings = detect_dead_code {|
    let fail () = raise "error"
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "raise in match arm body is not dead code" =
  let findings = detect_dead_code {|
    type t = A | B
    let f x = match x with
      | A -> 42
      | B -> raise "error"
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "raise in if then is not dead code" =
  let findings = detect_dead_code {|
    let check x =
      if x < 0 then raise "negative"
      else x + 1
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 0
    |}]

let%expect_test "chained sequences with raise" =
  let findings = detect_dead_code {|
    let f () =
      print "a";
      raise "error";
      print "b";
      print "c"
  |} in
  Printf.printf "Finding count: %d\n" (List.length findings);
  [%expect{|
    Finding count: 1
    |}]
