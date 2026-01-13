(** Tests for LSP type conversions. *)

open Lina_lsp

(* ============================================================ *)
(* Position Conversion Tests *)
(* ============================================================ *)

let%expect_test "position_of_lina_position - converts 1-indexed to 0-indexed" =
  let lina_pos : Common.Location.position =
    { filename = "test.lina"; line = 1; column = 1; offset = 0 }
  in
  let lsp_pos = Lsp_types.position_of_lina_position lina_pos in
  print_endline (Lsp_types.show_position lsp_pos);
  [%expect {| { Lsp_types.line = 0; character = 1 } |}]

let%expect_test "position_of_lina_position - handles line 5 column 10" =
  let lina_pos : Common.Location.position =
    { filename = "test.lina"; line = 5; column = 10; offset = 50 }
  in
  let lsp_pos = Lsp_types.position_of_lina_position lina_pos in
  print_endline (Lsp_types.show_position lsp_pos);
  [%expect {| { Lsp_types.line = 4; character = 10 } |}]

let%expect_test "range_of_location - converts location span" =
  let start_pos : Common.Location.position =
    { filename = "test.lina"; line = 1; column = 5; offset = 4 }
  in
  let end_pos : Common.Location.position =
    { filename = "test.lina"; line = 1; column = 10; offset = 9 }
  in
  let loc : Common.Location.t = { start_pos; end_pos } in
  let range = Lsp_types.range_of_location loc in
  print_endline (Lsp_types.show_range range);
  [%expect {|
    { Lsp_types.start_pos = { Lsp_types.line = 0; character = 5 };
      end_pos = { Lsp_types.line = 0; character = 10 } }
    |}]

let%expect_test "range_of_location - multi-line range" =
  let start_pos : Common.Location.position =
    { filename = "test.lina"; line = 1; column = 1; offset = 0 }
  in
  let end_pos : Common.Location.position =
    { filename = "test.lina"; line = 3; column = 5; offset = 25 }
  in
  let loc : Common.Location.t = { start_pos; end_pos } in
  let range = Lsp_types.range_of_location loc in
  print_endline (Lsp_types.show_range range);
  [%expect {|
    { Lsp_types.start_pos = { Lsp_types.line = 0; character = 1 };
      end_pos = { Lsp_types.line = 2; character = 5 } }
    |}]

let%expect_test "lina_position_of_position - reverse conversion" =
  let lsp_pos : Lsp_types.position = { line = 0; character = 0 } in
  let lina_pos = Lsp_types.lina_position_of_position "test.lina" lsp_pos 0 in
  Printf.printf "line=%d column=%d" lina_pos.line lina_pos.column;
  [%expect {| line=1 column=0 |}]

let%expect_test "lina_position_of_position - with offset" =
  let lsp_pos : Lsp_types.position = { line = 2; character = 5 } in
  let lina_pos = Lsp_types.lina_position_of_position "test.lina" lsp_pos 25 in
  Printf.printf "line=%d column=%d offset=%d" lina_pos.line lina_pos.column lina_pos.offset;
  [%expect {| line=3 column=5 offset=25 |}]

(* ============================================================ *)
(* Severity Conversion Tests *)
(* ============================================================ *)

let%expect_test "severity_to_int - all severities" =
  Printf.printf "Error=%d Warning=%d Info=%d Hint=%d"
    (Lsp_types.severity_to_int Lsp_types.Error)
    (Lsp_types.severity_to_int Lsp_types.Warning)
    (Lsp_types.severity_to_int Lsp_types.Information)
    (Lsp_types.severity_to_int Lsp_types.Hint);
  [%expect {| Error=1 Warning=2 Info=3 Hint=4 |}]

let%expect_test "severity_of_int - all codes" =
  let show sev =
    match sev with
    | Lsp_types.Error -> "Error"
    | Lsp_types.Warning -> "Warning"
    | Lsp_types.Information -> "Info"
    | Lsp_types.Hint -> "Hint"
  in
  Printf.printf "1=%s 2=%s 3=%s 4=%s"
    (show (Lsp_types.severity_of_int 1))
    (show (Lsp_types.severity_of_int 2))
    (show (Lsp_types.severity_of_int 3))
    (show (Lsp_types.severity_of_int 4));
  [%expect {| 1=Error 2=Warning 3=Info 4=Hint |}]

let%expect_test "severity_of_int - unknown defaults to Information" =
  let show sev =
    match sev with
    | Lsp_types.Information -> "Info"
    | _ -> "Other"
  in
  Printf.printf "0=%s 5=%s 99=%s"
    (show (Lsp_types.severity_of_int 0))
    (show (Lsp_types.severity_of_int 5))
    (show (Lsp_types.severity_of_int 99));
  [%expect {| 0=Info 5=Info 99=Info |}]

(* ============================================================ *)
(* Diagnostic Creation Tests *)
(* ============================================================ *)

let%expect_test "make_diagnostic - basic diagnostic" =
  let loc : Common.Location.t =
    {
      start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
      end_pos = { filename = "test.lina"; line = 1; column = 10; offset = 9 };
    }
  in
  let diag = Lsp_types.make_diagnostic ~severity:Error ~message:"Test error" loc in
  Printf.printf "severity=%s source=%s message=%s"
    (Lsp_types.show_diagnostic_severity diag.severity)
    (Option.value ~default:"none" diag.source)
    diag.message;
  [%expect {| severity=Lsp_types.Error source=lina message=Test error |}]

let%expect_test "make_diagnostic - with code" =
  let loc : Common.Location.t =
    {
      start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
      end_pos = { filename = "test.lina"; line = 1; column = 10; offset = 9 };
    }
  in
  let diag = Lsp_types.make_diagnostic ~severity:Warning ~message:"Test" ~code:"W001" loc in
  Printf.printf "code=%s" (Option.value ~default:"none" diag.code);
  [%expect {| code=W001 |}]

let%expect_test "make_diagnostic - with hints" =
  let loc : Common.Location.t =
    {
      start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
      end_pos = { filename = "test.lina"; line = 1; column = 10; offset = 9 };
    }
  in
  let diag = Lsp_types.make_diagnostic
    ~severity:Error
    ~message:"Type error"
    ~hints:["Did you mean 'int'?"; "Check the function signature"]
    loc
  in
  print_endline diag.message;
  [%expect {|
    Type error
    Hint: Did you mean 'int'?
    Hint: Check the function signature
    |}]

(* ============================================================ *)
(* Compiler Error Conversion Tests *)
(* ============================================================ *)

let%expect_test "diagnostic_of_compiler_error - lexer error" =
  let err : Common.Compiler_error.t =
    {
      location = {
        start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
        end_pos = { filename = "test.lina"; line = 1; column = 5; offset = 4 };
      };
      kind = Common.Compiler_error.LexerError "Invalid character";
      hints = [];
    }
  in
  let diag = Lsp_types.diagnostic_of_compiler_error err in
  Printf.printf "severity=%s code=%s"
    (match diag.severity with Lsp_types.Error -> "Error" | _ -> "Other")
    (Option.value ~default:"none" diag.code);
  [%expect {| severity=Error code=E0012 |}]

let%expect_test "diagnostic_of_compiler_error - parser error" =
  let err : Common.Compiler_error.t =
    {
      location = {
        start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
        end_pos = { filename = "test.lina"; line = 1; column = 5; offset = 4 };
      };
      kind = Common.Compiler_error.ParserError "Unexpected token";
      hints = [];
    }
  in
  let diag = Lsp_types.diagnostic_of_compiler_error err in
  Printf.printf "code=%s message=%s"
    (Option.value ~default:"none" diag.code)
    diag.message;
  [%expect {| code=E0011 message=Unexpected token |}]

let%expect_test "diagnostic_of_compiler_error - type error" =
  let err : Common.Compiler_error.t =
    {
      location = {
        start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
        end_pos = { filename = "test.lina"; line = 1; column = 5; offset = 4 };
      };
      kind = Common.Compiler_error.TypeError "Type mismatch";
      hints = ["Expected int, got string"];
    }
  in
  let diag = Lsp_types.diagnostic_of_compiler_error err in
  Printf.printf "code=%s\n" (Option.value ~default:"none" diag.code);
  print_endline diag.message;
  [%expect {|
    code=E0001
    Type mismatch
    Hint: Expected int, got string
    |}]

let%expect_test "diagnostic_of_compiler_error - internal error" =
  let err : Common.Compiler_error.t =
    {
      location = {
        start_pos = { filename = "test.lina"; line = 1; column = 1; offset = 0 };
        end_pos = { filename = "test.lina"; line = 1; column = 5; offset = 4 };
      };
      kind = Common.Compiler_error.InternalError "Assertion failed";
      hints = [];
    }
  in
  let diag = Lsp_types.diagnostic_of_compiler_error err in
  Printf.printf "code=%s" (Option.value ~default:"none" diag.code);
  [%expect {| code=internal |}]
