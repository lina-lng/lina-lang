(** Tests for warning configuration including path-specific overrides.

    Tests the config module's glob matching and path-override functionality. *)

module Warning_config = Common.Warning_config
module Error_code = Common.Error_code

(* ==================== Glob Matching ==================== *)

let%expect_test "exact path match" =
  Printf.printf "%b\n" (Driver.Config.glob_match "foo.lina" "foo.lina");
  [%expect{| true |}]

let%expect_test "exact path no match" =
  Printf.printf "%b\n" (Driver.Config.glob_match "foo.lina" "bar.lina");
  [%expect{| false |}]

let%expect_test "single star matches one component" =
  Printf.printf "%b\n" (Driver.Config.glob_match "src/*" "src/main.lina");
  [%expect{| true |}]

let%expect_test "single star no match nested" =
  Printf.printf "%b\n" (Driver.Config.glob_match "src/*" "src/sub/main.lina");
  [%expect{| false |}]

let%expect_test "double star matches any depth" =
  Printf.printf "%b\n" (Driver.Config.glob_match "test/**" "test/unit/foo.lina");
  [%expect{| true |}]

let%expect_test "double star matches direct child" =
  Printf.printf "%b\n" (Driver.Config.glob_match "test/**" "test/foo.lina");
  [%expect{| true |}]

let%expect_test "double star matches deeply nested" =
  Printf.printf "%b\n" (Driver.Config.glob_match "test/**" "test/a/b/c/d.lina");
  [%expect{| true |}]

let%expect_test "double star at end matches directory itself" =
  Printf.printf "%b\n" (Driver.Config.glob_match "test/**" "test/");
  [%expect{| true |}]

let%expect_test "double star no match different root" =
  Printf.printf "%b\n" (Driver.Config.glob_match "test/**" "src/foo.lina");
  [%expect{| false |}]

let%expect_test "combined star and double star" =
  Printf.printf "%b\n" (Driver.Config.glob_match "src/**/tests" "src/sub/tests");
  [%expect{| true |}]

let%expect_test "double star in middle" =
  Printf.printf "%b\n" (Driver.Config.glob_match "a/**/b" "a/x/y/z/b");
  [%expect{| true |}]

let%expect_test "double star empty middle" =
  Printf.printf "%b\n" (Driver.Config.glob_match "a/**/b" "a/b");
  [%expect{| true |}]

(* ==================== Default Config ==================== *)

let%expect_test "default config is strict" =
  let config = Driver.Config.default_warnings in
  Printf.printf "Preset: %s\n"
    (match config.preset with
     | Driver.Config.Strict -> "strict"
     | Driver.Config.Relaxed -> "relaxed");
  [%expect{| Preset: strict |}]

let%expect_test "strict preset makes unused variable an error" =
  let config = Driver.Config.default_warnings in
  let wconfig = Driver.Config.to_warning_config config in
  let is_error = Warning_config.is_error wconfig Error_code.w_unused_variable in
  Printf.printf "Unused variable is error: %b\n" is_error;
  [%expect{| Unused variable is error: true |}]

let%expect_test "strict preset makes dead code an error" =
  let config = Driver.Config.default_warnings in
  let wconfig = Driver.Config.to_warning_config config in
  let is_error = Warning_config.is_error wconfig Error_code.w_dead_code in
  Printf.printf "Dead code is error: %b\n" is_error;
  [%expect{| Dead code is error: true |}]

(* ==================== Warning Level Overrides ==================== *)

let%expect_test "override changes warning level" =
  let config = {
    Driver.Config.default_warnings with
    overrides = [("unused-variable", Warning_config.Warn)]
  } in
  let wconfig = Driver.Config.to_warning_config config in
  let is_error = Warning_config.is_error wconfig Error_code.w_unused_variable in
  Printf.printf "Unused variable is error after override: %b\n" is_error;
  [%expect{| Unused variable is error after override: false |}]

let%expect_test "allow level disables warning" =
  let config = {
    Driver.Config.default_warnings with
    overrides = [("unused-variable", Warning_config.Allow)]
  } in
  let wconfig = Driver.Config.to_warning_config config in
  let should_report = Warning_config.should_report wconfig Error_code.w_unused_variable in
  Printf.printf "Should report unused variable: %b\n" should_report;
  [%expect{| Should report unused variable: false |}]

let%expect_test "deny level is like error but overridable" =
  let config = {
    Driver.Config.default_warnings with
    preset = Driver.Config.Relaxed;
    overrides = [("unused-variable", Warning_config.Deny)]
  } in
  let wconfig = Driver.Config.to_warning_config config in
  let is_error = Warning_config.is_error wconfig Error_code.w_unused_variable in
  Printf.printf "Unused variable is error with Deny: %b\n" is_error;
  [%expect{| Unused variable is error with Deny: true |}]

(* ==================== Path-Specific Overrides ==================== *)

let%expect_test "path override applies to matching files" =
  let config = {
    Driver.Config.default_warnings with
    by_path = [{
      path_pattern = "test/**";
      path_preset = Some Driver.Config.Relaxed;
      path_overrides = [];
    }]
  } in
  let wconfig = Driver.Config.warning_config_for_file config "test/unit/foo.lina" in
  let is_error = Warning_config.is_error wconfig Error_code.w_unused_variable in
  Printf.printf "Unused variable is error in test/: %b\n" is_error;
  [%expect{| Unused variable is error in test/: false |}]

let%expect_test "path override does not apply to non-matching files" =
  let config = {
    Driver.Config.default_warnings with
    by_path = [{
      path_pattern = "test/**";
      path_preset = Some Driver.Config.Relaxed;
      path_overrides = [];
    }]
  } in
  let wconfig = Driver.Config.warning_config_for_file config "src/main.lina" in
  let is_error = Warning_config.is_error wconfig Error_code.w_unused_variable in
  Printf.printf "Unused variable is error in src/: %b\n" is_error;
  [%expect{| Unused variable is error in src/: true |}]

let%expect_test "path-specific warning override" =
  let config = {
    Driver.Config.default_warnings with
    by_path = [{
      path_pattern = "generated/**";
      path_preset = None;
      path_overrides = [
        ("unused-variable", Warning_config.Allow);
        ("dead-code", Warning_config.Allow);
      ];
    }]
  } in
  let wconfig = Driver.Config.warning_config_for_file config "generated/types.lina" in
  let should_report_unused = Warning_config.should_report wconfig Error_code.w_unused_variable in
  let should_report_dead = Warning_config.should_report wconfig Error_code.w_dead_code in
  Printf.printf "Report unused in generated/: %b\n" should_report_unused;
  Printf.printf "Report dead code in generated/: %b\n" should_report_dead;
  [%expect{|
    Report unused in generated/: false
    Report dead code in generated/: false
    |}]

let%expect_test "multiple path overrides first match wins" =
  let config = {
    Driver.Config.default_warnings with
    by_path = [
      {
        path_pattern = "test/special/**";
        path_preset = Some Driver.Config.Strict;
        path_overrides = [];
      };
      {
        path_pattern = "test/**";
        path_preset = Some Driver.Config.Relaxed;
        path_overrides = [];
      };
    ]
  } in
  let wconfig_special = Driver.Config.warning_config_for_file config "test/special/foo.lina" in
  let wconfig_regular = Driver.Config.warning_config_for_file config "test/unit/foo.lina" in
  let special_is_error = Warning_config.is_error wconfig_special Error_code.w_unused_variable in
  let regular_is_error = Warning_config.is_error wconfig_regular Error_code.w_unused_variable in
  Printf.printf "test/special/ is error: %b\n" special_is_error;
  Printf.printf "test/unit/ is error: %b\n" regular_is_error;
  [%expect{|
    test/special/ is error: true
    test/unit/ is error: false
    |}]

(* ==================== Warning_config.t Operations ==================== *)

let%expect_test "disable_all disables all warnings" =
  let config = Warning_config.disable_all Warning_config.default in
  Printf.printf "Unused: %b\n" (Warning_config.should_report config Error_code.w_unused_variable);
  Printf.printf "Dead: %b\n" (Warning_config.should_report config Error_code.w_dead_code);
  [%expect{|
    Unused: false
    Dead: false
    |}]

let%expect_test "default (strict) enables all as errors" =
  let config = Warning_config.default in
  Printf.printf "Unused is error: %b\n" (Warning_config.is_error config Error_code.w_unused_variable);
  Printf.printf "Function is error: %b\n" (Warning_config.is_error config Error_code.w_unused_function);
  Printf.printf "Dead is error: %b\n" (Warning_config.is_error config Error_code.w_dead_code);
  [%expect{|
    Unused is error: true
    Function is error: true
    Dead is error: true
    |}]

let%expect_test "set_level changes individual warning" =
  let config = Warning_config.set_level Warning_config.default Error_code.w_unused_variable Warning_config.Allow in
  Printf.printf "Report unused: %b\n" (Warning_config.should_report config Error_code.w_unused_variable);
  Printf.printf "Report function: %b\n" (Warning_config.should_report config Error_code.w_unused_function);
  [%expect{|
    Report unused: false
    Report function: true
    |}]

(* ==================== Relaxed Preset ==================== *)

let%expect_test "relaxed preset makes unused code warnings not errors" =
  let config = {
    Driver.Config.default_warnings with
    preset = Driver.Config.Relaxed
  } in
  let wconfig = Driver.Config.to_warning_config config in
  Printf.printf "Unused variable is error: %b\n"
    (Warning_config.is_error wconfig Error_code.w_unused_variable);
  Printf.printf "Unused function is error: %b\n"
    (Warning_config.is_error wconfig Error_code.w_unused_function);
  Printf.printf "Dead code is error: %b\n"
    (Warning_config.is_error wconfig Error_code.w_dead_code);
  [%expect{|
    Unused variable is error: false
    Unused function is error: false
    Dead code is error: false
    |}]

let%expect_test "relaxed preset still reports as warnings" =
  let config = {
    Driver.Config.default_warnings with
    preset = Driver.Config.Relaxed
  } in
  let wconfig = Driver.Config.to_warning_config config in
  Printf.printf "Report unused variable: %b\n"
    (Warning_config.should_report wconfig Error_code.w_unused_variable);
  Printf.printf "Report dead code: %b\n"
    (Warning_config.should_report wconfig Error_code.w_dead_code);
  [%expect{|
    Report unused variable: true
    Report dead code: true
    |}]

(* ==================== Error_code Mapping ==================== *)

let%expect_test "warning codes map correctly" =
  Printf.printf "unused-variable = %s\n" (Error_code.to_string Error_code.w_unused_variable);
  Printf.printf "unused-function = %s\n" (Error_code.to_string Error_code.w_unused_function);
  Printf.printf "unused-parameter = %s\n" (Error_code.to_string Error_code.w_unused_parameter);
  Printf.printf "dead-code = %s\n" (Error_code.to_string Error_code.w_dead_code);
  [%expect{|
    unused-variable = W0001
    unused-function = W0005
    unused-parameter = W0006
    dead-code = W0013
    |}]

let%expect_test "warning descriptions exist" =
  Printf.printf "unused-variable: %s\n" (Error_code.description Error_code.w_unused_variable);
  Printf.printf "unused-function: %s\n" (Error_code.description Error_code.w_unused_function);
  Printf.printf "dead-code: %s\n" (Error_code.description Error_code.w_dead_code);
  [%expect{|
    unused-variable: unused variable
    unused-function: unused function
    dead-code: unreachable code
    |}]
