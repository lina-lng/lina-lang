(** Unit tests for the package management system. *)

open Package

(** {1 Version Tests} *)

let%expect_test "version parsing - simple" =
  let result = Version.parse "1.2.3" in
  print_endline (match result with
    | Ok v -> Version.to_string v
    | Error msg -> "Error: " ^ msg);
  [%expect {| 1.2.3 |}]

let%expect_test "version parsing - with revision" =
  let result = Version.parse "1.0.2-1" in
  print_endline (match result with
    | Ok v -> Version.to_string v
    | Error msg -> "Error: " ^ msg);
  [%expect {| 1.0.2-1 |}]

let%expect_test "version parsing - invalid" =
  let result = Version.parse "invalid" in
  print_endline (match result with
    | Ok v -> Version.to_string v
    | Error _ -> "Error: parse failed");
  [%expect {| Error: parse failed |}]

let%expect_test "version compare - equal" =
  let v1 = Version.make 1 2 3 in
  let v2 = Version.make 1 2 3 in
  print_endline (string_of_int (Version.compare v1 v2));
  [%expect {| 0 |}]

let%expect_test "version compare - less than" =
  let v1 = Version.make 1 2 3 in
  let v2 = Version.make 1 2 4 in
  let cmp = Version.compare v1 v2 in
  print_endline (if cmp < 0 then "less" else "not less");
  [%expect {| less |}]

let%expect_test "version compare - greater than" =
  let v1 = Version.make 2 0 0 in
  let v2 = Version.make 1 9 9 in
  let cmp = Version.compare v1 v2 in
  print_endline (if cmp > 0 then "greater" else "not greater");
  [%expect {| greater |}]

(** {1 Version Constraint Tests} *)

let%expect_test "constraint - caret" =
  let c = Version.parse_constraint "^1.2.3" in
  print_endline (match c with
    | Ok c -> Version.constraint_to_string c
    | Error msg -> "Error: " ^ msg);
  [%expect {| ^1.2.3 |}]

let%expect_test "constraint - exact" =
  let c = Version.parse_constraint "1.2.3" in
  print_endline (match c with
    | Ok c -> Version.constraint_to_string c
    | Error msg -> "Error: " ^ msg);
  [%expect {| 1.2.3 |}]

let%expect_test "constraint - greater equal" =
  let c = Version.parse_constraint ">=1.0.0" in
  print_endline (match c with
    | Ok c -> Version.constraint_to_string c
    | Error msg -> "Error: " ^ msg);
  [%expect {| >=1.0.0 |}]

let%expect_test "constraint - any" =
  let c = Version.parse_constraint "*" in
  print_endline (match c with
    | Ok c -> Version.constraint_to_string c
    | Error msg -> "Error: " ^ msg);
  [%expect {| * |}]

let%expect_test "satisfies - caret basic" =
  let v = Version.make 1 5 0 in
  (match Version.parse_constraint "^1.2.0" with
   | Ok c -> print_endline (string_of_bool (Version.satisfies v c))
   | Error _ -> print_endline "parse error");
  [%expect {| true |}]

let%expect_test "satisfies - caret reject major" =
  let v = Version.make 2 0 0 in
  (match Version.parse_constraint "^1.2.0" with
   | Ok c -> print_endline (string_of_bool (Version.satisfies v c))
   | Error _ -> print_endline "parse error");
  [%expect {| false |}]

let%expect_test "satisfies - any" =
  let v = Version.make 99 99 99 in
  (match Version.parse_constraint "*" with
   | Ok c -> print_endline (string_of_bool (Version.satisfies v c))
   | Error _ -> print_endline "parse error");
  [%expect {| true |}]

let%expect_test "satisfies - exact match" =
  let v = Version.make 1 2 3 in
  (match Version.parse_constraint "1.2.3" with
   | Ok c -> print_endline (string_of_bool (Version.satisfies v c))
   | Error _ -> print_endline "parse error");
  [%expect {| true |}]

let%expect_test "satisfies - exact no match" =
  let v = Version.make 1 2 4 in
  (match Version.parse_constraint "1.2.3" with
   | Ok c -> print_endline (string_of_bool (Version.satisfies v c))
   | Error _ -> print_endline "parse error");
  [%expect {| false |}]

(** {1 Lockfile Tests} *)

let%expect_test "lockfile - empty" =
  let lock = Lockfile.empty in
  print_endline (string_of_int lock.lockfile_version);
  print_endline (string_of_int (List.length lock.packages));
  [%expect {|
    1
    0 |}]

let%expect_test "lockfile - add and find" =
  let entry = Lockfile.{ name = "test"; version = "1.0.0"; checksum = "abc123" } in
  let lock = Lockfile.add_package entry Lockfile.empty in
  (match Lockfile.find_package "test" lock with
   | Some e -> print_endline (e.name ^ "@" ^ e.version)
   | None -> print_endline "not found");
  [%expect {| test@1.0.0 |}]

let%expect_test "lockfile - has_package" =
  let entry = Lockfile.{ name = "test"; version = "1.0.0"; checksum = "abc123" } in
  let lock = Lockfile.add_package entry Lockfile.empty in
  print_endline (string_of_bool (Lockfile.has_package "test" lock));
  print_endline (string_of_bool (Lockfile.has_package "other" lock));
  [%expect {|
    true
    false |}]

let%expect_test "lockfile - remove" =
  let entry = Lockfile.{ name = "test"; version = "1.0.0"; checksum = "abc123" } in
  let lock = Lockfile.add_package entry Lockfile.empty in
  let lock = Lockfile.remove_package "test" lock in
  print_endline (string_of_bool (Lockfile.has_package "test" lock));
  [%expect {| false |}]

(** {1 Binding Generator Tests} *)

let%expect_test "binding module name - strip lua prefix" =
  print_endline (Binding_generator.binding_module_name "luasocket");
  [%expect {| Socket |}]

let%expect_test "binding module name - no prefix" =
  print_endline (Binding_generator.binding_module_name "lpeg");
  [%expect {| Lpeg |}]

let%expect_test "binding module name - capitalize" =
  print_endline (Binding_generator.binding_module_name "penlight");
  [%expect {| Penlight |}]

let%expect_test "binding filename" =
  print_endline (Binding_generator.binding_filename "luasocket");
  [%expect {| Socket.lina |}]

let%expect_test "has template - lpeg" =
  print_endline (string_of_bool (Binding_generator.has_template "lpeg"));
  [%expect {| true |}]

let%expect_test "has template - unknown" =
  print_endline (string_of_bool (Binding_generator.has_template "unknownpkg"));
  [%expect {| false |}]

(** {1 Types Tests} *)

let%expect_test "empty dependencies" =
  let deps = Types.empty_dependencies in
  print_endline (string_of_int (List.length deps.luarocks));
  print_endline (string_of_int (List.length deps.dev_luarocks));
  [%expect {|
    0
    0 |}]

(** {1 Installer Tests} *)

let%expect_test "installer paths" =
  let root = "/tmp/test-project" in
  print_endline (Installer.lina_dir root);
  print_endline (Installer.bindings_dir root);
  print_endline (Installer.lockfile_path root);
  [%expect {|
    /tmp/test-project/.lina
    /tmp/test-project/.lina/bindings
    /tmp/test-project/lina.lock |}]

(** {1 Project Dependency Tests} *)

(* Test topological sort of project dependencies *)
let%expect_test "topological_sort_deps - simple order" =
  (* Simulate: app depends on core; core has no deps *)
  let deps = [
    ("core", Ok (
      Package.Types.{ lina_name = "core"; lina_path = "/core"; lina_optional = false },
      "/core",
      []
    ));
    ("app", Ok (
      Package.Types.{ lina_name = "app"; lina_path = "/app"; lina_optional = false },
      "/app",
      ["core"]
    ));
  ] in
  (match Driver.Project_loader.topological_sort_deps deps with
   | Ok sorted -> List.iter (fun (name, _, _) -> print_endline name) sorted
   | Error msg -> print_endline ("Error: " ^ msg));
  [%expect {|
    core
    app |}]

let%expect_test "topological_sort_deps - transitive deps" =
  (* Simulate: app -> math -> core *)
  let deps = [
    ("core", Ok (
      Package.Types.{ lina_name = "core"; lina_path = "/core"; lina_optional = false },
      "/core",
      []
    ));
    ("math", Ok (
      Package.Types.{ lina_name = "math"; lina_path = "/math"; lina_optional = false },
      "/math",
      ["core"]
    ));
    ("app", Ok (
      Package.Types.{ lina_name = "app"; lina_path = "/app"; lina_optional = false },
      "/app",
      ["math"]
    ));
  ] in
  (match Driver.Project_loader.topological_sort_deps deps with
   | Ok sorted -> List.iter (fun (name, _, _) -> print_endline name) sorted
   | Error msg -> print_endline ("Error: " ^ msg));
  [%expect {|
    core
    math
    app |}]

let%expect_test "topological_sort_deps - diamond dependency" =
  (* app -> math, app -> collections; both math and collections -> core *)
  let deps = [
    ("core", Ok (
      Package.Types.{ lina_name = "core"; lina_path = "/core"; lina_optional = false },
      "/core",
      []
    ));
    ("math", Ok (
      Package.Types.{ lina_name = "math"; lina_path = "/math"; lina_optional = false },
      "/math",
      ["core"]
    ));
    ("collections", Ok (
      Package.Types.{ lina_name = "collections"; lina_path = "/collections"; lina_optional = false },
      "/collections",
      ["core"]
    ));
    ("app", Ok (
      Package.Types.{ lina_name = "app"; lina_path = "/app"; lina_optional = false },
      "/app",
      ["math"; "collections"]
    ));
  ] in
  (match Driver.Project_loader.topological_sort_deps deps with
   | Ok sorted -> List.iter (fun (name, _, _) -> print_endline name) sorted
   | Error msg -> print_endline ("Error: " ^ msg));
  (* core must come first, then math and collections in some order, then app *)
  [%expect {|
    core
    math
    collections
    app |}]

let%expect_test "topological_sort_deps - cyclic dependency" =
  (* a -> b -> a creates cycle *)
  let deps = [
    ("a", Ok (
      Package.Types.{ lina_name = "a"; lina_path = "/a"; lina_optional = false },
      "/a",
      ["b"]
    ));
    ("b", Ok (
      Package.Types.{ lina_name = "b"; lina_path = "/b"; lina_optional = false },
      "/b",
      ["a"]
    ));
  ] in
  match Driver.Project_loader.topological_sort_deps deps with
  | Ok _ -> print_endline "should have failed"
  | Error msg -> print_endline (if String.sub msg 0 6 = "Cyclic" then "Cyclic detected" else msg);
  [%expect {| Cyclic detected |}]

let%expect_test "topological_sort_deps - reverse order input" =
  (* Input is reverse of expected output - should still sort correctly *)
  let deps = [
    ("app", Ok (
      Package.Types.{ lina_name = "app"; lina_path = "/app"; lina_optional = false },
      "/app",
      ["math"; "core"]
    ));
    ("math", Ok (
      Package.Types.{ lina_name = "math"; lina_path = "/math"; lina_optional = false },
      "/math",
      ["core"]
    ));
    ("core", Ok (
      Package.Types.{ lina_name = "core"; lina_path = "/core"; lina_optional = false },
      "/core",
      []
    ));
  ] in
  (match Driver.Project_loader.topological_sort_deps deps with
   | Ok sorted -> List.iter (fun (name, _, _) -> print_endline name) sorted
   | Error msg -> print_endline ("Error: " ^ msg));
  [%expect {|
    core
    math
    app |}]
