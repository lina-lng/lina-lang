(** Tests for the built-in result type.

    Verifies that the result type and its constructors are properly
    registered in the initial environment. *)

open Typing

(** {1 Type Registration Tests} *)

let%expect_test "result type is built-in" =
  let env = Environment.initial in
  let result_type = Environment.find_type "result" env in
  (match result_type with
   | Some decl -> print_endline decl.Types.declaration_name
   | None -> print_endline "NOT FOUND");
  [%expect {| result |}]

let%expect_test "result type has two type parameters" =
  let env = Environment.initial in
  let result_type = Environment.find_type "result" env in
  (match result_type with
   | Some decl ->
       Printf.printf "param_count=%d" (List.length decl.Types.declaration_parameters)
   | None -> print_endline "NOT FOUND");
  [%expect {| param_count=2 |}]

let%expect_test "result type has correct variances" =
  let env = Environment.initial in
  let result_type = Environment.find_type "result" env in
  (match result_type with
   | Some decl ->
       let variances = decl.Types.declaration_variances in
       List.iter (fun variance ->
         print_endline (match variance with
           | Types.Covariant -> "Covariant"
           | Types.Contravariant -> "Contravariant"
           | Types.Invariant -> "Invariant"
           | Types.Bivariant -> "Bivariant")
       ) variances
   | None -> print_endline "NOT FOUND");
  [%expect {|
    Covariant
    Covariant
  |}]

(** {1 Constructor Tests} *)

let%expect_test "Ok constructor is available" =
  let env = Environment.initial in
  let ok_ctor = Environment.find_constructor "Ok" env in
  (match ok_ctor with
   | Some ctor -> print_endline ctor.Types.constructor_name
   | None -> print_endline "NOT FOUND");
  [%expect {| Ok |}]

let%expect_test "Error constructor is available" =
  let env = Environment.initial in
  let error_ctor = Environment.find_constructor "Error" env in
  (match error_ctor with
   | Some ctor -> print_endline ctor.Types.constructor_name
   | None -> print_endline "NOT FOUND");
  [%expect {| Error |}]

let%expect_test "Ok has tag 0" =
  let env = Environment.initial in
  let ok_ctor = Environment.find_constructor "Ok" env in
  (match ok_ctor with
   | Some ctor -> Printf.printf "tag=%d" ctor.Types.constructor_tag_index
   | None -> print_endline "NOT FOUND");
  [%expect {| tag=0 |}]

let%expect_test "Error has tag 1" =
  let env = Environment.initial in
  let error_ctor = Environment.find_constructor "Error" env in
  (match error_ctor with
   | Some ctor -> Printf.printf "tag=%d" ctor.Types.constructor_tag_index
   | None -> print_endline "NOT FOUND");
  [%expect {| tag=1 |}]

let%expect_test "Ok has argument type" =
  let env = Environment.initial in
  let ok_ctor = Environment.find_constructor "Ok" env in
  (match ok_ctor with
   | Some ctor ->
       (match ctor.Types.constructor_argument_type with
        | Some _ -> print_endline "has argument"
        | None -> print_endline "no argument")
   | None -> print_endline "NOT FOUND");
  [%expect {| has argument |}]

let%expect_test "Error has argument type" =
  let env = Environment.initial in
  let error_ctor = Environment.find_constructor "Error" env in
  (match error_ctor with
   | Some ctor ->
       (match ctor.Types.constructor_argument_type with
        | Some _ -> print_endline "has argument"
        | None -> print_endline "no argument")
   | None -> print_endline "NOT FOUND");
  [%expect {| has argument |}]

let%expect_test "Ok and Error belong to result type" =
  let env = Environment.initial in
  let ok_ctor = Environment.find_constructor "Ok" env in
  let error_ctor = Environment.find_constructor "Error" env in
  (match ok_ctor, error_ctor with
   | Some ok, Some err ->
       Printf.printf "Ok=%s Error=%s"
         ok.Types.constructor_type_name
         err.Types.constructor_type_name
   | _ -> print_endline "NOT FOUND");
  [%expect {| Ok=result Error=result |}]

(** {1 Re-export Tests} *)

let%expect_test "ok_constructor is re-exported from Environment" =
  let ctor = Environment.ok_constructor in
  print_endline ctor.Types.constructor_name;
  [%expect {| Ok |}]

let%expect_test "error_constructor is re-exported from Environment" =
  let ctor = Environment.error_constructor in
  print_endline ctor.Types.constructor_name;
  [%expect {| Error |}]
