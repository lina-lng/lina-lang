(** Tests for the Variance module.

    Tests variance operations: flip, combine, compose, compatible. *)

open Typing

(** Helper to print variance. *)
let variance_to_string = function
  | Variance.Covariant -> "+"
  | Variance.Contravariant -> "-"
  | Variance.Invariant -> ""
  | Variance.Bivariant -> "_"

(** {1 flip tests} *)

let%expect_test "flip: covariant becomes contravariant" =
  let result = Variance.flip Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| - |}]

let%expect_test "flip: contravariant becomes covariant" =
  let result = Variance.flip Variance.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| + |}]

let%expect_test "flip: invariant stays invariant" =
  let result = Variance.flip Variance.Invariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

let%expect_test "flip: bivariant stays bivariant" =
  let result = Variance.flip Variance.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| _ |}]

(** {1 combine tests} *)

let%expect_test "combine: covariant + covariant = covariant" =
  let result = Variance.combine Variance.Covariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| + |}]

let%expect_test "combine: contravariant + contravariant = contravariant" =
  let result = Variance.combine Variance.Contravariant Variance.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| - |}]

let%expect_test "combine: covariant + contravariant = invariant" =
  let result = Variance.combine Variance.Covariant Variance.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

let%expect_test "combine: contravariant + covariant = invariant" =
  let result = Variance.combine Variance.Contravariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

let%expect_test "combine: invariant + anything = invariant" =
  let result = Variance.combine Variance.Invariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

let%expect_test "combine: bivariant + covariant = covariant" =
  let result = Variance.combine Variance.Bivariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| + |}]

let%expect_test "combine: covariant + bivariant = covariant" =
  let result = Variance.combine Variance.Covariant Variance.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| + |}]

let%expect_test "combine: bivariant + bivariant = bivariant" =
  let result = Variance.combine Variance.Bivariant Variance.Bivariant in
  print_endline (variance_to_string result);
  [%expect {| _ |}]

let%expect_test "combine: bivariant + invariant = invariant" =
  let result = Variance.combine Variance.Bivariant Variance.Invariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

(** {1 compose tests} *)

let%expect_test "compose: covariant context preserves position" =
  let result = Variance.compose Variance.Covariant Variance.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| - |}]

let%expect_test "compose: contravariant context flips position" =
  let result = Variance.compose Variance.Contravariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| - |}]

let%expect_test "compose: contravariant context flips contravariant to covariant" =
  let result = Variance.compose Variance.Contravariant Variance.Contravariant in
  print_endline (variance_to_string result);
  [%expect {| + |}]

let%expect_test "compose: invariant context becomes invariant" =
  let result = Variance.compose Variance.Invariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| |}]

let%expect_test "compose: bivariant context becomes bivariant" =
  let result = Variance.compose Variance.Bivariant Variance.Covariant in
  print_endline (variance_to_string result);
  [%expect {| _ |}]

(** {1 compatible tests} *)

let%expect_test "compatible: same variances are compatible" =
  let cases = [
    (Variance.Covariant, Variance.Covariant);
    (Variance.Contravariant, Variance.Contravariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    +, + -> true
    -, - -> true |}]

let%expect_test "compatible: invariant impl is compatible with anything" =
  let cases = [
    (Variance.Covariant, Variance.Invariant);
    (Variance.Contravariant, Variance.Invariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    +,  -> true
    -,  -> true |}]

let%expect_test "compatible: bivariant decl accepts any impl" =
  let cases = [
    (Variance.Bivariant, Variance.Covariant);
    (Variance.Bivariant, Variance.Contravariant);
    (Variance.Bivariant, Variance.Invariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    _, + -> true
    _, - -> true
    _,  -> true |}]

let%expect_test "compatible: bivariant impl is compatible with any decl" =
  let cases = [
    (Variance.Covariant, Variance.Bivariant);
    (Variance.Contravariant, Variance.Bivariant);
    (Variance.Invariant, Variance.Bivariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    +, _ -> true
    -, _ -> true
    , _ -> true |}]

let%expect_test "compatible: invariant decl rejects non-invariant impl" =
  let cases = [
    (Variance.Invariant, Variance.Covariant);
    (Variance.Invariant, Variance.Contravariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    , + -> false
    , - -> false |}]

let%expect_test "compatible: conflicting variances are incompatible" =
  let cases = [
    (Variance.Covariant, Variance.Contravariant);
    (Variance.Contravariant, Variance.Covariant);
  ] in
  List.iter (fun (decl, impl) ->
    let result = Variance.compatible ~impl ~decl in
    Printf.printf "%s, %s -> %b\n"
      (variance_to_string decl) (variance_to_string impl) result
  ) cases;
  [%expect {|
    +, - -> false
    -, + -> false |}]

(** {1 to_string tests} *)

let%expect_test "to_string: all variants" =
  List.iter (fun v -> print_endline (Variance.to_string v))
    [Variance.Covariant; Variance.Contravariant; Variance.Invariant; Variance.Bivariant];
  [%expect {|
    +
    -

    _ |}]

(** {1 to_annotation tests} *)

let%expect_test "to_annotation: all variants" =
  List.iter (fun v -> print_endline (Variance.to_annotation v))
    [Variance.Covariant; Variance.Contravariant; Variance.Invariant; Variance.Bivariant];
  [%expect {|
    +
    -

    _ |}]
