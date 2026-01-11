(** Integration tests for the Lina code formatter.

    These tests verify the public API, configuration options,
    file operations, idempotency, and roundtrip properties. *)

(** {1 API Tests} *)

let%expect_test "format_string: valid code" =
  (match Lina_format.Formatter.format_string "let x = 42" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = 42 |}]

let%expect_test "format_string: syntax error handled gracefully" =
  (* CST formatter handles syntax errors gracefully and returns best-effort output *)
  (match Lina_format.Formatter.format_string "let x =" with
   | Ok formatted -> print_string ("Ok: " ^ formatted)
   | Error _ -> print_string "Error");
  [%expect {| Ok: let x = |}]

let%expect_test "format_string: empty input" =
  (match Lina_format.Formatter.format_string "" with
   | Ok formatted -> print_string ("Result: [" ^ formatted ^ "]")
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| Result: [] |}]

let%expect_test "format_string: already formatted" =
  let source = "let x = 42" in
  (match Lina_format.Formatter.format_string source with
   | Ok formatted ->
       if formatted = source then print_string "UNCHANGED"
       else print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| UNCHANGED |}]

(** {1 Configuration Tests} *)

let%expect_test "custom line width: narrow" =
  let config = Lina_format.Formatter.{ default_config with line_width = 20 } in
  (match Lina_format.Formatter.format_string ~config "let f x y z = x + y + z" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let f x y z = x + y + z |}]

let%expect_test "custom line width: very narrow" =
  let config = Lina_format.Formatter.{ default_config with line_width = 10 } in
  (match Lina_format.Formatter.format_string ~config "let f x y z = x + y + z" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let f x y z = x + y + z |}]

let%expect_test "custom line width: wide" =
  let config = Lina_format.Formatter.{ default_config with line_width = 120 } in
  let source = "let long_function_name argument1 argument2 argument3 = argument1 + argument2 + argument3" in
  (match Lina_format.Formatter.format_string ~config source with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let long_function_name argument1 argument2 argument3 = argument1 + argument2 + argument3 |}]

(** {1 Idempotency Tests} *)

let check_idempotent source =
  match Lina_format.Formatter.format_string source with
  | Error msg -> "PARSE_ERROR: " ^ msg
  | Ok once ->
      match Lina_format.Formatter.format_string once with
      | Error msg -> "REPARSE_ERROR: " ^ msg
      | Ok twice ->
          if once = twice then "IDEMPOTENT"
          else Printf.sprintf "DIFFERS:\nOnce: %s\nTwice: %s" once twice

let%expect_test "idempotency: simple expression" =
  print_string (check_idempotent "let x = 1 + 2 * 3");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: function definition" =
  print_string (check_idempotent "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: pattern matching" =
  print_string (check_idempotent "let x = match opt with | None -> 0 | Some x -> x + 1");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: records" =
  print_string (check_idempotent "let p = { x = 1; y = 2 }");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: module" =
  print_string (check_idempotent "module M = struct type t = int let x = 42 end");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: functor" =
  print_string (check_idempotent "module F (X : S) = struct let y = X.x end");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: signature" =
  print_string (check_idempotent "module type S = sig type t val x : t end");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: type declaration" =
  print_string (check_idempotent "type 'a option = None | Some of 'a");
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: complex nested code" =
  let source = {|
    module Stack = struct
      type 'a t = Nil | Cons of 'a * 'a t
      let empty = Nil
      let push x s = Cons (x, s)
      let pop s = match s with
        | Nil -> None
        | Cons (x, t) -> Some (x, t)
    end
  |} in
  print_string (check_idempotent source);
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: deeply nested expressions" =
  let source = "let x = let a = if cond then match opt with | None -> 0 | Some y -> y else 1 in a + 1" in
  print_string (check_idempotent source);
  [%expect {| IDEMPOTENT |}]

let%expect_test "idempotency: external with attributes" =
  let source = {|@pure external add : int -> int -> int = "add"|} in
  print_string (check_idempotent source);
  [%expect {| IDEMPOTENT |}]

(** {1 Roundtrip Tests} *)

(** Check that formatting preserves parseability:
    parse(format(parse(source))) should succeed *)
let check_roundtrip source =
  match Lina_format.Formatter.format_string source with
  | Error msg -> "FORMAT_ERROR: " ^ msg
  | Ok formatted ->
      match Lina_format.Formatter.format_string formatted with
      | Error msg -> "REFORMAT_ERROR: " ^ msg
      | Ok _ -> "ROUNDTRIP_OK"

let%expect_test "roundtrip: expressions" =
  print_string (check_roundtrip "let x = f (g (h 1 2) 3) 4");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: operators" =
  (* Use valid Lina operators (not && and ||) *)
  print_string (check_roundtrip "let x = a + b * c - d / e");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: lambdas" =
  print_string (check_roundtrip "let f = fun x y z -> x + y + z");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: let-in" =
  print_string (check_roundtrip "let x = let a = 1 in let b = 2 in a + b");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: conditionals" =
  print_string (check_roundtrip "let x = if a then if b then c else d else e");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: match" =
  print_string (check_roundtrip "let x = match y with | A -> 1 | B x -> x | C (a, b) -> a + b");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: records" =
  print_string (check_roundtrip "let x = { base with a = 1; b = 2 }");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: tuples" =
  print_string (check_roundtrip "let x = ((1, 2), (3, 4), 5)");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: type annotations" =
  print_string (check_roundtrip "let f (x : int) (y : string) : bool = true");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: modules" =
  print_string (check_roundtrip "module M = struct type t = A | B of int let x = A end");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: functors" =
  print_string (check_roundtrip "module F (X : S) (Y : T) : U = struct let z = X.a + Y.b end");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: signatures" =
  print_string (check_roundtrip "module type S = sig type t val x : t val f : t -> int end");
  [%expect {| ROUNDTRIP_OK |}]

let%expect_test "roundtrip: with constraints" =
  print_string (check_roundtrip "module type S = T with type t = int and module M = N");
  [%expect {| ROUNDTRIP_OK |}]

(** {1 Edge Cases} *)

let%expect_test "edge case: empty module" =
  (match Lina_format.Formatter.format_string "module M = struct end" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| module M = struct end |}]

let%expect_test "edge case: empty signature" =
  (match Lina_format.Formatter.format_string "module type S = sig end" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| module type S = sig end |}]

let%expect_test "edge case: empty record" =
  (match Lina_format.Formatter.format_string "let x = {}" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = {} |}]

let%expect_test "edge case: unit" =
  (match Lina_format.Formatter.format_string "let x = ()" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = () |}]

let%expect_test "edge case: deeply nested parens" =
  (match Lina_format.Formatter.format_string "let x = ((((1))))" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = ((((1)))) |}]

let%expect_test "edge case: string with escapes" =
  (match Lina_format.Formatter.format_string {|let x = "hello\nworld\t\"quoted\""|} with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = "hello\nworld\t\"quoted\"" |}]

let%expect_test "edge case: long identifier" =
  let long_name = "very_long_identifier_name_that_should_be_preserved_exactly" in
  let source = Printf.sprintf "let %s = 42" long_name in
  (match Lina_format.Formatter.format_string source with
   | Ok formatted ->
       if String.length formatted > 0 && formatted.[String.length formatted - 1] = '\n' then
         print_string (String.sub formatted 0 (String.length formatted - 1))
       else
         print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let very_long_identifier_name_that_should_be_preserved_exactly = 42 |}]

(** {1 Whitespace Handling} *)

let%expect_test "whitespace: leading whitespace removed" =
  (match Lina_format.Formatter.format_string "   let x = 42" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = 42 |}]

let%expect_test "whitespace: trailing whitespace handled" =
  (match Lina_format.Formatter.format_string "let x = 42   " with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = 42 |}]

let%expect_test "whitespace: extra internal whitespace normalized" =
  (match Lina_format.Formatter.format_string "let   x   =   42" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = 42 |}]

let%expect_test "whitespace: newlines normalized" =
  (match Lina_format.Formatter.format_string "let x = 42\n\n\nlet y = 43" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {|
    let x = 42


    let y = 43
    |}]

(** {1 Multiple Declarations} *)

let%expect_test "multiple let declarations" =
  (match Lina_format.Formatter.format_string "let x = 1 let y = 2 let z = 3" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| let x = 1 let y = 2 let z = 3 |}]

let%expect_test "multiple type declarations" =
  (match Lina_format.Formatter.format_string "type t = int type u = string type v = bool" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| type t = int type u = string type v = bool |}]

let%expect_test "mixed declarations" =
  (match Lina_format.Formatter.format_string "type t = int let x : t = 42 module M = struct let y = x end" with
   | Ok formatted -> print_string formatted
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| type t = int let x : t = 42 module M = struct let y = x end |}]

(** {1 Comments Preservation} *)

(* Note: Comment preservation would be tested here if the formatter
   supports comments. Currently, comments are handled by the lexer
   and may not be preserved through formatting. *)

(** {1 Large Input} *)

let%expect_test "large input: many declarations" =
  let declarations = List.init 10 (fun index -> Printf.sprintf "let x%d = %d" index index) in
  let source = String.concat " " declarations in
  (match Lina_format.Formatter.format_string source with
   | Ok formatted ->
       (* CST formatter preserves source layout, so count "let" occurrences *)
       let count =
         String.split_on_char ' ' formatted
         |> List.filter (fun word -> word = "let")
         |> List.length
       in
       Printf.printf "Formatted %d declarations" count
   | Error msg -> print_string ("ERROR: " ^ msg));
  [%expect {| Formatted 10 declarations |}]
