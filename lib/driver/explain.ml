(** Error code explanation system.

    Provides detailed explanations for error and warning codes. *)

open Common

(** Explanations for each error code. *)
let explanations = [
  (* E0001: Type mismatch *)
  (Error_code.e_type_mismatch, {|
TYPE MISMATCH

This error occurs when an expression has a different type than what was expected
in that context.

Example of erroneous code:

    let add x y = x + y
    let result = add 1 "hello"  -- Error: expected int, got string

Common causes:
  - Passing an argument of the wrong type to a function
  - Using operators with incompatible types (e.g., adding int and string)
  - Returning a value of the wrong type from a function
  - Pattern matching on the wrong type

How to fix:
  - Check the types of your expressions
  - Use type annotations to clarify your intent: let x : int = ...
  - Convert between types explicitly when needed
|});

  (* E0002: Unbound value *)
  (Error_code.e_unbound_value, {|
UNBOUND VALUE

This error occurs when you reference a variable or function that hasn't been
defined in the current scope.

Example of erroneous code:

    let result = undefined_function 42  -- Error: unbound value

Common causes:
  - Typo in a variable or function name
  - Using a variable before it's defined
  - Forgetting to import a module
  - Variable went out of scope

How to fix:
  - Check spelling of the identifier
  - Make sure the definition comes before its use
  - Use 'open Module' or qualify with 'Module.name'
  - Check if the binding is in scope (let bindings are lexically scoped)
|});

  (* E0003: Unbound type *)
  (Error_code.e_unbound_type, {|
UNBOUND TYPE

This error occurs when you reference a type that hasn't been defined.

Example of erroneous code:

    let x : undefined_type = 42  -- Error: unbound type

Common causes:
  - Typo in a type name
  - Forgetting to define a type
  - Type is in a different module

How to fix:
  - Check spelling of the type name
  - Define the type before using it
  - Qualify with module name: Module.type_name
|});

  (* E0004: Unbound module *)
  (Error_code.e_unbound_module, {|
UNBOUND MODULE

This error occurs when you reference a module that hasn't been defined.

Example of erroneous code:

    let x = UndefinedModule.value  -- Error: unbound module

Common causes:
  - Typo in the module name
  - Module hasn't been defined
  - Module is in a different file (multi-file modules not yet supported)

How to fix:
  - Check spelling of the module name
  - Define the module before using it
  - Make sure module names start with uppercase
|});

  (* E0005: Unbound constructor *)
  (Error_code.e_unbound_constructor, {|
UNBOUND CONSTRUCTOR

This error occurs when you use a constructor that hasn't been defined.

Example of erroneous code:

    let x = UndefinedConstructor 42  -- Error: unbound constructor

Common causes:
  - Typo in the constructor name
  - Constructor is from a type you haven't defined
  - Forgetting to bring constructors into scope

How to fix:
  - Check spelling of the constructor name
  - Make sure the type defining the constructor is in scope
  - Constructor names must start with uppercase
|});

  (* E0006: Non-exhaustive pattern *)
  (Error_code.e_pattern_not_exhaustive, {|
NON-EXHAUSTIVE PATTERN MATCH

This error occurs when a pattern match doesn't cover all possible cases.

Example of erroneous code:

    type color = Red | Green | Blue

    let name c = match c with
      | Red -> "red"
      | Green -> "green"
      -- Missing: Blue case

Common causes:
  - Forgetting to handle all constructors of a variant type
  - Missing the None case for option types
  - Not handling all tuple patterns

How to fix:
  - Add the missing cases to your match expression
  - Use a wildcard pattern (_) as a catch-all if appropriate
  - Consider whether your type needs all those constructors
|});

  (* E0007: Redundant pattern *)
  (Error_code.e_redundant_pattern, {|
REDUNDANT PATTERN

This warning occurs when a pattern in a match expression can never be reached
because earlier patterns already cover all the cases.

Example of erroneous code:

    match x with
    | _ -> "anything"
    | 42 -> "forty-two"  -- Warning: this pattern is never reached

Common causes:
  - Wildcard pattern before specific patterns
  - Duplicate patterns
  - Overlapping patterns

How to fix:
  - Reorder patterns so more specific ones come first
  - Remove duplicate or unreachable patterns
|});

  (* E0008: Occurs check *)
  (Error_code.e_occurs_check, {|
OCCURS CHECK (INFINITE TYPE)

This error occurs when type inference would create an infinite (recursive) type.

Example of erroneous code:

    let f x = f  -- Error: would create type 'a -> 'a -> 'a -> ...

Common causes:
  - Returning a function that takes itself as an argument
  - Circular type dependencies
  - Incorrectly recursive definitions

How to fix:
  - Review your recursive definitions
  - Use explicit type annotations to understand what's happening
  - Break circular dependencies
|});

  (* E0009: Arity mismatch *)
  (Error_code.e_arity_mismatch, {|
ARITY MISMATCH

This error occurs when a function or constructor is applied to the wrong
number of arguments.

Example of erroneous code:

    let add x y = x + y
    let result = add 1  -- Error: function expects 2 arguments

Common causes:
  - Too few arguments provided
  - Too many arguments provided
  - Confusion about curried vs uncurried functions

How to fix:
  - Check the function's signature for the expected number of arguments
  - Provide all required arguments
  - If partial application is intended, make sure the types align
|});

  (* E0010: Signature mismatch *)
  (Error_code.e_signature_mismatch, {|
SIGNATURE MISMATCH

This error occurs when a module implementation doesn't match its signature.

Example of erroneous code:

    module type S = sig
      val x : int
    end

    module M : S = struct
      let x = "hello"  -- Error: expected int, got string
    end

Common causes:
  - Type mismatch between signature and implementation
  - Missing values or types in the implementation
  - Extra items not declared in the signature (if opaque)

How to fix:
  - Ensure all types match between signature and implementation
  - Add missing definitions to the implementation
  - Update the signature to match the implementation
|});

  (* E0011: Syntax error *)
  (Error_code.e_syntax_error, {|
SYNTAX ERROR

This error occurs when the parser encounters invalid syntax.

Common causes:
  - Missing or extra parentheses, brackets, or braces
  - Missing 'in' after 'let' bindings in expressions
  - Invalid operator usage
  - Typos in keywords

How to fix:
  - Check for balanced parentheses and brackets
  - Ensure 'let ... in ...' is complete in expressions
  - Review Lina syntax documentation
|});

  (* E0012: Lexer error *)
  (Error_code.e_lexer_error, {|
LEXER ERROR

This error occurs during tokenization, before parsing.

Common causes:
  - Invalid characters in the source file
  - Unclosed string literals
  - Unclosed comments
  - Invalid escape sequences in strings

How to fix:
  - Check for unclosed strings or comments
  - Remove invalid characters
  - Fix escape sequences (use \\ for backslash, \" for quote)
|});

  (* E0013: Unbound module type *)
  (Error_code.e_unbound_module_type, {|
UNBOUND MODULE TYPE

This error occurs when you reference a module type that hasn't been defined.

Example of erroneous code:

    module M : UNDEFINED_SIG = struct
      let x = 1
    end

Common causes:
  - Typo in the module type name
  - Module type hasn't been defined
  - Module type is in a different module

How to fix:
  - Check spelling of the module type name
  - Define the module type before using it
  - Qualify with module name: Module.ModuleTypeName
|});

  (* E0014: Unbound field *)
  (Error_code.e_unbound_field, {|
UNBOUND RECORD FIELD

This error occurs when accessing a field that doesn't exist on a record.

Example of erroneous code:

    let point = { x = 1; y = 2 }
    let z = point.z  -- Error: field 'z' not found

Common causes:
  - Typo in field name
  - Accessing a field from the wrong record type
  - Field doesn't exist in the record definition

How to fix:
  - Check spelling of the field name
  - Verify you're using the correct record type
  - Add the field to the record definition if needed
|});

  (* E0015: Duplicate definition *)
  (Error_code.e_duplicate_definition, {|
DUPLICATE DEFINITION

This error occurs when the same name is defined multiple times in a scope
where duplicates are not allowed (e.g., record fields, constructors, type
parameters).

Example of erroneous code:

    type point = { x : int; x : int }  -- Error: duplicate field 'x'

    type t = A | B | A  -- Error: duplicate constructor 'A'

How to fix:
  - Remove the duplicate definition
  - Use different names for distinct items
|});

  (* E0016: Recursive type *)
  (Error_code.e_recursive_type, {|
RECURSIVE TYPE WITHOUT INDIRECTION

This error occurs when a type directly references itself without using
a constructor or other indirection.

Example of erroneous code:

    type t = t  -- Error: infinite type

    type bad = { self : bad }  -- Error without constructor

Correct recursive types:

    type 'a list = Nil | Cons of 'a * 'a list  -- OK: uses constructor

How to fix:
  - Use a variant type with constructors
  - Ensure there's a base case that doesn't recurse
|});

  (* W0001: Unused variable *)
  (Error_code.w_unused_variable, {|
UNUSED VARIABLE

This warning occurs when a variable is defined but never used.

Example:

    let unused = 42
    let result = 100  -- 'unused' is never referenced

Why this matters:
  - May indicate dead code
  - Could be a bug (forgot to use the variable)
  - Makes code harder to understand

How to fix:
  - Remove the unused binding
  - Prefix with underscore if intentional: let _unused = 42
  - Use the variable if it was meant to be used
|});

  (* W0002: Non-exhaustive pattern (warning) *)
  (Error_code.w_non_exhaustive, {|
NON-EXHAUSTIVE PATTERN (WARNING)

This warning indicates that a pattern match might not cover all cases.
While the compiler can still generate code, the match might fail at runtime.

Example:

    let get_some opt = match opt with
      | Some x -> x
      -- Missing None case: will crash if None is passed

How to fix:
  - Add the missing cases
  - Use a wildcard pattern (_) with appropriate error handling
|});

  (* W0003: Redundant pattern *)
  (Error_code.w_redundant_pattern, {|
REDUNDANT PATTERN

This warning occurs when a pattern can never match because earlier patterns
already handle all the cases it would cover.

Example:

    match x with
    | true -> "yes"
    | false -> "no"
    | _ -> "unreachable"  -- Warning: redundant pattern

How to fix:
  - Remove the redundant pattern
  - Reorder patterns if the order was wrong
|});

  (* W0004: Shadowing *)
  (Error_code.w_shadowing, {|
VARIABLE SHADOWING

This warning occurs when a new binding hides an existing one with the same name.

Example:

    let x = 1
    let x = 2  -- Warning: shadows previous binding of 'x'
    print x    -- Uses the second x (value 2)

Why this matters:
  - Can lead to confusion about which value is being used
  - May indicate a copy-paste error
  - Can hide bugs

How to fix:
  - Use a different name for the new binding
  - If shadowing is intentional, consider if it's clear to readers
|});
]

let get_explanation code =
  List.assoc_opt code explanations

let get_explanation_by_string code_str =
  match Error_code.of_string code_str with
  | Some code -> get_explanation code
  | None -> None

let format_explanation ~color code explanation =
  let header = Error_code.to_string code in
  if color then
    Printf.sprintf "\027[1;36m%s\027[0m\n%s" header explanation
  else
    Printf.sprintf "%s\n%s" header explanation
