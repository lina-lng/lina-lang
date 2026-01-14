let compile source =
  Typing.Types.reset_type_variable_id ();
  match Driver.Pipeline.compile_string Driver.Pipeline.default_options "<test>" source with
  | Ok lua_code -> lua_code
  | Error msg -> "ERROR: " ^ msg

let compile_and_run source =
  let lua_code = compile source in
  if String.sub lua_code 0 6 = "ERROR:" then
    lua_code
  else
    (* Write to temp file and run with luajit *)
    let temp_file = Filename.temp_file "lina_test" ".lua" in
    let oc = open_out temp_file in
    output_string oc lua_code;
    close_out oc;
    let ic = Unix.open_process_in ("luajit " ^ temp_file ^ " 2>&1") in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    Sys.remove temp_file;
    String.trim output

(* ==================== Simple Modules ==================== *)

let%expect_test "simple module definition" =
  print_endline (compile {|
module M = struct
  let x = 42
end
|});
  [%expect{|
    local x_13 = 42
    local M_14 = {x = x_13}
    |}]

let%expect_test "module with multiple bindings" =
  print_endline (compile {|
module Math = struct
  let add x y = x + y
  let sub x y = x - y
end
|});
  [%expect{|
    local function add_17(x_15, y_16)
      return x_15 + y_16
    end
    local function sub_20(x_18, y_19)
      return x_18 - y_19
    end
    local add_17 = add_17
    local sub_20 = sub_20
    local Math_21 = {add = add_17, sub = sub_20}
    |}]

let%expect_test "module value access" =
  print_endline (compile {|
module M = struct
  let x = 42
end
let y = M.x
|});
  [%expect{|
    local x_22 = 42
    local M_23 = {x = x_22}
    local y_24 = M_23.x
    |}]

let%expect_test "module function access and call" =
  print_endline (compile_and_run {|
module Math = struct
  let add x y = x + y
end
let result = Math.add 10 20
let _ = print result
|});
  [%expect{| 30 |}]

(* ==================== Nested Modules ==================== *)

let%expect_test "nested modules" =
  print_endline (compile {|
module Outer = struct
  module Inner = struct
    let x = 42
  end
end
|});
  [%expect{|
    local x_31 = 42
    local Inner_32 = {x = x_31}
    local Outer_33 = {Inner = Inner_32}
    |}]

let%expect_test "nested module access" =
  print_endline (compile_and_run {|
module Outer = struct
  module Inner = struct
    let value = 100
  end
end
let x = Outer.Inner.value
let _ = print x
|});
  [%expect{| 100 |}]

(* ==================== Signatures ==================== *)

let%expect_test "module with signature constraint" =
  print_endline (compile {|
module M : sig
  val x : int
end = struct
  let x = 42
  let hidden = 99
end
|});
  [%expect{|
    local x_40 = 42
    local hidden_41 = 99
    local M_42 = {x = x_40, hidden = hidden_41}
    |}]

let%expect_test "signature hides value" =
  print_endline (compile {|
module M : sig
  val x : int
end = struct
  let x = 42
  let hidden = 99
end
let y = M.x
|});
  [%expect{|
    local x_43 = 42
    local hidden_44 = 99
    local M_45 = {x = x_43, hidden = hidden_44}
    local y_46 = M_45.x
    |}]

let%expect_test "signature mismatch - missing value" =
  print_endline (compile {|
module M : sig
  val x : int
  val y : int
end = struct
  let x = 42
end
|});
  [%expect{|
    ERROR: File "<string>", line 2, characters 0-3:
    Type error: Module M does not match signature: Missing value: y
    |}]

let%expect_test "signature with function type" =
  print_endline (compile_and_run {|
module M : sig
  val add : int -> int -> int
end = struct
  let add x y = x + y
end
let result = M.add 3 4
let _ = print result
|});
  [%expect{| 7 |}]

(* ==================== Functors ==================== *)

let%expect_test "simple functor definition" =
  print_endline (compile {|
module MakeDouble = functor (X : sig val x : int end) -> struct
  let doubled = X.x + X.x
end
|});
  [%expect{|
    local MakeDouble_56 = function(X_54)
      return {doubled = X_54.x + X_54.x}
    end
    |}]

let%expect_test "functor application" =
  print_endline (compile_and_run {|
module MakeDouble = functor (X : sig val x : int end) -> struct
  let doubled = X.x + X.x
end
module Input = struct
  let x = 21
end
module Result = MakeDouble(Input)
let _ = print Result.doubled
|});
  [%expect{| 42 |}]

let%expect_test "functor shorthand syntax" =
  print_endline (compile_and_run {|
module MakeTriple(X : sig val n : int end) = struct
  let tripled = X.n + X.n + X.n
end
module Input = struct
  let n = 10
end
module Result = MakeTriple(Input)
let _ = print Result.tripled
|});
  [%expect{| 30 |}]

let%expect_test "functor with function in signature" =
  print_endline (compile_and_run {|
module MakeMapper(F : sig val f : int -> int end) = struct
  let apply x = F.f x
end
module Double = struct
  let f x = x + x
end
module Mapper = MakeMapper(Double)
let result = Mapper.apply 25
let _ = print result
|});
  [%expect{| 50 |}]

let%expect_test "functor argument mismatch" =
  print_endline (compile {|
module MakeDouble = functor (X : sig val x : int end) -> struct
  let doubled = X.x + X.x
end
module Bad = struct
  let y = 42
end
module Result = MakeDouble(Bad)
|});
  [%expect{|
    ERROR: File "<string>", line 8, characters 16-31:
    Type error: Functor argument does not match parameter: Missing value: x
    |}]

(* ==================== Open ==================== *)

let%expect_test "open module" =
  print_endline (compile {|
module M = struct
  let x = 10
  let y = 20
end
open M
let z = x + y
|});
  [%expect{|
    local x_86 = 10
    local y_87 = 20
    local M_88 = {x = x_86, y = y_87}
    local x_89 = M_88.x
    local y_90 = M_88.y
    local z_91 = x_89 + y_90
    |}]

let%expect_test "open module - runtime" =
  print_endline (compile_and_run {|
module Math = struct
  let add x y = x + y
  let sub x y = x - y
end
open Math
let result = add 100 sub 50 30
let _ = print result
|});
  [%expect{|
    ERROR: File "<string>", line 7, characters 13-30:
    Type error: Type mismatch: expected (int -> (int -> int)), got int
    Expected: (int -> (int -> int))
    Actual: int
    |}]

let%expect_test "open nested module" =
  print_endline (compile_and_run {|
module Outer = struct
  module Inner = struct
    let value = 42
  end
end
open Outer.Inner
let x = value
let _ = print x
|});
  [%expect{| 42 |}]

(* ==================== Include ==================== *)

let%expect_test "include in module" =
  print_endline (compile_and_run {|
module Base = struct
  let x = 10
  let y = 20
end
module Extended = struct
  include Base
  let z = x + y
end
let _ = print Extended.z
|});
  [%expect{| 30 |}]

let%expect_test "include exposes all values" =
  print_endline (compile_and_run {|
module Base = struct
  let a = 1
  let b = 2
end
module Extended = struct
  include Base
end
let sum = Extended.a + Extended.b
let _ = print sum
|});
  [%expect{| 3 |}]

let%expect_test "include with override" =
  print_endline (compile_and_run {|
module Base = struct
  let x = 10
end
module Override = struct
  include Base
  let x = 99
end
let _ = print Override.x
|});
  [%expect{| 99 |}]

(* ==================== Complex Examples ==================== *)

let%expect_test "functor + open" =
  print_endline (compile_and_run {|
module MakeOps(N : sig val base : int end) = struct
  let add x = N.base + x
  let double x = x + x
end
module Ops = MakeOps(struct let base = 100 end)
open Ops
let result = add (double 5)
let _ = print result
|});
  [%expect{| 110 |}]

let%expect_test "multiple nested modules" =
  print_endline (compile_and_run {|
module A = struct
  module B = struct
    module C = struct
      let deep = 999
    end
  end
end
let x = A.B.C.deep
let _ = print x
|});
  [%expect{| 999 |}]

let%expect_test "functor returning module with nested module" =
  print_endline (compile_and_run {|
module MakeWithNested(X : sig val n : int end) = struct
  module Inner = struct
    let value = X.n
  end
end
module Result = MakeWithNested(struct let n = 77 end)
let _ = print Result.Inner.value
|});
  [%expect{| 77 |}]

let%expect_test "recursive functions in module" =
  print_endline (compile_and_run {|
module Fib = struct
  let rec fib n =
    if n <= 1 then n
    else fib (n - 1) + fib (n - 2)
end
let _ = print (Fib.fib 10)
|});
  [%expect{| 55 |}]

let%expect_test "mutual recursion in module" =
  print_endline (compile_and_run {|
module EvenOdd = struct
  let rec even n = if n == 0 then 1 else odd (n - 1)
  and odd n = if n == 0 then 0 else even (n - 1)
end
let _ = print (EvenOdd.even 10)
let _ = print (EvenOdd.odd 10)
|});
  [%expect{|
    1
    0
    |}]

(* ==================== Error Messages ==================== *)

let%expect_test "unbound module error" =
  print_endline (compile {|
let x = NonExistent.value
|});
  [%expect{|
    ERROR: File "<string>", line 2, characters 8-19:
    Type error: Unbound constructor: NonExistent
    |}]

let%expect_test "value not found in module" =
  print_endline (compile {|
module M = struct
  let x = 42
end
let y = M.z
|});
  [%expect{|
    ERROR: File "<string>", line 5, characters 8-11:
    Type error: Value z not found in module M
    |}]

let%expect_test "cannot open functor" =
  print_endline (compile {|
module F = functor (X : sig val x : int end) -> struct
  let y = X.x
end
open F
|});
  [%expect{|
    ERROR: File "<string>", line 5, characters 0-6:
    Type error: Cannot open a functor
    |}]

let%expect_test "signature type mismatch" =
  print_endline (compile {|
module M : sig val x : int end = struct
  let x = "hello"
end
|});
  [%expect{|
    ERROR: Type error: Type mismatch: expected string, got int
    Expected: string
    Actual: int
    |}]

let%expect_test "nested path module not found" =
  print_endline (compile {|
module Outer = struct
  module Inner = struct
    let x = 1
  end
end
let y = Outer.NonExistent.x
|});
  [%expect{|
    ERROR: File "<string>", line 7, characters 8-27:
    Type error: Module NonExistent not found in signature
    |}]

let%expect_test "include non-module" =
  print_endline (compile {|
module M = struct
  include 42
end
|});
  [%expect{|
    ERROR: File "<string>", line 3, characters 10-12:
    Parse error: Syntax error
    |}]

let%expect_test "functor application with wrong argument type" =
  print_endline (compile {|
module F = functor (X : sig val x : int end) -> struct
  let y = X.x + 1
end
module M = F(struct let x = "hello" end)
|});
  [%expect{|
    ERROR: Type error: Type mismatch: expected string, got int
    Expected: string
    Actual: int
    |}]

(* ==================== Edge Cases ==================== *)

let%expect_test "empty module" =
  print_endline (compile {|
module Empty = struct end
let _ = print 1
|});
  [%expect{|
    local Empty_184 = {}
    local _top_185 = print(1)
    |}]

let%expect_test "single value module" =
  print_endline (compile_and_run {|
module Single = struct
  let x = 42
end
let _ = print Single.x
|});
  [%expect{| 42 |}]

let%expect_test "deeply nested modules (4 levels)" =
  print_endline (compile_and_run {|
module A = struct
  module B = struct
    module C = struct
      module D = struct
        let value = 999
      end
    end
  end
end
let _ = print A.B.C.D.value
|});
  [%expect{| 999 |}]

let%expect_test "value shadowing - local vs module" =
  print_endline (compile_and_run {|
module M = struct
  let x = 10
end
let x = 5
open M
let y = x
let _ = print y
|});
  [%expect{| 10 |}]

let%expect_test "module shadowing" =
  print_endline (compile_and_run {|
module M = struct
  let x = 1
end
module M = struct
  let x = 2
end
let _ = print M.x
|});
  [%expect{| 2 |}]

(* ==================== Pattern Matching in Modules ==================== *)

let%expect_test "module with ADT" =
  print_endline (compile_and_run {|
type 'a option = None | Some of 'a
module Option = struct
  let map f opt = match opt with
    | None -> None
    | Some x -> Some (f x)
  let get_or_default default opt = match opt with
    | None -> default
    | Some x -> x
end
let doubled = Option.map (fun x -> x + x) (Some 21)
let result = Option.get_or_default 0 doubled
let _ = print result
|});
  [%expect{| 42 |}]

(* ==================== Integration Tests ==================== *)

let%expect_test "stack implementation" =
  print_endline (compile_and_run {|
type 'a stack = Empty | Push of 'a * 'a stack

module Stack = struct
  let empty = Empty
  let push x s = Push (x, s)
  let pop s = match s with
    | Empty -> Empty
    | Push (_, rest) -> rest
  let top s = match s with
    | Empty -> 0
    | Push (x, _) -> x
end

let s = Stack.empty
let s = Stack.push 1 s
let s = Stack.push 2 s
let s = Stack.push 3 s
let _ = print (Stack.top s)
let s = Stack.pop s
let _ = print (Stack.top s)
|});
  [%expect{|
    3
    2
    |}]

let%expect_test "counter module" =
  print_endline (compile_and_run {|
module MakeCounter(Init : sig val start : int end) = struct
  let value = Init.start
  let inc x = x + 1
  let dec x = x - 1
end

module Counter = MakeCounter(struct let start = 0 end)
open Counter

let c = value
let c = inc c
let c = inc c
let c = inc c
let _ = print c
|});
  [%expect{| 3 |}]

(* ==================== Module Type Definitions ==================== *)

let%expect_test "module type definition" =
  print_endline (compile {|
module type SIMPLE = sig
  val x : int
end
|});
  [%expect{| |}]

let%expect_test "module type used as constraint" =
  print_endline (compile_and_run {|
module type ADDABLE = sig
  val x : int
  val y : int
end
module M : ADDABLE = struct
  let x = 10
  let y = 20
  let hidden = 30
end
let _ = print (M.x + M.y)
|});
  [%expect{| 30 |}]

let%expect_test "module type used for functor parameter" =
  print_endline (compile_and_run {|
module type NUM = sig
  val n : int
end
module Double(X : NUM) = struct
  let doubled = X.n + X.n
end
module Input = struct
  let n = 21
end
module Result = Double(Input)
let _ = print Result.doubled
|});
  [%expect{| 42 |}]

let%expect_test "module type not found error" =
  print_endline (compile {|
module M : NONEXISTENT = struct
  let x = 42
end
|});
  [%expect{|
    ERROR: File "<string>", line 2, characters 11-22:
    Type error: Unbound module type: NONEXISTENT
    |}]

let%expect_test "module type reuse" =
  print_endline (compile_and_run {|
module type POINT = sig
  val x : int
  val y : int
end
module P1 : POINT = struct let x = 1 let y = 2 end
module P2 : POINT = struct let x = 10 let y = 20 end
let _ = print (P1.x + P2.y)
|});
  [%expect{| 21 |}]

(* ==================== Qualified Module Type Paths ==================== *)

let%expect_test "qualified module type path M.S" =
  print_endline (compile_and_run {|
module Container = struct
  module type ELEMENT = sig
    val value : int
  end
end
module Elem : Container.ELEMENT = struct
  let value = 42
end
let _ = print Elem.value
|});
  [%expect{| 42 |}]

let%expect_test "nested qualified module type path M.N.S" =
  print_endline (compile_and_run {|
module Outer = struct
  module Inner = struct
    module type T = sig
      val x : int
    end
  end
end
module M : Outer.Inner.T = struct
  let x = 99
end
let _ = print M.x
|});
  [%expect{| 99 |}]

let%expect_test "qualified path functor parameter" =
  print_endline (compile_and_run {|
module Types = struct
  module type NUM = sig
    val n : int
  end
end
module Double(X : Types.NUM) = struct
  let result = X.n + X.n
end
module R = Double(struct let n = 15 end)
let _ = print R.result
|});
  [%expect{| 30 |}]

let%expect_test "qualified path not found error" =
  print_endline (compile {|
module M = struct
  module type EXISTS = sig val x : int end
end
module Bad : M.NOPE = struct
  let x = 1
end
|});
  [%expect{|
    ERROR: File "<string>", line 5, characters 13-19:
    Type error: Module type NOPE not found in module
    |}]

(* ==================== Functor Signature Matching ==================== *)

let%expect_test "functor signature constraint" =
  print_endline (compile_and_run {|
module type IN = sig val x : int end
module type OUT = sig val y : int end
module type F = functor (X : IN) -> OUT

module MyFunc : F = functor (X : IN) -> struct
  let y = X.x + 1
end
module Result = MyFunc(struct let x = 41 end)
let _ = print Result.y
|});
  [%expect{| 42 |}]

let%expect_test "functor result covariance - impl has more" =
  print_endline (compile_and_run {|
module type IN = sig val x : int end
module type OUT = sig val y : int end
module type F = functor (X : IN) -> OUT

(* Implementation returns more than signature requires - OK *)
module MyFunc : F = functor (X : IN) -> struct
  let y = X.x
  let extra = 999
end
module Result = MyFunc(struct let x = 42 end)
let _ = print Result.y
|});
  [%expect{| 42 |}]

let%expect_test "functor result covariance - impl missing value" =
  print_endline (compile {|
module type IN = sig val x : int end
module type OUT = sig val y : int end
module type F = functor (X : IN) -> OUT

(* Implementation missing required value - ERROR *)
module MyFunc : F = functor (X : IN) -> struct
  let z = X.x
end
|});
  [%expect{|
    ERROR: File "<string>", line 7, characters 0-3:
    Type error: Module MyFunc does not match signature: Missing value: y
    |}]

let%expect_test "functor param contravariance - impl accepts more" =
  print_endline (compile_and_run {|
module type SMALL = sig val x : int end
module type BIG = sig val x : int val y : int end

(* Signature parameter is BIG, implementation accepts SMALL - OK *)
(* Because anything that provides BIG also provides SMALL *)
module type F = functor (X : BIG) -> sig val result : int end
module MyFunc : F = functor (X : SMALL) -> struct
  let result = X.x
end
module R = MyFunc(struct let x = 10 let y = 20 end)
let _ = print R.result
|});
  [%expect{| 10 |}]

let%expect_test "functor param contravariance - impl too strict" =
  print_endline (compile {|
module type SMALL = sig val x : int end
module type BIG = sig val x : int val y : int end

(* Signature parameter is SMALL, implementation requires BIG - ERROR *)
(* Because callers might provide only SMALL *)
module type F = functor (X : SMALL) -> sig val result : int end
module MyFunc : F = functor (X : BIG) -> struct
  let result = X.x + X.y
end
|});
  [%expect{|
    ERROR: File "<string>", line 8, characters 0-3:
    Type error: Module MyFunc does not match signature: Missing value: y
    |}]

(* ==================== With Constraints ==================== *)

let%expect_test "with type constraint basic" =
  print_endline (compile {|
module type S = sig
  type t
  val x : int
end
module type S2 = S with type t = int
|});
  [%expect{| |}]

let%expect_test "with type constraint - type not found" =
  print_endline (compile {|
module type S = sig
  val x : int
end
module type S2 = S with type t = int
|});
  [%expect{|
    ERROR: File "<string>", line 5, characters 17-36:
    Type error: Type t not found in signature
    |}]

let%expect_test "with type constraint - parameterized type" =
  print_endline (compile {|
module type S = sig
  type 'a t
  val x : int
end
module type S2 = S with type t 'a = int
|});
  [%expect{| |}]

let%expect_test "with type constraint - wrong parameter count" =
  print_endline (compile {|
module type S = sig
  type 'a t
  val x : int
end
module type S2 = S with type t = int
|});
  [%expect{|
    ERROR: File "<string>", line 6, characters 17-36:
    Type error: Type t has 1 parameters but constraint has 0
    |}]

(* ==================== Destructive Substitution ==================== *)

let%expect_test "destructive substitution basic" =
  (* Basic parsing test - type exists in signature *)
  print_endline (compile {|
module type S = sig
  type t
  val x : int
end
module type S_INT = S with type t := int
|});
  [%expect{| |}]

let%expect_test "destructive substitution - type not found" =
  print_endline (compile {|
module type S = sig
  val x : int
end
module type S2 = S with type t := int
|});
  [%expect{|
    ERROR: File "<string>", line 5, characters 17-37:
    Type error: Type t not found in signature
    |}]

let%expect_test "destructive substitution - wrong parameter count" =
  print_endline (compile {|
module type S = sig
  type 'a t
  val x : int
end
module type S2 = S with type t := int
|});
  [%expect{|
    ERROR: File "<string>", line 6, characters 17-37:
    Type error: Type t has 1 parameters but constraint has 0
    |}]

(* ==================== Module Binding Inter-Reference Tests ==================== *)
(* These tests verify that bindings within a module can reference earlier bindings
   in the same module. This was a bug where the codegen generated inline table
   literals instead of defining bindings first. *)

let%expect_test "module binding references earlier binding" =
  print_endline (compile_and_run {|
module M = struct
  let id = fun x -> x
  let applied = id id
  let value = applied 42
end
let _ = print M.value
|});
  [%expect{| 42 |}]

let%expect_test "module binding chain" =
  print_endline (compile_and_run {|
module M = struct
  let a = 10
  let b = a + 5
  let c = b * 2
end
let _ = print M.c
|});
  [%expect{| 30 |}]

let%expect_test "module binding references function" =
  print_endline (compile_and_run {|
module M = struct
  let f = fun x -> x + 1
  let result = f 41
end
let _ = print M.result
|});
  [%expect{| 42 |}]

let%expect_test "nested module binding references" =
  print_endline (compile_and_run {|
module Outer = struct
  module Inner = struct
    let f = fun x -> x
    let g = f f
    let value = g 100
  end
  let result = Inner.value
end
let _ = print Outer.result
|});
  [%expect{| 100 |}]

let%expect_test "module with multiple inter-references" =
  print_endline (compile_and_run {|
module M = struct
  let add = fun x -> fun y -> x + y
  let add5 = add 5
  let add10 = add 10
  let result = add5 (add10 20)
end
let _ = print M.result
|});
  [%expect{| 35 |}]

(* ==================== Private Types ==================== *)

let%expect_test "private type prevents construction" =
  print_endline (compile {|
type t = private | A | B of int
let x = A
|});
  [%expect{|
    ERROR: File "<string>", line 3, characters 8-9:
    Type error: Cannot construct value of private type t
    |}]

let%expect_test "private type prevents construction with argument" =
  print_endline (compile {|
type t = private | A | B of int
let x = B 42
|});
  [%expect{|
    ERROR: File "<string>", line 3, characters 8-12:
    Type error: Cannot construct value of private type t
    |}]

let%expect_test "private type allows pattern matching" =
  (* Pattern matching on private types should be allowed *)
  print_endline (compile {|
type t = private | A | B of int
let f (x : t) = match x with
  | A -> 0
  | B n -> n
|});
  [%expect{|
    local function f_365(x_363)
      local _scrutinee_366 = x_363
      if _scrutinee_366._tag == 1 then
        local n_364 = _scrutinee_366._0
        return n_364
      else
        if _scrutinee_366._tag == 0 then
          return 0
        else
          return error("Match failure")
        end
      end
    end
    |}]

let%expect_test "non-private type allows construction" =
  print_endline (compile_and_run {|
type t = | A | B of int
let x = A
let y = B 42
let result = match y with
  | A -> 0
  | B n -> n
let _ = print result
|});
  [%expect{| 42 |}]

(* ==================== Locally Abstract Types ==================== *)

let%expect_test "locally abstract type parses" =
  (* Basic parsing test - (type a) as a pattern *)
  print_endline (compile {|
let f (type a) (x : a) = x
|});
  [%expect{|
    local function f_375(x_374)
      return x_374
    end
    |}]

let%expect_test "locally abstract type introduces scoped type" =
  (* The abstract type 'a' should be in scope for the body *)
  print_endline (compile {|
let f (type a) (x : a) (y : a) = (x, y)
|});
  [%expect{|
    local function f_379(x_377, y_378)
      return {x_377, y_378}
    end
    |}]

let%expect_test "locally abstract type with multiple type params" =
  print_endline (compile {|
let f (type a) (type b) (x : a) (y : b) = (x, y)
|});
  [%expect{|
    local function f_384(x_382, y_383)
      return {x_382, y_383}
    end
    |}]

let%expect_test "locally abstract type in function body" =
  (* Use the abstract type within the function *)
  print_endline (compile_and_run {|
let f (type a) (x : a) = x
let result = f 42
let _ = print result
|});
  [%expect{| 42 |}]
