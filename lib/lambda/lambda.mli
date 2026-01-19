(** Lambda intermediate representation.

    This module defines the Lambda IR, a simplified intermediate language
    between typed AST and Lua code generation. The IR is designed to be
    close to the target while still abstracting over low-level details.

    {2 Key Features}

    - All pattern matching is compiled to switches and if-chains
    - Variant constructors carry integer tags for efficient dispatch
    - Records use field names as keys
    - Modules become nested let-bindings with field access

    {2 Translation}

    Typed AST is translated to Lambda IR by {!translate_structure}.
    Pattern matching is compiled using Maranget's algorithm in
    {!Pattern_match.compile_match}. *)

open Common

(** {1 Primitives} *)

(** Built-in primitive operations.

    Primitives are implemented directly in the target language rather
    than being defined in Lina source code. *)
type primitive =
  | PrimitiveAddInt              (** Integer addition *)
  | PrimitiveSubInt              (** Integer subtraction *)
  | PrimitiveMulInt              (** Integer multiplication *)
  | PrimitiveDivInt              (** Integer division *)
  | PrimitiveModInt              (** Integer modulo *)
  | PrimitiveNegInt              (** Integer negation *)
  | PrimitiveIntEqual            (** Integer equality *)
  | PrimitiveIntNotEqual         (** Integer inequality *)
  | PrimitiveIntLess             (** Integer less-than *)
  | PrimitiveIntGreater          (** Integer greater-than *)
  | PrimitiveIntLessEqual        (** Integer less-or-equal *)
  | PrimitiveIntGreaterEqual     (** Integer greater-or-equal *)
  | PrimitiveStringEqual         (** String equality *)
  | PrimitiveStringLess          (** String less-than (lexicographic) *)
  | PrimitiveStringGreater       (** String greater-than (lexicographic) *)
  | PrimitiveStringLessEqual     (** String less-or-equal (lexicographic) *)
  | PrimitiveStringGreaterEqual  (** String greater-or-equal (lexicographic) *)
  | PrimitiveStringConcat        (** String concatenation *)
  | PrimitiveBoolNot             (** Boolean negation *)
  | PrimitiveBoolEqual           (** Boolean equality *)
  | PrimitiveBoolNotEqual        (** Boolean inequality *)
  | PrimitiveBoolLess            (** Boolean less-than (false < true) *)
  | PrimitiveBoolGreater         (** Boolean greater-than (true > false) *)
  | PrimitiveBoolLessEqual       (** Boolean less-or-equal *)
  | PrimitiveBoolGreaterEqual    (** Boolean greater-or-equal *)
  | PrimitiveBoolAnd             (** Short-circuit boolean AND (&&) *)
  | PrimitiveBoolOr              (** Short-circuit boolean OR (||) *)
  | PrimitiveListAppend          (** List concatenation (@) *)
  | PrimitiveArrayMake           (** Create array: array_make n init *)
  | PrimitiveArrayLength         (** Get array length: array_length arr *)
  | PrimitiveArrayUnsafeGet      (** Get element without bounds check *)
  | PrimitiveArrayUnsafeSet      (** Set element without bounds check *)
  | PrimitiveArrayEmpty          (** Create empty array: array_empty () *)
  | PrimitiveDictEmpty           (** dict_empty () -> empty dict of any type *)
  | PrimitiveDictGet             (** dict_get key dict -> option *)
  | PrimitiveDictSet             (** dict_set key value dict -> new dict *)
  | PrimitiveDictHas             (** dict_has key dict -> bool *)
  | PrimitiveDictRemove          (** dict_remove key dict -> new dict *)
  | PrimitiveDictSize            (** dict_size dict -> int *)
  | PrimitiveDictKeys            (** dict_keys dict -> key list *)
  | PrimitiveDictEntries         (** dict_entries dict -> (key * value) list *)
  | PrimitiveSetEmpty            (** set_empty () -> empty set of any type *)
  | PrimitiveSetAdd              (** set_add elem set -> new set with elem *)
  | PrimitiveSetRemove           (** set_remove elem set -> new set without elem *)
  | PrimitiveSetMem              (** set_mem elem set -> bool *)
  | PrimitiveSetSize             (** set_size set -> int *)
  | PrimitiveSetElements         (** set_elements set -> elem list *)
  | PrimitiveMakeBlock of int    (** Create a tuple of given arity *)
  | PrimitiveGetField of int     (** Get tuple field by index (0-based) *)
  | PrimitivePrint               (** Print any value *)
  | PrimError                    (** Raise an error with message *)

(** {1 Constants} *)

(** Literal constant values. *)
type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBool of bool
  | ConstantUnit

(** {1 Constructor Tags} *)

(** Information about a variant constructor at runtime.

    Tags are integers used for efficient pattern matching via dispatch tables
    or numeric comparisons. *)
type constructor_tag_index = {
  tag_name : string;       (** Constructor name (e.g., "Some") *)
  tag_index : int;         (** Numeric tag (0-based within type) *)
  tag_type_name : string;  (** Parent type name (e.g., "option") *)
  tag_is_nullary : bool;   (** True if constructor takes no arguments *)
  tag_is_extension : bool; (** True for extensible variant constructors *)
}

(** {1 Lambda Expressions} *)

(** The core Lambda IR type.

    Lambda expressions form a tree structure where each node represents
    an operation to be performed at runtime. *)
type lambda =
  | LambdaVariable of Identifier.t
      (** Variable reference *)
  | LambdaConstant of constant
      (** Literal constant *)
  | LambdaApply of lambda * lambda list
      (** Function application: [f(arg1, arg2, ...)] *)
  | LambdaFunction of Identifier.t list * lambda
      (** Function definition: [fun (x, y) -> body] *)
  | LambdaLet of Identifier.t * lambda * lambda
      (** Let binding: [let x = e1 in e2] *)
  | LambdaLetRecursive of (Identifier.t * lambda) list * lambda
      (** Recursive let: [let rec f = e1 and g = e2 in body] *)
  | LambdaPrimitive of primitive * lambda list
      (** Primitive operation *)
  | LambdaIfThenElse of lambda * lambda * lambda
      (** Conditional: [if cond then e1 else e2] *)
  | LambdaSequence of lambda * lambda
      (** Sequence: [e1; e2] (evaluate e1 for effect, return e2) *)
  | LambdaMakeBlock of int * lambda list
      (** Create tuple: [{e1, e2, ...}] with arity *)
  | LambdaGetField of int * lambda
      (** Tuple field access (0-based index) *)
  | LambdaSwitch of lambda * switch_case list * lambda option
      (** Switch on integer tag: [switch scrutinee { cases } default] *)
  | LambdaConstructor of constructor_tag_index * lambda option
      (** Variant constructor: [Tag] or [Tag(arg)] *)
  | LambdaMakeRecord of (string * lambda) list
      (** Record literal: [{x = e1; y = e2}] *)
  | LambdaGetRecordField of string * lambda
      (** Record field access: [r.field] *)
  | LambdaRecordUpdate of lambda * (string * lambda) list
      (** Record update: [{r with x = e}] *)
  | LambdaModule of module_binding list
      (** Module as record of bindings *)
  | LambdaModuleAccess of lambda * string
      (** Module field access: [M.x] *)
  | LambdaFunctor of Identifier.t * lambda
      (** Functor: [functor (X) -> body] *)
  | LambdaFunctorApply of lambda * lambda
      (** Functor application: [F(A)] *)
  | LambdaExternalCall of Typing_ffi.Types.ffi_spec * lambda list
      (** FFI external call with arguments (translated to inline Lua) *)
  | LambdaRef of lambda
      (** Create mutable reference: [ref e] *)
  | LambdaDeref of lambda
      (** Dereference: [!e] *)
  | LambdaAssign of lambda * lambda
      (** Assignment: [e1 := e2] *)
  | LambdaPolyVariant of string * lambda option
      (** Polymorphic variant constructor: [`Tag] or [`Tag(arg)] *)
  | LambdaWhile of lambda * lambda
      (** While loop: [while cond do body done] *)
  | LambdaFor of Common.Identifier.t * lambda * lambda * Parsing.Syntax_tree.direction_flag * lambda
      (** For loop: [for i = start to/downto end do body done] *)

(** Module binding in a module expression. *)
and module_binding = {
  mb_id : Common.Identifier.t;  (** Original identifier for internal references *)
  mb_value : lambda;            (** Bound value *)
}

(** A case in a switch expression. *)
and switch_case = {
  switch_tag : int;    (** Tag to match *)
  switch_body : lambda;  (** Body to execute on match *)
}

(** {1 Helper Functions} *)

(** Get the binding name for a module binding.
    This is the name used for the module field in generated code. *)
val module_binding_name : module_binding -> string

(** {1 Translation} *)

(** [translate_structure structure] translates typed AST to Lambda IR.

    Processes all structure items and returns a list of Lambda expressions.
    Pattern matching in match expressions is compiled using Maranget's
    algorithm via {!Pattern_match.compile_match}.

    @param structure The typed structure to translate
    @return A list of Lambda expressions for top-level items *)
val translate_structure : Typing.Typed_tree.typed_structure -> lambda list
