open Common

type primitive =
  | PrimitiveAddInt
  | PrimitiveSubInt
  | PrimitiveMulInt
  | PrimitiveDivInt
  | PrimitiveNegInt
  | PrimitiveIntEqual
  | PrimitiveIntNotEqual
  | PrimitiveIntLess
  | PrimitiveIntGreater
  | PrimitiveIntLessEqual
  | PrimitiveIntGreaterEqual
  | PrimitiveMakeBlock of int
  | PrimitiveGetField of int
  | PrimitivePrint

type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBool of bool
  | ConstantUnit

type constructor_tag = {
  tag_name : string;
  tag_index : int;
  tag_type_name : string;
  tag_is_nullary : bool;
}

type lambda =
  | LambdaVariable of Identifier.t
  | LambdaConstant of constant
  | LambdaApply of lambda * lambda list
  | LambdaFunction of Identifier.t list * lambda
  | LambdaLet of Identifier.t * lambda * lambda
  | LambdaLetRecursive of (Identifier.t * lambda) list * lambda
  | LambdaPrimitive of primitive * lambda list
  | LambdaIfThenElse of lambda * lambda * lambda
  | LambdaSequence of lambda * lambda
  | LambdaMakeBlock of int * lambda list
  | LambdaGetField of int * lambda
  | LambdaSwitch of lambda * switch_case list * lambda option
  | LambdaConstructor of constructor_tag * lambda option
  | LambdaMakeRecord of (string * lambda) list
  | LambdaGetRecordField of string * lambda
  | LambdaRecordUpdate of lambda * (string * lambda) list
  | LambdaModule of module_binding list
  | LambdaModuleAccess of lambda * string
  | LambdaFunctor of Identifier.t * lambda
  | LambdaFunctorApply of lambda * lambda

and module_binding = {
  mb_name : string;
  mb_value : lambda;
}

and switch_case = {
  switch_tag : int;
  switch_body : lambda;
}

val translate_structure : Typing.Typed_tree.typed_structure -> lambda list
