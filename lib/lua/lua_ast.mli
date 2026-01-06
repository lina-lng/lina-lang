type identifier = string

type expression =
  | ExpressionNil
  | ExpressionBool of bool
  | ExpressionNumber of float
  | ExpressionString of string
  | ExpressionVariable of identifier
  | ExpressionTable of table_field list
  | ExpressionIndex of expression * expression
  | ExpressionField of expression * identifier
  | ExpressionCall of expression * expression list
  | ExpressionBinaryOp of binary_operator * expression * expression
  | ExpressionUnaryOp of unary_operator * expression
  | ExpressionFunction of identifier list * block

and table_field =
  | FieldArray of expression
  | FieldNamed of identifier * expression
  | FieldIndexed of expression * expression

and binary_operator =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpPow
  | OpConcat
  | OpEqual
  | OpNotEqual
  | OpLess
  | OpGreater
  | OpLessEqual
  | OpGreaterEqual
  | OpAnd
  | OpOr

and unary_operator =
  | OpNegate
  | OpNot
  | OpLength

and statement =
  | StatementLocal of identifier list * expression list
  | StatementAssign of lvalue list * expression list
  | StatementCall of expression * expression list
  | StatementIf of (expression * block) list * block option
  | StatementWhile of expression * block
  | StatementReturn of expression list
  | StatementBreak
  | StatementDo of block
  | StatementLocalFunction of identifier * identifier list * block
  | StatementFunction of identifier * identifier list * block

and lvalue =
  | LvalueVariable of identifier
  | LvalueIndex of expression * expression
  | LvalueField of expression * identifier

and block = statement list

type chunk = block
