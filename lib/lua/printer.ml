open Lua_ast

let indent_string level =
  String.make (level * 2) ' '

let escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let binary_operator_to_string = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpMod -> "%"
  | OpPow -> "^"
  | OpConcat -> ".."
  | OpEqual -> "=="
  | OpNotEqual -> "~="
  | OpLess -> "<"
  | OpGreater -> ">"
  | OpLessEqual -> "<="
  | OpGreaterEqual -> ">="
  | OpAnd -> "and"
  | OpOr -> "or"

let unary_operator_to_string = function
  | OpNegate -> "-"
  | OpNot -> "not "
  | OpLength -> "#"

let rec print_expression_prec prec expr =
  match expr with
  | ExpressionNil -> "nil"
  | ExpressionBool true -> "true"
  | ExpressionBool false -> "false"
  | ExpressionNumber n ->
    if Float.is_integer n then
      Printf.sprintf "%.0f" n
    else
      Printf.sprintf "%g" n
  | ExpressionString s ->
    Printf.sprintf "\"%s\"" (escape_string s)
  | ExpressionVariable name -> name
  | ExpressionTable fields ->
    let field_strs = List.map print_table_field fields in
    "{" ^ String.concat ", " field_strs ^ "}"
  | ExpressionIndex (obj, idx) ->
    Printf.sprintf "%s[%s]" (print_expression_prec 100 obj) (print_expression idx)
  | ExpressionField (obj, field) ->
    Printf.sprintf "%s.%s" (print_expression_prec 100 obj) field
  | ExpressionCall (func, args) ->
    let func_str = match func with
      | ExpressionFunction _ ->
        "(" ^ print_expression_prec 0 func ^ ")"
      | _ ->
        print_expression_prec 100 func
    in
    let arg_strs = List.map print_expression args in
    Printf.sprintf "%s(%s)" func_str (String.concat ", " arg_strs)
  | ExpressionBinaryOp (op, left, right) ->
    let op_prec = operator_precedence op in
    let left_str = print_expression_prec op_prec left in
    let right_str = print_expression_prec (op_prec + 1) right in
    let result = Printf.sprintf "%s %s %s" left_str (binary_operator_to_string op) right_str in
    if op_prec < prec then "(" ^ result ^ ")" else result
  | ExpressionUnaryOp (op, operand) ->
    let operand_str = print_expression_prec 90 operand in
    Printf.sprintf "%s%s" (unary_operator_to_string op) operand_str
  | ExpressionFunction (params, body) ->
    let params_str = String.concat ", " params in
    let body_str = print_block 1 body in
    Printf.sprintf "function(%s)\n%s\nend" params_str body_str

and operator_precedence = function
  | OpOr -> 10
  | OpAnd -> 20
  | OpEqual | OpNotEqual | OpLess | OpGreater | OpLessEqual | OpGreaterEqual -> 30
  | OpConcat -> 40
  | OpAdd | OpSub -> 50
  | OpMul | OpDiv | OpMod -> 60
  | OpPow -> 70

and print_table_field = function
  | FieldArray expr ->
    print_expression expr
  | FieldNamed (name, expr) ->
    Printf.sprintf "%s = %s" name (print_expression expr)
  | FieldIndexed (key, value) ->
    Printf.sprintf "[%s] = %s" (print_expression key) (print_expression value)

and print_expression expr = print_expression_prec 0 expr

and print_lvalue = function
  | LvalueVariable name -> name
  | LvalueIndex (obj, idx) ->
    Printf.sprintf "%s[%s]" (print_expression obj) (print_expression idx)
  | LvalueField (obj, field) ->
    Printf.sprintf "%s.%s" (print_expression obj) field

and print_statement_at_level level stmt =
  let indent = indent_string level in
  match stmt with
  | StatementLocal (names, []) ->
    Printf.sprintf "%slocal %s" indent (String.concat ", " names)
  | StatementLocal (names, values) ->
    let names_str = String.concat ", " names in
    let values_str = String.concat ", " (List.map print_expression values) in
    Printf.sprintf "%slocal %s = %s" indent names_str values_str
  | StatementAssign (lvalues, values) ->
    let lvalues_str = String.concat ", " (List.map print_lvalue lvalues) in
    let values_str = String.concat ", " (List.map print_expression values) in
    Printf.sprintf "%s%s = %s" indent lvalues_str values_str
  | StatementCall (func, args) ->
    let func_str = print_expression func in
    let args_str = String.concat ", " (List.map print_expression args) in
    Printf.sprintf "%s%s(%s)" indent func_str args_str
  | StatementIf (branches, else_block) ->
    let first_branch = match branches with
      | [] -> ""
      | (cond, block) :: rest ->
        let cond_str = print_expression cond in
        let block_str = print_block (level + 1) block in
        let first = Printf.sprintf "%sif %s then\n%s" indent cond_str block_str in
        let elseifs = List.map (fun (cond, block) ->
          let cond_str = print_expression cond in
          let block_str = print_block (level + 1) block in
          Printf.sprintf "\n%selseif %s then\n%s" indent cond_str block_str
        ) rest in
        first ^ String.concat "" elseifs
    in
    let else_str = match else_block with
      | None -> ""
      | Some block ->
        let block_str = print_block (level + 1) block in
        Printf.sprintf "\n%selse\n%s" indent block_str
    in
    Printf.sprintf "%s%s\n%send" first_branch else_str indent
  | StatementWhile (cond, block) ->
    let cond_str = print_expression cond in
    let block_str = print_block (level + 1) block in
    Printf.sprintf "%swhile %s do\n%s\n%send" indent cond_str block_str indent
  | StatementReturn [] ->
    Printf.sprintf "%sreturn" indent
  | StatementReturn exprs ->
    let exprs_str = String.concat ", " (List.map print_expression exprs) in
    Printf.sprintf "%sreturn %s" indent exprs_str
  | StatementBreak ->
    Printf.sprintf "%sbreak" indent
  | StatementDo block ->
    let block_str = print_block (level + 1) block in
    Printf.sprintf "%sdo\n%s\n%send" indent block_str indent
  | StatementLocalFunction (name, params, body) ->
    let params_str = String.concat ", " params in
    let body_str = print_block (level + 1) body in
    Printf.sprintf "%slocal function %s(%s)\n%s\n%send" indent name params_str body_str indent
  | StatementFunction (name, params, body) ->
    let params_str = String.concat ", " params in
    let body_str = print_block (level + 1) body in
    Printf.sprintf "%sfunction %s(%s)\n%s\n%send" indent name params_str body_str indent

and print_block level stmts =
  let stmt_strs = List.map (print_statement_at_level level) stmts in
  String.concat "\n" stmt_strs

let print_statement stmt = print_statement_at_level 0 stmt

let print_chunk chunk = print_block 0 chunk
