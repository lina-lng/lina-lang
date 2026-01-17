(** Lua AST pretty printer.

    Uses Buffer-based internal functions for O(n) string building. *)

open Lua_ast

(** {1 Helper Functions} *)

let add_indent buf level =
  for _ = 1 to level * 2 do
    Buffer.add_char buf ' '
  done

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

let operator_precedence = function
  | OpOr -> 10
  | OpAnd -> 20
  | OpEqual | OpNotEqual | OpLess | OpGreater | OpLessEqual | OpGreaterEqual -> 30
  | OpConcat -> 40
  | OpAdd | OpSub -> 50
  | OpMul | OpDiv | OpMod -> 60
  | OpPow -> 70

(** Print items with a separator between them. *)
let print_separated buf ~sep ~print items =
  match items with
  | [] -> ()
  | first :: rest ->
      print buf first;
      List.iter (fun item ->
        Buffer.add_string buf sep;
        print buf item
      ) rest

(** {1 Buffer-Based Printing Functions} *)

let rec print_expression_prec_to_buf buf prec expr =
  match expr with
  | ExpressionNil ->
    Buffer.add_string buf "nil"
  | ExpressionBool true ->
    Buffer.add_string buf "true"
  | ExpressionBool false ->
    Buffer.add_string buf "false"
  | ExpressionNumber n ->
    if Float.is_integer n then
      Printf.bprintf buf "%.0f" n
    else
      Printf.bprintf buf "%g" n
  | ExpressionString s ->
    Buffer.add_char buf '"';
    Buffer.add_string buf (escape_string s);
    Buffer.add_char buf '"'
  | ExpressionVariable name ->
    Buffer.add_string buf name
  | ExpressionTable fields ->
    Buffer.add_char buf '{';
    print_table_fields_to_buf buf fields;
    Buffer.add_char buf '}'
  | ExpressionIndex (obj, idx) ->
    print_expression_prec_to_buf buf 100 obj;
    Buffer.add_char buf '[';
    print_expression_to_buf buf idx;
    Buffer.add_char buf ']'
  | ExpressionField (obj, field) ->
    print_expression_prec_to_buf buf 100 obj;
    Buffer.add_char buf '.';
    Buffer.add_string buf field
  | ExpressionCall (func, args) ->
    begin match func with
    | ExpressionFunction _ ->
      Buffer.add_char buf '(';
      print_expression_prec_to_buf buf 0 func;
      Buffer.add_char buf ')'
    | _ ->
      print_expression_prec_to_buf buf 100 func
    end;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf args;
    Buffer.add_char buf ')'
  | ExpressionMethodCall (obj, method_name, args) ->
    print_expression_prec_to_buf buf 100 obj;
    Buffer.add_char buf ':';
    Buffer.add_string buf method_name;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf args;
    Buffer.add_char buf ')'
  | ExpressionBinaryOp (op, left, right) ->
    let op_prec = operator_precedence op in
    let needs_parens = op_prec < prec in
    if needs_parens then Buffer.add_char buf '(';
    print_expression_prec_to_buf buf op_prec left;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (binary_operator_to_string op);
    Buffer.add_char buf ' ';
    print_expression_prec_to_buf buf (op_prec + 1) right;
    if needs_parens then Buffer.add_char buf ')'
  | ExpressionUnaryOp (op, operand) ->
    Buffer.add_string buf (unary_operator_to_string op);
    print_expression_prec_to_buf buf 90 operand
  | ExpressionFunction (params, body) ->
    Buffer.add_string buf "function(";
    print_string_list_to_buf buf params;
    Buffer.add_string buf ")\n";
    print_block_to_buf buf 1 body;
    Buffer.add_string buf "\nend"

and print_expression_to_buf buf expr =
  print_expression_prec_to_buf buf 0 expr

and print_expressions_to_buf buf exprs =
  print_separated buf ~sep:", " ~print:print_expression_to_buf exprs

and print_string_list_to_buf buf strings =
  print_separated buf ~sep:", " ~print:Buffer.add_string strings

and print_table_fields_to_buf buf fields =
  print_separated buf ~sep:", " ~print:print_table_field_to_buf fields

and print_table_field_to_buf buf field =
  match field with
  | FieldArray expr ->
    print_expression_to_buf buf expr
  | FieldNamed (name, expr) ->
    Buffer.add_string buf name;
    Buffer.add_string buf " = ";
    print_expression_to_buf buf expr
  | FieldIndexed (key, value) ->
    Buffer.add_char buf '[';
    print_expression_to_buf buf key;
    Buffer.add_string buf "] = ";
    print_expression_to_buf buf value

and print_lvalue_to_buf buf lvalue =
  match lvalue with
  | LvalueVariable name ->
    Buffer.add_string buf name
  | LvalueIndex (obj, idx) ->
    print_expression_to_buf buf obj;
    Buffer.add_char buf '[';
    print_expression_to_buf buf idx;
    Buffer.add_char buf ']'
  | LvalueField (obj, field) ->
    print_expression_to_buf buf obj;
    Buffer.add_char buf '.';
    Buffer.add_string buf field

and print_lvalues_to_buf buf lvalues =
  print_separated buf ~sep:", " ~print:print_lvalue_to_buf lvalues

and print_statement_to_buf buf level stmt =
  add_indent buf level;
  match stmt with
  | StatementLocal (names, []) ->
    Buffer.add_string buf "local ";
    print_string_list_to_buf buf names
  | StatementLocal (names, values) ->
    Buffer.add_string buf "local ";
    print_string_list_to_buf buf names;
    Buffer.add_string buf " = ";
    print_expressions_to_buf buf values
  | StatementAssign (lvalues, values) ->
    print_lvalues_to_buf buf lvalues;
    Buffer.add_string buf " = ";
    print_expressions_to_buf buf values
  | StatementCall (func, args) ->
    begin match func with
    | ExpressionFunction _ ->
      Buffer.add_char buf '(';
      print_expression_to_buf buf func;
      Buffer.add_char buf ')'
    | _ ->
      print_expression_to_buf buf func
    end;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf args;
    Buffer.add_char buf ')'
  | StatementIf (branches, else_block) ->
    print_if_branches_to_buf buf level branches;
    begin match else_block with
    | None -> ()
    | Some block ->
      Buffer.add_char buf '\n';
      add_indent buf level;
      Buffer.add_string buf "else\n";
      print_block_to_buf buf (level + 1) block
    end;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementWhile (cond, block) ->
    Buffer.add_string buf "while ";
    print_expression_to_buf buf cond;
    Buffer.add_string buf " do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementForNum (var, start_expr, end_expr, step_opt, block) ->
    Buffer.add_string buf "for ";
    Buffer.add_string buf var;
    Buffer.add_string buf " = ";
    print_expression_to_buf buf start_expr;
    Buffer.add_string buf ", ";
    print_expression_to_buf buf end_expr;
    (match step_opt with
     | Some step ->
       Buffer.add_string buf ", ";
       print_expression_to_buf buf step
     | None -> ());
    Buffer.add_string buf " do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementForIn (names, iterator_expressions, block) ->
    Buffer.add_string buf "for ";
    print_string_list_to_buf buf names;
    Buffer.add_string buf " in ";
    print_expressions_to_buf buf iterator_expressions;
    Buffer.add_string buf " do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementReturn [] ->
    Buffer.add_string buf "return"
  | StatementReturn exprs ->
    Buffer.add_string buf "return ";
    print_expressions_to_buf buf exprs
  | StatementBreak ->
    Buffer.add_string buf "break"
  | StatementDo block ->
    Buffer.add_string buf "do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementLocalFunction (name, params, body) ->
    Buffer.add_string buf "local function ";
    Buffer.add_string buf name;
    Buffer.add_char buf '(';
    print_string_list_to_buf buf params;
    Buffer.add_string buf ")\n";
    print_block_to_buf buf (level + 1) body;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementFunction (name, params, body) ->
    Buffer.add_string buf "function ";
    Buffer.add_string buf name;
    Buffer.add_char buf '(';
    print_string_list_to_buf buf params;
    Buffer.add_string buf ")\n";
    print_block_to_buf buf (level + 1) body;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"

and print_if_branches_to_buf buf level branches =
  match branches with
  | [] -> ()
  | (cond, block) :: rest ->
    Buffer.add_string buf "if ";
    print_expression_to_buf buf cond;
    Buffer.add_string buf " then\n";
    print_block_to_buf buf (level + 1) block;
    List.iter (fun (cond, block) ->
      Buffer.add_char buf '\n';
      add_indent buf level;
      Buffer.add_string buf "elseif ";
      print_expression_to_buf buf cond;
      Buffer.add_string buf " then\n";
      print_block_to_buf buf (level + 1) block
    ) rest

and print_block_to_buf buf level stmts =
  let print_stmt buf stmt = print_statement_to_buf buf level stmt in
  (* Use semicolon separator to avoid Lua's ambiguous parsing of
     "expr\n(..." as a function call. Without semicolons, "print(x)\n(function()...end)()"
     would be parsed as "print(x)(function()...end)()" calling nil. *)
  print_separated buf ~sep:";\n" ~print:print_stmt stmts

(** {1 Public API} *)

let print_expression expr =
  let buf = Buffer.create 256 in
  print_expression_prec_to_buf buf 0 expr;
  Buffer.contents buf

let print_statement stmt =
  let buf = Buffer.create 256 in
  print_statement_to_buf buf 0 stmt;
  Buffer.contents buf

let print_chunk chunk =
  let buf = Buffer.create 4096 in
  print_block_to_buf buf 0 chunk;
  Buffer.contents buf
