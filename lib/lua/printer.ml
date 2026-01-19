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

let is_valid_lua_identifier name =
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let is_digit c = c >= '0' && c <= '9' in
  let is_ident_char c = is_alpha c || is_digit c || c = '_' in
  let is_ident_start c = is_alpha c || c = '_' in

  String.length name > 0
  && is_ident_start name.[0]
  && String.for_all is_ident_char name

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

let rec print_expression_prec_to_buf buf level prec expr =
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
    print_table_fields_to_buf buf level fields;
    Buffer.add_char buf '}'
  | ExpressionIndex (obj, idx) ->
    print_expression_prec_to_buf buf level 100 obj;
    Buffer.add_char buf '[';
    print_expression_to_buf buf level idx;
    Buffer.add_char buf ']'
  | ExpressionField (obj, field) ->
    print_expression_prec_to_buf buf level 100 obj;
    if is_valid_lua_identifier field then begin
      Buffer.add_char buf '.';
      Buffer.add_string buf field
    end else begin
      Buffer.add_string buf "[\"";
      Buffer.add_string buf (escape_string field);
      Buffer.add_string buf "\"]"
    end
  | ExpressionCall (func, args) ->
    begin match func with
    | ExpressionFunction _ ->
      Buffer.add_char buf '(';
      print_expression_prec_to_buf buf level 0 func;
      Buffer.add_char buf ')'
    | _ ->
      print_expression_prec_to_buf buf level 100 func
    end;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf level args;
    Buffer.add_char buf ')'
  | ExpressionMethodCall (obj, method_name, args) ->
    print_expression_prec_to_buf buf level 100 obj;
    Buffer.add_char buf ':';
    Buffer.add_string buf method_name;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf level args;
    Buffer.add_char buf ')'
  | ExpressionBinaryOp (op, left, right) ->
    let op_prec = operator_precedence op in
    let needs_parens = op_prec < prec in
    if needs_parens then Buffer.add_char buf '(';
    print_expression_prec_to_buf buf level op_prec left;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (binary_operator_to_string op);
    Buffer.add_char buf ' ';
    print_expression_prec_to_buf buf level (op_prec + 1) right;
    if needs_parens then Buffer.add_char buf ')'
  | ExpressionUnaryOp (op, operand) ->
    Buffer.add_string buf (unary_operator_to_string op);
    print_expression_prec_to_buf buf level 90 operand
  | ExpressionFunction (params, body) ->
    Buffer.add_string buf "function(";
    print_string_list_to_buf buf params;
    Buffer.add_string buf ")\n";
    print_block_to_buf buf (level + 1) body;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"

and print_expression_to_buf buf level expr =
  print_expression_prec_to_buf buf level 0 expr

and print_expressions_to_buf buf level exprs =
  print_separated buf ~sep:", " ~print:(fun buf expr -> print_expression_to_buf buf level expr) exprs

and print_string_list_to_buf buf strings =
  print_separated buf ~sep:", " ~print:Buffer.add_string strings

and print_table_fields_to_buf buf level fields =
  print_separated buf ~sep:", " ~print:(fun buf field -> print_table_field_to_buf buf level field) fields

and print_table_field_to_buf buf level field =
  match field with
  | FieldArray expr ->
    print_expression_to_buf buf level expr
  | FieldNamed (name, expr) ->
    Buffer.add_string buf name;
    Buffer.add_string buf " = ";
    print_expression_to_buf buf level expr
  | FieldIndexed (key, value) ->
    Buffer.add_char buf '[';
    print_expression_to_buf buf level key;
    Buffer.add_string buf "] = ";
    print_expression_to_buf buf level value

and print_lvalue_to_buf buf level lvalue =
  match lvalue with
  | LvalueVariable name ->
    Buffer.add_string buf name
  | LvalueIndex (obj, idx) ->
    print_expression_to_buf buf level obj;
    Buffer.add_char buf '[';
    print_expression_to_buf buf level idx;
    Buffer.add_char buf ']'
  | LvalueField (obj, field) ->
    print_expression_to_buf buf level obj;
    if is_valid_lua_identifier field then begin
      Buffer.add_char buf '.';
      Buffer.add_string buf field
    end else begin
      Buffer.add_string buf "[\"";
      Buffer.add_string buf (escape_string field);
      Buffer.add_string buf "\"]"
    end

and print_lvalues_to_buf buf level lvalues =
  print_separated buf ~sep:", " ~print:(fun buf lv -> print_lvalue_to_buf buf level lv) lvalues

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
    print_expressions_to_buf buf level values
  | StatementAssign (lvalues, values) ->
    print_lvalues_to_buf buf level lvalues;
    Buffer.add_string buf " = ";
    print_expressions_to_buf buf level values
  | StatementCall (func, args) ->
    begin match func with
    | ExpressionFunction _ ->
      Buffer.add_char buf '(';
      print_expression_to_buf buf level func;
      Buffer.add_char buf ')'
    | _ ->
      print_expression_to_buf buf level func
    end;
    Buffer.add_char buf '(';
    print_expressions_to_buf buf level args;
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
    print_expression_to_buf buf level cond;
    Buffer.add_string buf " do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementForNum (var, start_expr, end_expr, step_opt, block) ->
    Buffer.add_string buf "for ";
    Buffer.add_string buf var;
    Buffer.add_string buf " = ";
    print_expression_to_buf buf level start_expr;
    Buffer.add_string buf ", ";
    print_expression_to_buf buf level end_expr;
    (match step_opt with
     | Some step ->
       Buffer.add_string buf ", ";
       print_expression_to_buf buf level step
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
    print_expressions_to_buf buf level iterator_expressions;
    Buffer.add_string buf " do\n";
    print_block_to_buf buf (level + 1) block;
    Buffer.add_char buf '\n';
    add_indent buf level;
    Buffer.add_string buf "end"
  | StatementReturn [] ->
    Buffer.add_string buf "return"
  | StatementReturn exprs ->
    Buffer.add_string buf "return ";
    print_expressions_to_buf buf level exprs
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
    print_expression_to_buf buf level cond;
    Buffer.add_string buf " then\n";
    print_block_to_buf buf (level + 1) block;
    List.iter (fun (cond, block) ->
      Buffer.add_char buf '\n';
      add_indent buf level;
      Buffer.add_string buf "elseif ";
      print_expression_to_buf buf level cond;
      Buffer.add_string buf " then\n";
      print_block_to_buf buf (level + 1) block
    ) rest

(** Check if a statement defines a function (for blank line insertion). *)
and is_function_definition stmt =
  match stmt with
  | StatementLocalFunction _ | StatementFunction _ -> true
  | StatementLocal (_, [ExpressionFunction _]) -> true
  | StatementAssign ([LvalueVariable _], [ExpressionFunction _]) -> true
  | _ -> false

and print_block_to_buf buf level stmts =
  let print_stmt buf stmt = print_statement_to_buf buf level stmt in

  (* Print statements with appropriate separators:
     - Blank line between function definitions for readability
     - Semicolon between statement and IIFE/expression-starting to avoid Lua ambiguity *)
  match stmts with
  | [] -> ()
  | first :: rest ->
    print_stmt buf first;
    let rec print_rest prev remaining =
      match remaining with
      | [] -> ()
      | next :: tail ->
        let needs_blank_line = is_function_definition prev && is_function_definition next in
        if needs_blank_line then
          Buffer.add_string buf ";\n\n"
        else
          Buffer.add_string buf ";\n";
        print_stmt buf next;
        print_rest next tail
    in
    print_rest first rest

(** {1 Public API} *)

let print_expression expr =
  let buf = Buffer.create 256 in
  print_expression_prec_to_buf buf 0 0 expr;
  Buffer.contents buf

let print_statement stmt =
  let buf = Buffer.create 256 in
  print_statement_to_buf buf 0 stmt;
  Buffer.contents buf

let print_chunk chunk =
  let buf = Buffer.create 4096 in
  print_block_to_buf buf 0 chunk;
  Buffer.contents buf
