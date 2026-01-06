%{
open Common
open Syntax_tree

let make_loc (start_pos, end_pos) =
  Location.from_lexing_positions start_pos end_pos

let make_located value loc = Location.{ value; location = make_loc loc }

let make_binding pattern expression loc =
  { binding_pattern = pattern; binding_expression = expression; binding_location = make_loc loc }
%}

%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> LOWERCASE_IDENTIFIER
%token <string> UPPERCASE_IDENTIFIER
%token <string> TYPE_VARIABLE
%token TRUE FALSE
%token LET REC IN FUN IF THEN ELSE TYPE OF AND AS
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON COLON ARROW EQUAL BAR UNDERSCORE
%token STAR PLUS MINUS SLASH
%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL NOT_EQUAL
%token EOF

%right ARROW
%right SEMICOLON
%nonassoc IN
%left BAR
%nonassoc AS
%left COMMA
%nonassoc THEN
%nonassoc ELSE
%left EQUAL_EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left STAR SLASH
%nonassoc unary_minus

%start <Syntax_tree.structure> structure
%start <Syntax_tree.expression> expression_eof

%%

structure:
  | items = list(structure_item); EOF { items }

structure_item:
  | LET; rf = rec_flag; bindings = separated_nonempty_list(AND, let_binding)
    { make_located (StructureValue (rf, bindings)) $loc }
  | TYPE; decls = separated_nonempty_list(AND, type_declaration)
    { make_located (StructureType decls) $loc }

rec_flag:
  | { Nonrecursive }
  | REC { Recursive }

let_binding:
  | pattern = simple_pattern; EQUAL; expr = expression
    { make_binding pattern expr $loc }
  | name = LOWERCASE_IDENTIFIER; params = nonempty_list(simple_pattern); EQUAL; expr = expression
    {
      let func_expr = make_located (ExpressionFunction (params, expr)) $loc in
      let name_pattern = make_located (PatternVariable name) $loc(name) in
      make_binding name_pattern func_expr $loc
    }

type_declaration:
  | params = type_parameters; name = LOWERCASE_IDENTIFIER; EQUAL; kind = type_declaration_kind
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = kind;
        type_location = make_loc $loc }
    }

type_parameters:
  | { [] }
  | v = TYPE_VARIABLE { [v] }
  | LPAREN; params = separated_nonempty_list(COMMA, TYPE_VARIABLE); RPAREN { params }

type_declaration_kind:
  | constructors = separated_nonempty_list(BAR, constructor_declaration)
    { TypeVariant constructors }
  | BAR; constructors = separated_nonempty_list(BAR, constructor_declaration)
    { TypeVariant constructors }

constructor_declaration:
  | name = UPPERCASE_IDENTIFIER
    { { constructor_name = make_located name $loc(name); constructor_argument = None } }
  | name = UPPERCASE_IDENTIFIER; OF; ty = type_expression
    { { constructor_name = make_located name $loc(name); constructor_argument = Some ty } }

type_expression:
  | t = simple_type_expression { t }
  | t1 = type_expression; ARROW; t2 = type_expression
    { make_located (TypeArrow (t1, t2)) $loc }
  | ts = tuple_type_expression
    { make_located (TypeTuple ts) $loc }

tuple_type_expression:
  | t1 = simple_type_expression; STAR; t2 = simple_type_expression
    { [t1; t2] }
  | ts = tuple_type_expression; STAR; t = simple_type_expression
    { ts @ [t] }

simple_type_expression:
  | var = TYPE_VARIABLE
    { make_located (TypeVariable var) $loc }
  | name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (name, [])) $loc }
  | arg = simple_type_expression; name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (name, [arg])) $loc }
  | LPAREN; args = separated_list(COMMA, type_expression); RPAREN; name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (name, args)) $loc }
  | LPAREN; t = type_expression; RPAREN
    { t }

expression_eof:
  | e = expression; EOF { e }

expression:
  | e = simple_expression { e }
  | LET; rf = rec_flag; bindings = separated_nonempty_list(AND, let_binding); IN; body = expression
    { make_located (ExpressionLet (rf, bindings, body)) $loc }
  | FUN; params = nonempty_list(simple_pattern); ARROW; body = expression
    { make_located (ExpressionFunction (params, body)) $loc }
  | IF; cond = expression; THEN; then_branch = expression; ELSE; else_branch = expression
    { make_located (ExpressionIf (cond, then_branch, Some else_branch)) $loc }
  | IF; cond = expression; THEN; then_branch = expression %prec THEN
    { make_located (ExpressionIf (cond, then_branch, None)) $loc }
  | e1 = expression; SEMICOLON; e2 = expression
    { make_located (ExpressionSequence (e1, e2)) $loc }
  | e = expression; COLON; t = type_expression
    { make_located (ExpressionConstraint (e, t)) $loc }

simple_expression:
  | e = application_expression { e }
  | MINUS; e = simple_expression %prec unary_minus
    {
      let zero = make_located (ExpressionConstant (ConstantInteger 0)) $loc in
      let minus = make_located (ExpressionVariable "-") $loc in
      make_located (ExpressionApply (minus, [zero; e])) $loc
    }
  | e1 = simple_expression; op = binary_operator; e2 = simple_expression
    {
      let op_expr = make_located (ExpressionVariable op) $loc in
      make_located (ExpressionApply (op_expr, [e1; e2])) $loc
    }

%inline binary_operator:
  | PLUS { "+" }
  | MINUS { "-" }
  | STAR { "*" }
  | SLASH { "/" }
  | EQUAL_EQUAL { "==" }
  | NOT_EQUAL { "!=" }
  | LESS { "<" }
  | GREATER { ">" }
  | LESS_EQUAL { "<=" }
  | GREATER_EQUAL { ">=" }

application_expression:
  | e = atomic_expression { e }
  | func = application_expression; arg = atomic_expression
    {
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [arg])) $loc
      | _ -> make_located (ExpressionApply (func, [arg])) $loc
    }

atomic_expression:
  | LPAREN; RPAREN
    { make_located (ExpressionConstant ConstantUnit) $loc }
  | LPAREN; e = expression; RPAREN
    { e }
  | LPAREN; es = expression_tuple; RPAREN
    { make_located (ExpressionTuple es) $loc }
  | n = INTEGER
    { make_located (ExpressionConstant (ConstantInteger n)) $loc }
  | f = FLOAT
    { make_located (ExpressionConstant (ConstantFloat f)) $loc }
  | s = STRING
    { make_located (ExpressionConstant (ConstantString s)) $loc }
  | TRUE
    { make_located (ExpressionConstant (ConstantBoolean true)) $loc }
  | FALSE
    { make_located (ExpressionConstant (ConstantBoolean false)) $loc }
  | name = LOWERCASE_IDENTIFIER
    { make_located (ExpressionVariable name) $loc }
  | name = UPPERCASE_IDENTIFIER
    { make_located (ExpressionConstructor (name, None)) $loc }
  | name = UPPERCASE_IDENTIFIER; arg = atomic_expression
    { make_located (ExpressionConstructor (name, Some arg)) $loc }

expression_tuple:
  | e1 = expression; COMMA; e2 = expression
    { [e1; e2] }
  | es = expression_tuple; COMMA; e = expression
    { es @ [e] }

simple_pattern:
  | p = atomic_pattern { p }
  | p = simple_pattern; AS; name = LOWERCASE_IDENTIFIER
    { make_located (PatternAlias (p, name)) $loc }

atomic_pattern:
  | UNDERSCORE
    { make_located PatternWildcard $loc }
  | LPAREN; RPAREN
    { make_located (PatternConstant ConstantUnit) $loc }
  | LPAREN; p = pattern; RPAREN
    { p }
  | LPAREN; ps = pattern_tuple; RPAREN
    { make_located (PatternTuple ps) $loc }
  | n = INTEGER
    { make_located (PatternConstant (ConstantInteger n)) $loc }
  | s = STRING
    { make_located (PatternConstant (ConstantString s)) $loc }
  | TRUE
    { make_located (PatternConstant (ConstantBoolean true)) $loc }
  | FALSE
    { make_located (PatternConstant (ConstantBoolean false)) $loc }
  | name = LOWERCASE_IDENTIFIER
    { make_located (PatternVariable name) $loc }
  | name = UPPERCASE_IDENTIFIER
    { make_located (PatternConstructor (name, None)) $loc }
  | name = UPPERCASE_IDENTIFIER; p = atomic_pattern
    { make_located (PatternConstructor (name, Some p)) $loc }

pattern:
  | p = simple_pattern { p }
  | p = pattern; COLON; t = type_expression
    { make_located (PatternConstraint (p, t)) $loc }

pattern_tuple:
  | p1 = pattern; COMMA; p2 = pattern
    { [p1; p2] }
  | ps = pattern_tuple; COMMA; p = pattern
    { ps @ [p] }
