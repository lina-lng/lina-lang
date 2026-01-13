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
%token LET REC IN FUN IF THEN ELSE TYPE OF AND AS MATCH WITH WHEN
%token MODULE STRUCT END SIG FUNCTOR OPEN INCLUDE VAL PRIVATE CONSTRAINT
%token EXTERNAL AT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA SEMICOLON COLON DOT DOTDOT ARROW EQUAL BAR UNDERSCORE
%token STAR PLUS MINUS SLASH
%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL NOT_EQUAL
%token REF BANG COLONEQUALS
%token <string> BACKTICK_TAG
%token EOF

%right ARROW
%right COLONEQUALS
%right SEMICOLON
%nonassoc IN
%nonassoc WITH
%left BAR
%nonassoc WHEN
%nonassoc AS
%left COMMA
%nonassoc THEN
%nonassoc ELSE
%left EQUAL_EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left STAR SLASH
%nonassoc unary_minus
%nonassoc REF BANG
%left DOT
%nonassoc APP

%start <Syntax_tree.structure> structure
%start <Syntax_tree.expression> expression_eof

(* Error recovery: reduce these symbols before reporting errors *)
%on_error_reduce expression
%on_error_reduce simple_expression
%on_error_reduce application_expression
%on_error_reduce postfix_expression
%on_error_reduce atomic_expression
%on_error_reduce pattern
%on_error_reduce simple_pattern
%on_error_reduce type_expression
%on_error_reduce let_binding

%%

structure:
  | items = list(structure_item); EOF { items }

structure_item:
  | LET; rf = rec_flag; bindings = separated_nonempty_list(AND, let_binding)
    { make_located (StructureValue (rf, bindings)) $loc }
  | TYPE; decls = separated_nonempty_list(AND, type_declaration)
    { make_located (StructureType decls) $loc }
  | MODULE; binding = module_binding
    { make_located (StructureModule binding) $loc }
  | MODULE; TYPE; name = UPPERCASE_IDENTIFIER; EQUAL; mt = module_type
    { make_located (StructureModuleType (make_located name $loc(name), mt)) $loc }
  | OPEN; path = module_path
    { make_located (StructureOpen path) $loc }
  | INCLUDE; me = module_expression
    { make_located (StructureInclude me) $loc }
  | ext = external_declaration
    { make_located (StructureExternal ext) $loc }

rec_flag:
  | { Nonrecursive }
  | REC { Recursive }

let_binding:
  | pattern = simple_pattern; EQUAL; expr = expression
    { make_binding pattern expr $loc }
  (* let x : type = expr - type constraint on binding *)
  | pattern = simple_pattern; COLON; ty = type_expression; EQUAL; expr = expression
    {
      let constrained_pattern = make_located (PatternConstraint (pattern, ty)) $loc(pattern) in
      make_binding constrained_pattern expr $loc
    }
  | name = LOWERCASE_IDENTIFIER; params = nonempty_list(simple_pattern); EQUAL; expr = expression
    {
      let func_expr = make_located (ExpressionFunction (params, expr)) $loc in
      let name_pattern = make_located (PatternVariable name) $loc(name) in
      make_binding name_pattern func_expr $loc
    }
  (* let f x y : return_type = expr - return type annotation on function *)
  | name = LOWERCASE_IDENTIFIER; params = nonempty_list(simple_pattern); COLON; ty = type_expression; EQUAL; expr = expression
    {
      let constrained_expr = make_located (ExpressionConstraint (expr, ty)) $loc(expr) in
      let func_expr = make_located (ExpressionFunction (params, constrained_expr)) $loc in
      let name_pattern = make_located (PatternVariable name) $loc(name) in
      make_binding name_pattern func_expr $loc
    }

type_declaration:
  | params = type_parameters; name = LOWERCASE_IDENTIFIER; EQUAL; kind = type_declaration_kind; constraints = list(type_constraint)
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = kind;
        type_private = false;
        type_constraints = constraints;
        type_location = make_loc $loc }
    }
  | params = type_parameters; name = LOWERCASE_IDENTIFIER; EQUAL; PRIVATE; kind = type_declaration_kind; constraints = list(type_constraint)
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = kind;
        type_private = true;
        type_constraints = constraints;
        type_location = make_loc $loc }
    }
  | params = type_parameters; name = LOWERCASE_IDENTIFIER
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = TypeAbstract;
        type_private = false;
        type_constraints = [];
        type_location = make_loc $loc }
    }

(* Type parameter constraint: constraint 'a = type_expression *)
type_constraint:
  | CONSTRAINT; var = TYPE_VARIABLE; EQUAL; ty = type_expression
    {
      { Syntax_tree.constraint_variable = var;
        constraint_type = ty;
        constraint_location = make_loc $loc }
    }

(* Type declaration in signatures - allows abstract types (no = definition) *)
signature_type_declaration:
  | params = type_parameters; name = LOWERCASE_IDENTIFIER; EQUAL; kind = type_declaration_kind; constraints = list(type_constraint)
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = kind;
        type_private = false;
        type_constraints = constraints;
        type_location = make_loc $loc }
    }
  | params = type_parameters; name = LOWERCASE_IDENTIFIER; EQUAL; PRIVATE; kind = type_declaration_kind; constraints = list(type_constraint)
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = kind;
        type_private = true;
        type_constraints = constraints;
        type_location = make_loc $loc }
    }
  | params = type_parameters; name = LOWERCASE_IDENTIFIER
    {
      { type_name = make_located name $loc(name);
        type_parameters = params;
        type_kind = TypeAbstract;
        type_private = false;
        type_constraints = [];
        type_location = make_loc $loc }
    }

(* Type parameter with optional variance annotation: 'a, +'a, -'a, _ *)
type_param:
  | PLUS; v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = Some VarianceCovariant } }
  | MINUS; v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = Some VarianceContravariant } }
  | v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = None } }
  (* Wildcard type parameter for GADTs: type _ t = ... *)
  | UNDERSCORE
    { { Syntax_tree.parameter_name = "_"; parameter_variance = None } }

type_parameters:
  | { [] }
  | p = type_param { [p] }
  | LPAREN; params = separated_nonempty_list(COMMA, type_param); RPAREN { params }

type_declaration_kind:
  | constructors = separated_nonempty_list(BAR, constructor_declaration)
    { TypeVariant constructors }
  | BAR; constructors = separated_nonempty_list(BAR, constructor_declaration)
    { TypeVariant constructors }
  | ty = type_expression
    { TypeAlias ty }

constructor_declaration:
  (* Standard constructor: Name *)
  | name = UPPERCASE_IDENTIFIER
    { { constructor_name = make_located name $loc(name);
        constructor_argument = None;
        constructor_return_type = None } }
  (* Standard constructor with argument: Name of type *)
  | name = UPPERCASE_IDENTIFIER; OF; ty = type_expression
    { { constructor_name = make_located name $loc(name);
        constructor_argument = Some ty;
        constructor_return_type = None } }
  (* GADT constructor without argument: Name : return_type *)
  | name = UPPERCASE_IDENTIFIER; COLON; ret = type_expression
    { { constructor_name = make_located name $loc(name);
        constructor_argument = None;
        constructor_return_type = Some ret } }
  (* GADT constructor with argument: Name : arg_type -> return_type *)
  | name = UPPERCASE_IDENTIFIER; COLON; arg = type_expression; ARROW; ret = type_expression
    { { constructor_name = make_located name $loc(name);
        constructor_argument = Some arg;
        constructor_return_type = Some ret } }

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
  | LBRACE; fields = separated_list(SEMICOLON, type_record_field); RBRACE
    { make_located (TypeRecord (fields, false)) $loc }
  | LBRACE; fields = separated_list(SEMICOLON, type_record_field); SEMICOLON; DOTDOT; RBRACE
    { make_located (TypeRecord (fields, true)) $loc }
  (* Polymorphic variant types *)
  | LBRACKET; fields = poly_variant_fields; RBRACKET
    { make_located (TypePolyVariant (PolyRowExact fields)) $loc }
  | LBRACKET; GREATER; fields = poly_variant_fields; RBRACKET
    { make_located (TypePolyVariant (PolyRowAtLeast fields)) $loc }
  | LBRACKET; LESS; fields = poly_variant_fields; RBRACKET
    { make_located (TypePolyVariant (PolyRowAtMost (fields, []))) $loc }
  | LBRACKET; LESS; fields = poly_variant_fields; GREATER; required = separated_nonempty_list(BAR, BACKTICK_TAG); RBRACKET
    { make_located (TypePolyVariant (PolyRowAtMost (fields, required))) $loc }

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
  | MATCH; scrutinee = expression; WITH; arms = match_arms
    { make_located (ExpressionMatch (scrutinee, arms)) $loc }
  (* Reference assignment: e1 := e2 *)
  | e1 = simple_expression; COLONEQUALS; e2 = expression
    { make_located (ExpressionAssign (e1, e2)) $loc }

simple_expression:
  | e = application_expression { e }
  | MINUS; e = simple_expression %prec unary_minus
    {
      let zero = make_located (ExpressionConstant (ConstantInteger 0)) $loc in
      let minus = make_located (ExpressionVariable "-") $loc in
      make_located (ExpressionApply (minus, [zero; e])) $loc
    }
  (* Reference creation: ref e *)
  | REF; e = simple_expression %prec REF
    { make_located (ExpressionRef e) $loc }
  | e1 = simple_expression; op = binary_operator; e2 = simple_expression
    {
      let op_expr = make_located (ExpressionVariable op) $loc(op) in
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
  | e = postfix_expression { e }
  | func = application_expression; arg = postfix_expression %prec APP
    {
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [arg])) $loc
      | _ -> make_located (ExpressionApply (func, [arg])) $loc
    }

(* Postfix expressions: record/field access binds tighter than application *)
postfix_expression:
  | e = atomic_expression { e }
  | e = postfix_expression; DOT; field = LOWERCASE_IDENTIFIER
    { make_located (ExpressionRecordAccess (e, field)) $loc }
  | e = postfix_expression; DOT; field = UPPERCASE_IDENTIFIER
    (* Allow M.N for nested module access - type checker handles the semantics *)
    { make_located (ExpressionRecordAccess (e, field)) $loc }
  (* Dereference at postfix level so it can be used as function argument *)
  | BANG; e = postfix_expression %prec BANG
    { make_located (ExpressionDeref e) $loc }

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
  | LBRACE; fields = separated_list(SEMICOLON, record_field); RBRACE
    { make_located (ExpressionRecord fields) $loc }
  | LBRACE; base = simple_expression; WITH; fields = separated_nonempty_list(SEMICOLON, record_field); RBRACE
    { make_located (ExpressionRecordUpdate (base, fields)) $loc }
  (* Polymorphic variant expression: `Tag or `Tag arg *)
  | tag = BACKTICK_TAG
    { make_located (ExpressionPolyVariant (tag, None)) $loc }
  | tag = BACKTICK_TAG; arg = atomic_expression
    { make_located (ExpressionPolyVariant (tag, Some arg)) $loc }

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
  | LPAREN; TYPE; name = LOWERCASE_IDENTIFIER; RPAREN
    { make_located (PatternLocallyAbstract name) $loc }
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
  | LBRACE; fields = separated_list(SEMICOLON, record_pattern_field); RBRACE
    { make_located (PatternRecord (fields, false)) $loc }
  | LBRACE; fields = record_pattern_field_list_open; RBRACE
    { make_located (PatternRecord (fields, true)) $loc }
  (* Polymorphic variant pattern: `Tag or `Tag p *)
  | tag = BACKTICK_TAG
    { make_located (PatternPolyVariant (tag, None)) $loc }
  | tag = BACKTICK_TAG; p = atomic_pattern
    { make_located (PatternPolyVariant (tag, Some p)) $loc }

pattern:
  | p = simple_pattern { p }
  | p = pattern; COLON; t = type_expression
    { make_located (PatternConstraint (p, t)) $loc }

pattern_tuple:
  | p1 = pattern; COMMA; p2 = pattern
    { [p1; p2] }
  | ps = pattern_tuple; COMMA; p = pattern
    { ps @ [p] }

(* Record fields for expressions - use simple_expression to avoid semicolon conflict *)
record_field:
  | name = LOWERCASE_IDENTIFIER; EQUAL; value = simple_expression
    { { field_name = make_located name $loc(name); field_value = value } }
  | name = LOWERCASE_IDENTIFIER
    {
      let var_expr = make_located (ExpressionVariable name) $loc(name) in
      { field_name = make_located name $loc(name); field_value = var_expr }
    }

(* Record fields for patterns *)
record_pattern_field:
  | name = LOWERCASE_IDENTIFIER; EQUAL; p = pattern
    { { pattern_field_name = make_located name $loc(name); pattern_field_pattern = Some p } }
  | name = LOWERCASE_IDENTIFIER
    { { pattern_field_name = make_located name $loc(name); pattern_field_pattern = None } }

(* Record pattern fields with trailing ; .. for open records *)
record_pattern_field_list_open:
  | DOTDOT
    { [] }
  | field = record_pattern_field; SEMICOLON; DOTDOT
    { [field] }
  | field = record_pattern_field; SEMICOLON; rest = record_pattern_field_list_open
    { field :: rest }

(* Match arms *)
match_arms:
  | BAR?; first = match_arm; rest = list(preceded(BAR, match_arm))
    { first :: rest }

match_arm:
  | p = pattern; ARROW; e = expression
    { { arm_pattern = p; arm_guard = None; arm_expression = e; arm_location = make_loc $loc } }
  | p = pattern; WHEN; guard = expression; ARROW; e = expression
    { { arm_pattern = p; arm_guard = Some guard; arm_expression = e; arm_location = make_loc $loc } }

(* Record fields for types *)
type_record_field:
  | name = LOWERCASE_IDENTIFIER; COLON; ty = type_expression
    { { type_field_name = name; type_field_type = ty } }

(* Polymorphic variant fields for type expressions *)
poly_variant_fields:
  | BAR?; fields = separated_nonempty_list(BAR, poly_variant_field)
    { fields }

poly_variant_field:
  | tag = BACKTICK_TAG
    { { poly_variant_tag = tag; poly_variant_argument = None } }
  | tag = BACKTICK_TAG; OF; ty = type_expression
    { { poly_variant_tag = tag; poly_variant_argument = Some ty } }

(* ==================== Module System ==================== *)

(* Module paths: M or M.N.P *)
module_path:
  | names = separated_nonempty_list(DOT, UPPERCASE_IDENTIFIER)
    { make_located names $loc }

(* Module bindings: module M = ... or module F(X : S) = ... *)
module_binding:
  (* Simple: module M = ME *)
  | name = UPPERCASE_IDENTIFIER; EQUAL; expr = module_expression
    { { module_name = make_located name $loc(name);
        module_params = [];
        module_type = None;
        module_expr = expr;
        module_location = make_loc $loc } }
  (* Sealed: module M : MT = ME *)
  | name = UPPERCASE_IDENTIFIER; COLON; mt = module_type; EQUAL; expr = module_expression
    { { module_name = make_located name $loc(name);
        module_params = [];
        module_type = Some mt;
        module_expr = expr;
        module_location = make_loc $loc } }
  (* Functor shorthand: module F(X : S) = ME *)
  | name = UPPERCASE_IDENTIFIER; params = nonempty_list(functor_parameter); EQUAL; expr = module_expression
    { { module_name = make_located name $loc(name);
        module_params = params;
        module_type = None;
        module_expr = expr;
        module_location = make_loc $loc } }
  (* Functor with result type: module F(X : S) : T = ME *)
  | name = UPPERCASE_IDENTIFIER; params = nonempty_list(functor_parameter); COLON; mt = module_type; EQUAL; expr = module_expression
    { { module_name = make_located name $loc(name);
        module_params = params;
        module_type = Some mt;
        module_expr = expr;
        module_location = make_loc $loc } }

(* Functor parameters: (X : S) *)
functor_parameter:
  | LPAREN; name = UPPERCASE_IDENTIFIER; COLON; mt = module_type; RPAREN
    { { functor_param_name = make_located name $loc(name); functor_param_type = mt } }

(* Module expressions *)
module_expression:
  | me = simple_module_expression { me }
  | FUNCTOR; params = nonempty_list(functor_parameter); ARROW; body = module_expression
    { make_located (ModuleFunctor (params, body)) $loc }

simple_module_expression:
  | me = atomic_module_expression { me }
  | func = simple_module_expression; LPAREN; arg = module_expression; RPAREN
    { make_located (ModuleApply (func, arg)) $loc }

atomic_module_expression:
  | STRUCT; items = list(structure_item); END
    { make_located (ModuleStructure items) $loc }
  | path = module_path
    { make_located (ModulePath path) $loc }
  | LPAREN; me = module_expression; COLON; mt = module_type; RPAREN
    { make_located (ModuleConstraint (me, mt)) $loc }
  | LPAREN; me = module_expression; RPAREN
    { me }

(* Module types (signatures) *)
module_type:
  | mt = simple_module_type { mt }
  | FUNCTOR; params = nonempty_list(functor_parameter); ARROW; result = module_type
    { make_located (ModuleTypeFunctor (params, result)) $loc }

simple_module_type:
  | mt = atomic_module_type { mt }
  | mt = simple_module_type; WITH; constraints = separated_nonempty_list(AND, with_constraint)
    { make_located (ModuleTypeWith (mt, constraints)) $loc }

atomic_module_type:
  | SIG; items = list(signature_item); END
    { make_located (ModuleTypeSignature items) $loc }
  | path = module_path
    { make_located (ModuleTypePath path) $loc }
  | LPAREN; mt = module_type; RPAREN
    { mt }

(* Signature items *)
signature_item:
  | VAL; name = LOWERCASE_IDENTIFIER; COLON; ty = type_expression
    { make_located (SignatureValue (make_located name $loc(name), ty)) $loc }
  | TYPE; decls = separated_nonempty_list(AND, signature_type_declaration)
    { make_located (SignatureType decls) $loc }
  | MODULE; name = UPPERCASE_IDENTIFIER; COLON; mt = module_type
    { make_located (SignatureModule (make_located name $loc(name), mt)) $loc }
  | MODULE; TYPE; name = UPPERCASE_IDENTIFIER
    { make_located (SignatureModuleType (make_located name $loc(name), None)) $loc }
  | MODULE; TYPE; name = UPPERCASE_IDENTIFIER; EQUAL; mt = module_type
    { make_located (SignatureModuleType (make_located name $loc(name), Some mt)) $loc }
  | OPEN; path = module_path
    { make_located (SignatureOpen path) $loc }
  | INCLUDE; mt = module_type
    { make_located (SignatureInclude mt) $loc }
  | ext = external_declaration
    { make_located (SignatureExternal ext) $loc }

(* With constraints - use simple type variable names, no variance annotations *)
with_constraint:
  | TYPE; path = longident_type; params = type_parameters; EQUAL; ty = type_expression
    { WithType (path, List.map (fun p -> p.Syntax_tree.parameter_name) params, ty) }
  | TYPE; path = longident_type; params = type_parameters; COLONEQUALS; ty = type_expression
    { WithTypeDestructive (path, List.map (fun p -> p.Syntax_tree.parameter_name) params, ty) }
  | MODULE; path = longident; EQUAL; target = module_path
    { WithModule (path, target) }

(* Longidents for qualified names *)
longident:
  | name = LOWERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | li = longident; DOT; name = LOWERCASE_IDENTIFIER
    { make_located (Ldot (li, name)) $loc }

longident_type:
  | name = LOWERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | path = module_path; DOT; name = LOWERCASE_IDENTIFIER
    { let rec build_longident = function
        | [] -> failwith "empty module path"
        | [m] -> make_located (Lident m) $loc(path)
        | m :: rest ->
            let inner = build_longident rest in
            make_located (Ldot (inner, m)) $loc(path)
      in
      make_located (Ldot (build_longident (List.rev path.value), name)) $loc }

(* ==================== FFI External Declarations ==================== *)

(* Attribute names - allow keywords that are used as FFI attributes *)
%inline attribute_name:
  | name = LOWERCASE_IDENTIFIER { name }
  | MODULE { "module" }
  | VAL { "val" }
  | AS { "as" }

(* Attribute: @name or @name("arg") or @name(ident) or @name(("a", "b")) *)
attribute:
  | AT; name = attribute_name
    { Parsing_ffi.Attributes.{
        attribute_name = name;
        attribute_payload = None;
        attribute_location = make_loc $loc
      } }
  | AT; name = attribute_name; LPAREN; arg = STRING; RPAREN
    { Parsing_ffi.Attributes.{
        attribute_name = name;
        attribute_payload = Some (PayloadString arg);
        attribute_location = make_loc $loc
      } }
  | AT; name = attribute_name; LPAREN; ident = LOWERCASE_IDENTIFIER; RPAREN
    { Parsing_ffi.Attributes.{
        attribute_name = name;
        attribute_payload = Some (PayloadIdent ident);
        attribute_location = make_loc $loc
      } }
  | AT; name = attribute_name; LPAREN; LPAREN; args = separated_nonempty_list(COMMA, STRING); RPAREN; RPAREN
    { Parsing_ffi.Attributes.{
        attribute_name = name;
        attribute_payload = Some (PayloadStringList args);
        attribute_location = make_loc $loc
      } }

(* External declaration: [attrs] external name : type = "primitive" *)
external_declaration:
  | attrs = list(attribute); EXTERNAL; name = LOWERCASE_IDENTIFIER; COLON; ty = type_expression; EQUAL; prim = STRING
    { {
        external_attributes = attrs;
        external_name = make_located name $loc(name);
        external_type = ty;
        external_primitive = prim;
        external_location = make_loc $loc
      } }
