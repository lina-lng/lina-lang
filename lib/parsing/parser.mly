%{
open Common
open Syntax_tree

let make_loc (start_pos, end_pos) =
  Location.from_lexing_positions start_pos end_pos

let make_located value loc = Location.{ value; location = make_loc loc }

let make_binding pattern expression loc =
  { binding_pattern = pattern; binding_expression = expression; binding_location = make_loc loc }

(** Helper to create a parameter without default. *)
let make_param label pattern : fun_param =
  { param_label = label; param_pattern = pattern; param_default = None }

(** Helper to create a parameter with default (for optional arguments). *)
let make_param_default label pattern default : fun_param =
  { param_label = label; param_pattern = pattern; param_default = Some default }

(** Desugar optional arguments with defaults.
    [fun ?(x=default) -> body] becomes:
    [fun ?x -> let x = match x with None -> default | Some x -> x in body]
*)
let desugar_defaults params body =
  List.fold_right (fun p body ->
    match p.param_default with
    | None -> body
    | Some default ->
        let name = match p.param_pattern.Location.value with
          | PatternVariable n -> n
          | _ -> failwith "optional argument with default must use simple variable pattern"
        in
        let loc = p.param_pattern.Location.location in

        (* Build: let name = match name with None -> default | Some name -> name in body *)
        let var_expr = Location.{ value = ExpressionVariable name; location = loc } in
        let none_ctor = Location.{ value = Lident "None"; location = loc } in
        let some_ctor = Location.{ value = Lident "Some"; location = loc } in
        let none_pat = Location.{ value = PatternConstructor (none_ctor, None); location = loc } in
        let some_inner_pat = Location.{ value = PatternVariable name; location = loc } in
        let some_pat = Location.{ value = PatternConstructor (some_ctor, Some some_inner_pat); location = loc } in
        let none_arm = { arm_pattern = none_pat; arm_guard = None; arm_expression = default; arm_location = loc } in
        let some_arm = { arm_pattern = some_pat; arm_guard = None; arm_expression = var_expr; arm_location = loc } in
        let match_expr = Location.{ value = ExpressionMatch (var_expr, [none_arm; some_arm]); location = loc } in
        let var_pat = Location.{ value = PatternVariable name; location = loc } in
        let binding = { binding_pattern = var_pat; binding_expression = match_expr; binding_location = loc } in

        Location.{ value = ExpressionLet (Nonrecursive, [binding], body); location = loc }
  ) params body

(** Build a function expression from parameters (with possible defaults) and body.
    Desugars defaults and creates the ExpressionFunction node. *)
let build_function loc params body =
  let desugared_body = desugar_defaults params body in
  let labels_patterns = List.map (fun p -> (p.param_label, p.param_pattern)) params in
  Location.{ value = ExpressionFunction (labels_patterns, desugared_body); location = make_loc loc }
%}

%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> LOWERCASE_IDENTIFIER
%token <string> UPPERCASE_IDENTIFIER
%token <string> TYPE_VARIABLE
%token TRUE FALSE
%token LET REC IN FUN FUNCTION IF THEN ELSE TYPE OF AND AS MATCH WITH WHEN
%token MODULE STRUCT END SIG FUNCTOR OPEN INCLUDE VAL PRIVATE CONSTRAINT
%token EXTERNAL AT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA SEMICOLON COLON DOT DOTDOT ARROW EQUAL BAR UNDERSCORE
%token STAR PLUS MINUS MINUSDOT SLASH CARET
%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL NOT_EQUAL
%token REF BANG COLONEQUALS COLONCOLON MOD
%token <string> BACKTICK_TAG
%token TILDE QUESTION PLUSEQUAL
%token ASSERT
%token WHILE DO DONE FOR TO DOWNTO
%token <string> LETOP
%token <string> ANDOP
(* Custom operators - precedence determined by first character *)
%token <string> INFIXOP0  (* |> || && etc. - lowest precedence *)
%token <string> INFIXOP1  (* @@ etc. - right-associative *)
%token <string> INFIXOP2  (* ++ +. etc. - additive *)
%token <string> INFIXOP3  (* *. // etc. - multiplicative *)
%token <string> INFIXOP4  (* ** - highest precedence *)
%token <string> PREFIXOP  (* !. ~~ etc. - prefix operators *)
%token EOF

%right ARROW
%nonassoc IN
%right SEMICOLON
%right COLONEQUALS
%nonassoc WITH
%left BAR
%nonassoc WHEN
%nonassoc AS
%left COMMA
%nonassoc THEN
%nonassoc ELSE
%left INFIXOP0                              (* |> && || - lowest custom operator precedence *)
%left EQUAL EQUAL_EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%right INFIXOP1 COLONCOLON AT               (* @@ @ :: - right-associative *)
%left INFIXOP2 PLUS MINUS MINUSDOT          (* ++ +. + - -. - additive *)
%left INFIXOP3 STAR SLASH MOD               (* *. // * / mod - multiplicative *)
%right INFIXOP4 CARET                       (* ** ^ - highest custom operator precedence *)
%nonassoc unary_minus
%nonassoc PREFIXOP REF BANG                 (* prefix operators *)
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
  (* Recursive modules: module rec A : S = ... and B : T = ... *)
  | MODULE; REC; bindings = separated_nonempty_list(AND, rec_module_binding)
    { make_located (StructureRecModule bindings) $loc }
  | MODULE; TYPE; name = UPPERCASE_IDENTIFIER; EQUAL; mt = module_type
    { make_located (StructureModuleType (make_located name $loc(name), mt)) $loc }
  | OPEN; path = module_path
    { make_located (StructureOpen path) $loc }
  | INCLUDE; me = module_expression
    { make_located (StructureInclude me) $loc }
  | ext = external_declaration
    { make_located (StructureExternal ext) $loc }
  (* Type extension: type t += Constructor of type *)
  | TYPE; params = type_parameters; name = longident; PLUSEQUAL; constrs = separated_nonempty_list(BAR, constructor_declaration)
    {
      let ext = {
        extension_type_name = name;
        extension_type_params = params;
        extension_constructors = constrs;
        extension_location = make_loc $loc;
      } in
      make_located (StructureTypeExtension ext) $loc
    }
  | TYPE; params = type_parameters; name = longident; PLUSEQUAL; BAR; constrs = separated_nonempty_list(BAR, constructor_declaration)
    {
      let ext = {
        extension_type_name = name;
        extension_type_params = params;
        extension_constructors = constrs;
        extension_location = make_loc $loc;
      } in
      make_located (StructureTypeExtension ext) $loc
    }
  (* Top-level expression (e.g., let x = 1 in x, or print "hello") *)
  | e = expression
    { make_located (StructureExpression e) $loc }

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
  | name = LOWERCASE_IDENTIFIER; params = nonempty_list(labeled_pattern); EQUAL; expr = expression
    {
      let func_expr = build_function $loc params expr in
      let name_pattern = make_located (PatternVariable name) $loc(name) in
      make_binding name_pattern func_expr $loc
    }
  (* Operator definitions: let ( |> ) x f = f x, let ( let* ) x f = ..., etc. *)
  | LPAREN; op = operator_name; RPAREN; params = nonempty_list(labeled_pattern); EQUAL; expr = expression
    {
      let func_expr = build_function $loc params expr in
      let name_pattern = make_located (PatternVariable op) $loc in
      make_binding name_pattern func_expr $loc
    }
  (* let f x y : return_type = expr - return type annotation on function *)
  | name = LOWERCASE_IDENTIFIER; params = nonempty_list(labeled_pattern); COLON; ty = type_expression; EQUAL; expr = expression
    {
      let constrained_expr = make_located (ExpressionConstraint (expr, ty)) $loc(expr) in
      let func_expr = build_function $loc params constrained_expr in
      let name_pattern = make_located (PatternVariable name) $loc(name) in
      make_binding name_pattern func_expr $loc
    }

(* Binding operator "and" clause: and* p = e *)
and_letop_binding:
  | op = ANDOP; p = pattern; EQUAL; e = expression
    { { letop_and = Some op; letop_pattern = p; letop_expression = e } }

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

(* Type parameter with optional variance/injectivity annotation: 'a, +'a, -'a, !'a, _ *)
type_param:
  | PLUS; v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = Some VarianceCovariant; parameter_injective = false } }
  | MINUS; v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = Some VarianceContravariant; parameter_injective = false } }
  | BANG; v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = None; parameter_injective = true } }
  | v = TYPE_VARIABLE
    { { Syntax_tree.parameter_name = v; parameter_variance = None; parameter_injective = false } }
  (* Wildcard type parameter for GADTs: type _ t = ... *)
  | UNDERSCORE
    { { Syntax_tree.parameter_name = "_"; parameter_variance = None; parameter_injective = false } }

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
  | DOTDOT
    { TypeExtensible }

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
  (* GADT constructor with argument: Name : arg_type -> return_type
     Note: arg must be non_arrow_type_expression to avoid ambiguity with the no-arg case *)
  | name = UPPERCASE_IDENTIFIER; COLON; arg = non_arrow_type_expression; ARROW; ret = type_expression
    { { constructor_name = make_located name $loc(name);
        constructor_argument = Some arg;
        constructor_return_type = Some ret } }
  (* GADT constructor without argument: Name : return_type *)
  | name = UPPERCASE_IDENTIFIER; COLON; ret = type_expression
    { { constructor_name = make_located name $loc(name);
        constructor_argument = None;
        constructor_return_type = Some ret } }

type_expression:
  | t = arrow_type_expression { t }
  (* Polymorphic recursion annotation: type a b. body
     This has the lowest precedence - extends all the way to the right *)
  | TYPE; vars = nonempty_list(LOWERCASE_IDENTIFIER); DOT; body = type_expression
    { make_located (TypeForall (vars, body)) $loc }

(* Arrow types - TypeForall cannot appear on the left of an arrow *)
arrow_type_expression:
  | t = non_arrow_type_expression { t }
  (* Unlabeled argument: type -> result *)
  | t1 = non_arrow_type_expression; ARROW; t2 = arrow_type_expression
    { make_located (TypeArrow (Nolabel, t1, t2)) $loc }
  (* Labeled argument: ~label:type -> result or label:type -> result *)
  | TILDE; label = LOWERCASE_IDENTIFIER; COLON; t1 = non_arrow_type_expression; ARROW; t2 = arrow_type_expression
    { make_located (TypeArrow (Labelled label, t1, t2)) $loc }
  | label = LOWERCASE_IDENTIFIER; COLON; t1 = non_arrow_type_expression; ARROW; t2 = arrow_type_expression
    { make_located (TypeArrow (Labelled label, t1, t2)) $loc }
  (* Optional argument: ?label:type -> result *)
  | QUESTION; label = LOWERCASE_IDENTIFIER; COLON; t1 = non_arrow_type_expression; ARROW; t2 = arrow_type_expression
    { make_located (TypeArrow (Optional label, t1, t2)) $loc }

(* Types that are not arrows or foralls *)
non_arrow_type_expression:
  | t = simple_type_expression { t }
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
  (* Simple type: int, option *)
  | name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (make_located (Lident name) $loc, [])) $loc }
  (* Qualified type: M.t, M.N.t *)
  | mod_name = UPPERCASE_IDENTIFIER; DOT; type_name = LOWERCASE_IDENTIFIER
    { let mod_lid = make_located (Lident mod_name) $loc(mod_name) in
      make_located (TypeConstructor (make_located (Ldot (mod_lid, type_name)) $loc, [])) $loc }
  | mod_name = UPPERCASE_IDENTIFIER; DOT; rest = qualified_type_path
    { let mod_lid = make_located (Lident mod_name) $loc(mod_name) in
      let (path, type_name) = rest in
      let full_path = List.fold_left (fun acc m ->
        make_located (Ldot (acc, m)) $loc
      ) mod_lid path in
      make_located (TypeConstructor (make_located (Ldot (full_path, type_name)) $loc, [])) $loc }
  (* Type application with simple type: 'a option *)
  | arg = simple_type_expression; name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (make_located (Lident name) $loc(name), [arg])) $loc }
  (* Type application with qualified type: 'a M.t *)
  | arg = simple_type_expression; mod_name = UPPERCASE_IDENTIFIER; DOT; type_name = LOWERCASE_IDENTIFIER
    { let mod_lid = make_located (Lident mod_name) $loc(mod_name) in
      make_located (TypeConstructor (make_located (Ldot (mod_lid, type_name)) $loc, [arg])) $loc }
  (* Multi-arg type application: (int, string) result *)
  | LPAREN; args = separated_list(COMMA, type_expression); RPAREN; name = LOWERCASE_IDENTIFIER
    { make_located (TypeConstructor (make_located (Lident name) $loc(name), args)) $loc }
  | LPAREN; args = separated_list(COMMA, type_expression); RPAREN; mod_name = UPPERCASE_IDENTIFIER; DOT; type_name = LOWERCASE_IDENTIFIER
    { let mod_lid = make_located (Lident mod_name) $loc(mod_name) in
      make_located (TypeConstructor (make_located (Ldot (mod_lid, type_name)) $loc, args)) $loc }
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
  (* First-class module type: (module S) where S is a module type path *)
  | LPAREN; MODULE; path = module_path; RPAREN
    { make_located (TypePackage path) $loc }

(* Helper for multi-level qualified type paths like M.N.t (after initial M.) *)
(* Returns (module_path_list, type_name) where module_path_list is the intermediate modules *)
qualified_type_path:
  | m = UPPERCASE_IDENTIFIER; DOT; type_name = LOWERCASE_IDENTIFIER
    { ([m], type_name) }
  | m = UPPERCASE_IDENTIFIER; DOT; rest = qualified_type_path
    { let (path, type_name) = rest in ([m] @ path, type_name) }

expression_eof:
  | e = expression; EOF { e }

expression:
  | e = simple_expression { e }
  | LET; rf = rec_flag; bindings = separated_nonempty_list(AND, let_binding); IN; body = expression
    { make_located (ExpressionLet (rf, bindings, body)) $loc }
  | LET; MODULE; name = UPPERCASE_IDENTIFIER; EQUAL; me = module_expression; IN; body = expression
    { make_located (ExpressionLetModule (make_located name $loc(name), me, body)) $loc }
  (* Binding operators: let* x = e1 [and* y = e2 ...] in body *)
  | op = LETOP; p = pattern; EQUAL; e = expression; rest = list(and_letop_binding); IN; body = expression
    {
      let first_binding = { letop_and = None; letop_pattern = p; letop_expression = e } in
      make_located (ExpressionLetOp (op, first_binding :: rest, body)) $loc
    }
  | FUN; params = nonempty_list(labeled_pattern); ARROW; body = expression
    { build_function $loc params body }
  (* function | pat -> expr | ... is sugar for fun x -> match x with | pat -> expr | ... *)
  | FUNCTION; arms = match_arms
    {
      (* Generate a fresh variable name for the implicit argument *)
      let arg_name = "_function_arg" in
      let arg_pattern = make_located (PatternVariable arg_name) $loc in
      let arg_expr = make_located (ExpressionVariable arg_name) $loc in
      let match_expr = make_located (ExpressionMatch (arg_expr, arms)) $loc in
      make_located (ExpressionFunction ([(Nolabel, arg_pattern)], match_expr)) $loc
    }
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
  (* While loop: while cond do body done *)
  | WHILE; cond = expression; DO; body = expression; DONE
    { make_located (ExpressionWhile (cond, body)) $loc }
  (* For loop: for i = start to/downto end do body done *)
  | FOR; i = LOWERCASE_IDENTIFIER; EQUAL; start_e = expression; TO; end_e = expression; DO; body = expression; DONE
    { make_located (ExpressionFor (i, start_e, end_e, Upto, body)) $loc }
  | FOR; i = LOWERCASE_IDENTIFIER; EQUAL; start_e = expression; DOWNTO; end_e = expression; DO; body = expression; DONE
    { make_located (ExpressionFor (i, start_e, end_e, Downto, body)) $loc }
  (* Reference assignment: e1 := e2 *)
  | e1 = simple_expression; COLONEQUALS; e2 = expression
    { make_located (ExpressionAssign (e1, e2)) $loc }

simple_expression:
  | e = application_expression { e }
  | MINUS; e = simple_expression %prec unary_minus
    {
      (* Like OCaml: -<int> and -<float> become negative constants directly *)
      match e.value with
      | ExpressionConstant (ConstantInteger n) ->
          make_located (ExpressionConstant (ConstantInteger (-n))) $loc
      | ExpressionConstant (ConstantFloat f) ->
          make_located (ExpressionConstant (ConstantFloat (-.f))) $loc
      | _ ->
          let neg = make_located (ExpressionVariable "~-") $loc in
          make_located (ExpressionApply (neg, [(Nolabel, e)])) $loc
    }
  | MINUSDOT; e = simple_expression %prec unary_minus
    {
      (* Like OCaml: -.<float> becomes a negative constant directly *)
      match e.value with
      | ExpressionConstant (ConstantFloat f) ->
          make_located (ExpressionConstant (ConstantFloat (-.f))) $loc
      | _ ->
          let neg = make_located (ExpressionVariable "~-.") $loc in
          make_located (ExpressionApply (neg, [(Nolabel, e)])) $loc
    }
  (* Custom prefix operators: !foo, ~~x, etc. *)
  | op = PREFIXOP; e = simple_expression %prec PREFIXOP
    {
      let op_expr = make_located (ExpressionVariable op) $loc(op) in
      make_located (ExpressionApply (op_expr, [(Nolabel, e)])) $loc
    }
  (* Reference creation: ref e *)
  | REF; e = simple_expression %prec REF
    { make_located (ExpressionRef e) $loc }
  (* Assert expression: assert e *)
  | ASSERT; e = simple_expression %prec REF
    { make_located (ExpressionAssert e) $loc }
  | e1 = simple_expression; op = binary_operator; e2 = simple_expression
    {
      let op_expr = make_located (ExpressionVariable op) $loc(op) in
      make_located (ExpressionApply (op_expr, [(Nolabel, e1); (Nolabel, e2)])) $loc
    }
  (* List cons operator: e1 :: e2 desugars to Cons (e1, e2) *)
  | e1 = simple_expression; COLONCOLON; e2 = simple_expression
    {
      let cons_name = make_located (Lident "Cons") $loc in
      let tuple_arg = make_located (ExpressionTuple [e1; e2]) $loc in
      make_located (ExpressionConstructor (cons_name, Some tuple_arg)) $loc
    }

%inline binary_operator:
  | PLUS { "+" }
  | MINUS { "-" }
  | MINUSDOT { "-." }
  | STAR { "*" }
  | SLASH { "/" }
  | MOD { "mod" }
  | CARET { "^" }
  | AT { "@" }
  | EQUAL { "=" }
  | EQUAL_EQUAL { "==" }
  | NOT_EQUAL { "<>" }
  | LESS { "<" }
  | GREATER { ">" }
  | LESS_EQUAL { "<=" }
  | GREATER_EQUAL { ">=" }
  (* Custom operators *)
  | op = INFIXOP0 { op }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }

(* Operator names for definitions: let ( |> ) x f = f x *)
%inline operator_name:
  | PLUS { "+" }
  | MINUS { "-" }
  | MINUSDOT { "-." }
  | STAR { "*" }
  | SLASH { "/" }
  | MOD { "mod" }
  | CARET { "^" }
  | AT { "@" }
  | LESS { "<" }
  | GREATER { ">" }
  | LESS_EQUAL { "<=" }
  | GREATER_EQUAL { ">=" }
  | op = INFIXOP0 { op }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }
  | op = PREFIXOP { op }
  | op = LETOP { op }
  | op = ANDOP { op }

application_expression:
  | e = postfix_expression { e }
  (* Unlabeled argument *)
  | func = application_expression; arg = postfix_expression %prec APP
    {
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [(Nolabel, arg)])) $loc
      (* Constructor application: Some x becomes ExpressionConstructor("Some", Some x) *)
      | ExpressionConstructor (name, None) -> make_located (ExpressionConstructor (name, Some arg)) $loc
      (* Polymorphic variant application: `Tag x becomes ExpressionPolyVariant("Tag", Some x) *)
      | ExpressionPolyVariant (tag, None) -> make_located (ExpressionPolyVariant (tag, Some arg)) $loc
      | _ -> make_located (ExpressionApply (func, [(Nolabel, arg)])) $loc
    }
  (* Labeled argument: f ~x:e *)
  | func = application_expression; TILDE; label = LOWERCASE_IDENTIFIER; COLON; arg = postfix_expression %prec APP
    {
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [(Labelled label, arg)])) $loc
      | _ -> make_located (ExpressionApply (func, [(Labelled label, arg)])) $loc
    }
  (* Labeled argument with punning: f ~x is short for f ~x:x *)
  | func = application_expression; TILDE; label = LOWERCASE_IDENTIFIER %prec APP
    {
      let arg = make_located (ExpressionVariable label) $loc(label) in
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [(Labelled label, arg)])) $loc
      | _ -> make_located (ExpressionApply (func, [(Labelled label, arg)])) $loc
    }
  (* Optional argument: f ?x:e *)
  | func = application_expression; QUESTION; label = LOWERCASE_IDENTIFIER; COLON; arg = postfix_expression %prec APP
    {
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [(Optional label, arg)])) $loc
      | _ -> make_located (ExpressionApply (func, [(Optional label, arg)])) $loc
    }
  (* Optional argument with punning: f ?x is short for f ?x:x *)
  | func = application_expression; QUESTION; label = LOWERCASE_IDENTIFIER %prec APP
    {
      let arg = make_located (ExpressionVariable label) $loc(label) in
      match func.value with
      | ExpressionApply (f, args) -> make_located (ExpressionApply (f, args @ [(Optional label, arg)])) $loc
      | _ -> make_located (ExpressionApply (func, [(Optional label, arg)])) $loc
    }

(* Postfix expressions: record/field access binds tighter than application *)
postfix_expression:
  | e = atomic_expression { e }
  | e = postfix_expression; DOT; field = LOWERCASE_IDENTIFIER
    { make_located (ExpressionRecordAccess (e, field)) $loc }
  | e = postfix_expression; DOT; field = UPPERCASE_IDENTIFIER
    (* Allow M.N for nested module access - type checker handles the semantics *)
    { make_located (ExpressionRecordAccess (e, field)) $loc }
  (* Module operator access: M.( let* ), Option.( |> ), etc. *)
  | e = postfix_expression; DOT; LPAREN; op = operator_name; RPAREN
    { make_located (ExpressionRecordAccess (e, op)) $loc }
  (* Dereference at postfix level so it can be used as function argument *)
  | BANG; e = postfix_expression %prec BANG
    { make_located (ExpressionDeref e) $loc }

atomic_expression:
  | LPAREN; RPAREN
    { make_located (ExpressionConstant ConstantUnit) $loc }
  (* Parenthesized operators as values: ( let* ), ( |> ), ( + ), etc. *)
  | LPAREN; op = operator_name; RPAREN
    { make_located (ExpressionVariable op) $loc }
  | LPAREN; e = expression; RPAREN
    { e }
  | LPAREN; es = expression_tuple; RPAREN
    { make_located (ExpressionTuple es) $loc }
  (* First-class module packing: (module ME : MT) *)
  | LPAREN; MODULE; me = module_expression; COLON; mt = module_type; RPAREN
    { make_located (ExpressionPack (me, mt)) $loc }
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
  (* Constructor expressions: simple constructor or module *)
  | name = UPPERCASE_IDENTIFIER
    { make_located (ExpressionConstructor (make_located (Lident name) $loc(name), None)) $loc }
  | LBRACE; fields = separated_list(SEMICOLON, record_field); RBRACE
    { make_located (ExpressionRecord fields) $loc }
  | LBRACE; base = simple_expression; WITH; fields = separated_nonempty_list(SEMICOLON, record_field); RBRACE
    { make_located (ExpressionRecordUpdate (base, fields)) $loc }
  (* Polymorphic variant expression: `Tag (arg applied at application level) *)
  | tag = BACKTICK_TAG
    { make_located (ExpressionPolyVariant (tag, None)) $loc }
  (* Empty list: [] desugars to Nil *)
  | LBRACKET; RBRACKET
    {
      let nil_name = make_located (Lident "Nil") $loc in
      make_located (ExpressionConstructor (nil_name, None)) $loc
    }
  (* List literal: [e1; e2; e3] desugars to Cons(e1, Cons(e2, Cons(e3, Nil)))
     Uses simple_expression to avoid conflict with e1 SEMICOLON e2 sequence operator *)
  | LBRACKET; es = separated_nonempty_list(SEMICOLON, simple_expression); RBRACKET
    {
      let nil_name = make_located (Lident "Nil") $loc in
      let nil = make_located (ExpressionConstructor (nil_name, None)) $loc in
      let cons_name = make_located (Lident "Cons") $loc in
      List.fold_right (fun e acc ->
        let tuple_arg = make_located (ExpressionTuple [e; acc]) $loc in
        make_located (ExpressionConstructor (cons_name, Some tuple_arg)) $loc
      ) es nil
    }

expression_tuple:
  | e1 = expression; COMMA; e2 = expression
    { [e1; e2] }
  | es = expression_tuple; COMMA; e = expression
    { es @ [e] }

(* Labeled patterns for function parameters - returns fun_param with optional defaults *)
labeled_pattern:
  (* Unlabeled: p *)
  | p = simple_pattern
    { make_param Nolabel p }
  (* Labeled with punning: ~x binds variable x with label ~x *)
  | TILDE; name = LOWERCASE_IDENTIFIER
    { make_param (Labelled name) (make_located (PatternVariable name) $loc(name)) }
  (* Labeled with pattern: ~x:pattern *)
  | TILDE; label = LOWERCASE_IDENTIFIER; COLON; p = simple_pattern
    { make_param (Labelled label) p }
  (* Optional with punning: ?x binds variable x with label ?x *)
  | QUESTION; name = LOWERCASE_IDENTIFIER
    { make_param (Optional name) (make_located (PatternVariable name) $loc(name)) }
  (* Optional with pattern: ?x:pattern *)
  | QUESTION; label = LOWERCASE_IDENTIFIER; COLON; p = simple_pattern
    { make_param (Optional label) p }
  (* Optional with default: ?(x=expr) - desugared to match on None/Some *)
  | QUESTION; LPAREN; name = LOWERCASE_IDENTIFIER; EQUAL; default = simple_expression; RPAREN
    { make_param_default (Optional name) (make_located (PatternVariable name) $loc(name)) default }

simple_pattern:
  | p = atomic_pattern { p }
  | p = simple_pattern; AS; name = LOWERCASE_IDENTIFIER
    { make_located (PatternAlias (p, name)) $loc }
  (* List cons pattern: p1 :: p2 desugars to Cons (p1, p2) *)
  | p1 = simple_pattern; COLONCOLON; p2 = simple_pattern
    {
      let cons_ctor = make_located (Lident "Cons") $loc in
      let tuple_pat = make_located (PatternTuple [p1; p2]) $loc in
      make_located (PatternConstructor (cons_ctor, Some tuple_pat)) $loc
    }

atomic_pattern:
  | UNDERSCORE
    { make_located PatternWildcard $loc }
  | LPAREN; RPAREN
    { make_located (PatternConstant ConstantUnit) $loc }
  | LPAREN; TYPE; name = LOWERCASE_IDENTIFIER; RPAREN
    { make_located (PatternLocallyAbstract name) $loc }
  (* Parenthesized operators as patterns: ( let* ), ( |> ), ( + ), etc. *)
  | LPAREN; op = operator_name; RPAREN
    { make_located (PatternVariable op) $loc }
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
  (* Constructor patterns: simple (Some) or qualified (M.Some, M.N.Some) *)
  | ctor = constructor_longident
    { make_located (PatternConstructor (ctor, None)) $loc }
  | ctor = constructor_longident; p = atomic_pattern
    { make_located (PatternConstructor (ctor, Some p)) $loc }
  | LBRACE; fields = separated_list(SEMICOLON, record_pattern_field); RBRACE
    { make_located (PatternRecord (fields, false)) $loc }
  | LBRACE; fields = record_pattern_field_list_open; RBRACE
    { make_located (PatternRecord (fields, true)) $loc }
  (* Polymorphic variant pattern: `Tag or `Tag p *)
  | tag = BACKTICK_TAG
    { make_located (PatternPolyVariant (tag, None)) $loc }
  | tag = BACKTICK_TAG; p = atomic_pattern
    { make_located (PatternPolyVariant (tag, Some p)) $loc }
  (* Empty list pattern: [] desugars to Nil *)
  | LBRACKET; RBRACKET
    {
      let nil_ctor = make_located (Lident "Nil") $loc in
      make_located (PatternConstructor (nil_ctor, None)) $loc
    }
  (* List literal pattern: [p1; p2; p3] desugars to Cons(p1, Cons(p2, Cons(p3, Nil)))
     Uses simple_pattern to avoid conflicts *)
  | LBRACKET; ps = separated_nonempty_list(SEMICOLON, simple_pattern); RBRACKET
    {
      let nil_ctor = make_located (Lident "Nil") $loc in
      let nil_pat = make_located (PatternConstructor (nil_ctor, None)) $loc in
      let cons_ctor = make_located (Lident "Cons") $loc in
      List.fold_right (fun p acc ->
        let tuple_pat = make_located (PatternTuple [p; acc]) $loc in
        make_located (PatternConstructor (cons_ctor, Some tuple_pat)) $loc
      ) ps nil_pat
    }

pattern:
  | p = simple_pattern { p }
  | p1 = pattern; BAR; p2 = pattern
    { make_located (PatternOr (p1, p2)) $loc }
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

(* Functor parameters: (X : S) for applicative, () for generative *)
functor_parameter:
  | LPAREN; name = UPPERCASE_IDENTIFIER; COLON; mt = module_type; RPAREN
    { FunctorParamNamed (make_located name $loc(name), mt) }
  | LPAREN; RPAREN
    { FunctorParamUnit (make_loc $loc) }

(* Recursive module bindings: module rec A : S = ME and B : T = ME2
   Unlike regular modules, recursive modules MUST have explicit signatures *)
rec_module_binding:
  | name = UPPERCASE_IDENTIFIER; COLON; mt = module_type; EQUAL; expr = module_expression
    { { rec_module_name = make_located name $loc(name);
        rec_module_type = mt;
        rec_module_expr = expr;
        rec_module_location = make_loc $loc } }

(* Module expressions *)
module_expression:
  | me = simple_module_expression { me }
  | FUNCTOR; params = nonempty_list(functor_parameter); ARROW; body = module_expression
    { make_located (ModuleFunctor (params, body)) $loc }

simple_module_expression:
  | me = atomic_module_expression { me }
  | func = simple_module_expression; LPAREN; arg = module_expression; RPAREN
    { make_located (ModuleApply (func, arg)) $loc }
  (* Generative functor application: F() - unit argument for generative functors *)
  | func = simple_module_expression; LPAREN; RPAREN
    { let unit_struct = make_located (ModuleStructure []) $loc in
      make_located (ModuleApply (func, unit_struct)) $loc }

atomic_module_expression:
  | STRUCT; items = list(structure_item); END
    { make_located (ModuleStructure items) $loc }
  | path = module_path
    { make_located (ModulePath path) $loc }
  | LPAREN; me = module_expression; COLON; mt = module_type; RPAREN
    { make_located (ModuleConstraint (me, mt)) $loc }
  | LPAREN; me = module_expression; RPAREN
    { me }
  (* First-class module unpacking: (val e : MT) *)
  | LPAREN; VAL; e = expression; COLON; mt = module_type; RPAREN
    { make_located (ModuleUnpack (e, mt)) $loc }

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
  | MODULE; TYPE; OF; me = module_expression
    { make_located (ModuleTypeOf me) $loc }

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

(* With constraints - type parameters come BEFORE the type name, like 'a t or ('a, 'b) t *)
with_constraint:
  | TYPE; params = type_parameters; path = longident_type; EQUAL; ty = type_expression
    { WithType (path, List.map (fun p -> p.Syntax_tree.parameter_name) params, ty) }
  | TYPE; params = type_parameters; path = longident_type; COLONEQUALS; ty = type_expression
    { WithTypeDestructive (path, List.map (fun p -> p.Syntax_tree.parameter_name) params, ty) }
  | MODULE; path = module_longident; EQUAL; target = module_path
    { WithModule (path, target) }

(* Longidents for qualified names *)
longident:
  | name = LOWERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | li = module_longident; DOT; name = LOWERCASE_IDENTIFIER
    { make_located (Ldot (li, name)) $loc }

(* Module longidents (uppercase) for with module constraints *)
module_longident:
  | name = UPPERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | li = module_longident; DOT; name = UPPERCASE_IDENTIFIER
    { make_located (Ldot (li, name)) $loc }

(* Constructor longidents (uppercase) for pattern matching: Some, M.Some, M.N.Some *)
constructor_longident:
  | name = UPPERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | li = constructor_longident; DOT; name = UPPERCASE_IDENTIFIER
    { make_located (Ldot (li, name)) $loc }

(* Type paths for with-constraints: t, M.t, M.N.t, Inner.t, M.Inner.t
   Uses a separate prefix rule to avoid conflicts with module_path *)
longident_type:
  | name = LOWERCASE_IDENTIFIER
    { make_located (Lident name) $loc }
  | prefix = longident_type_prefix; DOT; name = LOWERCASE_IDENTIFIER
    { make_located (Ldot (prefix, name)) $loc }

longident_type_prefix:
  | mod_name = UPPERCASE_IDENTIFIER
    { make_located (Lident mod_name) $loc }
  | prefix = longident_type_prefix; DOT; mod_name = UPPERCASE_IDENTIFIER
    { make_located (Ldot (prefix, mod_name)) $loc }

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
