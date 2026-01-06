open Common
open Parsing

(** Strip location from a located value *)
let strip_loc (x : 'a Location.located) : 'a Location.located =
  { x with location = Location.none }

(** Recursively strip all locations from a type expression *)
let rec strip_type_locations (t : Syntax_tree.type_expression) :
    Syntax_tree.type_expression =
  let desc =
    match t.value with
    | Syntax_tree.TypeVariable _ -> t.value
    | Syntax_tree.TypeConstructor (name, args) ->
        Syntax_tree.TypeConstructor (name, List.map strip_type_locations args)
    | Syntax_tree.TypeTuple ts ->
        Syntax_tree.TypeTuple (List.map strip_type_locations ts)
    | Syntax_tree.TypeArrow (t1, t2) ->
        Syntax_tree.TypeArrow (strip_type_locations t1, strip_type_locations t2)
  in
  { value = desc; location = Location.none }

(** Recursively strip all locations from a pattern *)
let rec strip_pattern_locations (p : Syntax_tree.pattern) : Syntax_tree.pattern =
  let desc =
    match p.value with
    | Syntax_tree.PatternVariable _ | Syntax_tree.PatternWildcard
    | Syntax_tree.PatternConstant _ ->
        p.value
    | Syntax_tree.PatternTuple ps ->
        Syntax_tree.PatternTuple (List.map strip_pattern_locations ps)
    | Syntax_tree.PatternConstructor (name, arg) ->
        Syntax_tree.PatternConstructor
          (name, Option.map strip_pattern_locations arg)
    | Syntax_tree.PatternAlias (p', name) ->
        Syntax_tree.PatternAlias (strip_pattern_locations p', name)
    | Syntax_tree.PatternConstraint (p', ty) ->
        Syntax_tree.PatternConstraint
          (strip_pattern_locations p', strip_type_locations ty)
  in
  { value = desc; location = Location.none }

(** Recursively strip all locations from an expression *)
let rec strip_expr_locations (e : Syntax_tree.expression) :
    Syntax_tree.expression =
  let desc =
    match e.value with
    | Syntax_tree.ExpressionVariable _ | Syntax_tree.ExpressionConstant _ ->
        e.value
    | Syntax_tree.ExpressionTuple es ->
        Syntax_tree.ExpressionTuple (List.map strip_expr_locations es)
    | Syntax_tree.ExpressionConstructor (name, arg) ->
        Syntax_tree.ExpressionConstructor
          (name, Option.map strip_expr_locations arg)
    | Syntax_tree.ExpressionApply (f, args) ->
        Syntax_tree.ExpressionApply
          (strip_expr_locations f, List.map strip_expr_locations args)
    | Syntax_tree.ExpressionFunction (pats, body) ->
        Syntax_tree.ExpressionFunction
          (List.map strip_pattern_locations pats, strip_expr_locations body)
    | Syntax_tree.ExpressionLet (rf, bindings, body) ->
        Syntax_tree.ExpressionLet
          (rf, List.map strip_binding_locations bindings, strip_expr_locations body)
    | Syntax_tree.ExpressionIf (cond, then_, else_) ->
        Syntax_tree.ExpressionIf
          ( strip_expr_locations cond,
            strip_expr_locations then_,
            Option.map strip_expr_locations else_ )
    | Syntax_tree.ExpressionSequence (e1, e2) ->
        Syntax_tree.ExpressionSequence
          (strip_expr_locations e1, strip_expr_locations e2)
    | Syntax_tree.ExpressionConstraint (e', ty) ->
        Syntax_tree.ExpressionConstraint
          (strip_expr_locations e', strip_type_locations ty)
  in
  { value = desc; location = Location.none }

and strip_binding_locations (b : Syntax_tree.binding) : Syntax_tree.binding =
  {
    binding_pattern = strip_pattern_locations b.binding_pattern;
    binding_expression = strip_expr_locations b.binding_expression;
    binding_location = Location.none;
  }

(** Strip locations from type declaration *)
let rec strip_type_decl_locations (d : Syntax_tree.type_declaration) :
    Syntax_tree.type_declaration =
  {
    type_name = strip_loc d.type_name;
    type_parameters = d.type_parameters;
    type_kind = strip_type_kind_locations d.type_kind;
    type_location = Location.none;
  }

and strip_type_kind_locations (k : Syntax_tree.type_declaration_kind) :
    Syntax_tree.type_declaration_kind =
  match k with
  | Syntax_tree.TypeAbstract -> k
  | Syntax_tree.TypeVariant constrs ->
      Syntax_tree.TypeVariant (List.map strip_constructor_locations constrs)

and strip_constructor_locations (c : Syntax_tree.constructor_declaration) :
    Syntax_tree.constructor_declaration =
  {
    constructor_name = strip_loc c.constructor_name;
    constructor_argument = Option.map strip_type_locations c.constructor_argument;
  }

(** Strip locations from a structure item *)
let strip_structure_item_locations (item : Syntax_tree.structure_item) :
    Syntax_tree.structure_item =
  let desc =
    match item.value with
    | Syntax_tree.StructureValue (rf, bindings) ->
        Syntax_tree.StructureValue (rf, List.map strip_binding_locations bindings)
    | Syntax_tree.StructureType decls ->
        Syntax_tree.StructureType (List.map strip_type_decl_locations decls)
  in
  { value = desc; location = Location.none }

(** Parse and return expression with stripped locations for comparison *)
let parse_expr (src : string) : Syntax_tree.expression =
  let expr = Parse.expression_from_string src in
  strip_expr_locations expr

(** Parse and show expression for expect tests *)
let show_parsed_expr (src : string) : string =
  try
    let expr = parse_expr src in
    Syntax_tree.show_expression expr
  with Compiler_error.Error e -> Compiler_error.report_to_string e

(** Parse and show structure for expect tests *)
let show_parsed_structure (src : string) : string =
  try
    let structure = Parse.structure_from_string src in
    let stripped = List.map strip_structure_item_locations structure in
    Syntax_tree.show_structure stripped
  with Compiler_error.Error e -> Compiler_error.report_to_string e

(** Tokenize and show tokens for expect tests *)
let show_tokens (src : string) : string =
  try
    let tokens = Lexer.tokenize "<test>" src in
    String.concat "\n"
      (List.map (fun (tok, _) -> Lexer.show_token tok) tokens)
  with Compiler_error.Error e -> Compiler_error.report_to_string e

(** Check if two expressions are structurally equal (ignoring locations) *)
let expr_equal (e1 : Syntax_tree.expression) (e2 : Syntax_tree.expression) : bool
    =
  Syntax_tree.equal_expression (strip_expr_locations e1) (strip_expr_locations e2)
