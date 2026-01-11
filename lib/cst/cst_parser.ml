(** CST parser: builds concrete syntax trees from token streams.

    This module provides a recursive descent parser that constructs CST nodes
    directly from tokens, preserving all trivia. The parser is designed to be
    error-tolerant and always produces a tree, even for invalid input.

    For now, this implements a simple token-based approach. A more sophisticated
    version could integrate with the Menhir parser for better error recovery. *)

(** {1 Parser State} *)

(** Parser state containing the token stream. *)
type state = {
  lexer : Cst_lexer.state;
  mutable current : Cst_lexer.cst_token;
  mutable children : Green_tree.green_child list;
}

(** [create filename content] creates a new parser state. *)
let create filename content =
  let lexer = Cst_lexer.create_state filename content in
  let current = Cst_lexer.next_token lexer in
  { lexer; current; children = [] }

(** {1 Token Operations} *)

(** [current_kind state] returns the syntax kind of the current token. *)
let current_kind state = state.current.green.kind

(** [at state kind] returns true if the current token has the given kind. *)
let at state kind = Syntax_kind.equal (current_kind state) kind

(** [at_eof state] returns true if at end of file. *)
let at_eof state = at state Syntax_kind.TK_EOF

(** [bump state] consumes the current token and advances to the next.
    The consumed token is added to the children list. *)
let bump state =
  state.children <- state.children @ [ Green_tree.GreenToken state.current.green ];
  state.current <- Cst_lexer.next_token state.lexer

(** [expect state kind] consumes a token of the given kind, or creates an error. *)
let expect state kind =
  if at state kind then bump state
  else
    (* Create an error node for the missing token *)
    let error = Green_tree.make_error_node [] in
    state.children <- state.children @ [ Green_tree.GreenNode error ]

(** [peek_kind state] returns the kind of the next token without consuming it. *)
let peek_kind state =
  let token = Cst_lexer.peek_token state.lexer in
  token.green.kind

(** {1 Node Building} *)

(** [start_node state] marks the start of a new node by saving the current
    children list and starting a fresh one. Returns the saved children. *)
let start_node state =
  let saved = state.children in
  state.children <- [];
  saved

(** [finish_node state kind saved_children] finishes building a node,
    restores the saved children, and adds the new node to them. *)
let finish_node state kind saved_children =
  let node = Green_tree.make_node kind state.children in
  state.children <- saved_children @ [ Green_tree.GreenNode node ];
  node

(** {1 Expression Parsing} *)

(** [parse_primary state] parses a primary expression (literals, identifiers, parens). *)
let rec parse_primary state =
  let saved = start_node state in
  let kind =
    match current_kind state with
    | TK_INTEGER | TK_FLOAT | TK_STRING | TK_TRUE | TK_FALSE ->
        bump state;
        Syntax_kind.NK_CONSTANT_EXPR
    | TK_LOWERCASE_IDENT ->
        bump state;
        Syntax_kind.NK_VARIABLE_EXPR
    | TK_UPPERCASE_IDENT ->
        bump state;
        Syntax_kind.NK_CONSTRUCTOR_EXPR
    | TK_LPAREN ->
        bump state;
        parse_expression state;
        expect state Syntax_kind.TK_RPAREN;
        Syntax_kind.NK_PAREN_EXPR
    | TK_LBRACE ->
        parse_record_expr state;
        Syntax_kind.NK_RECORD_EXPR
    | TK_IF ->
        parse_if_expr state;
        Syntax_kind.NK_IF_EXPR
    | TK_LET ->
        parse_let_expr state;
        Syntax_kind.NK_LET_EXPR
    | TK_FUN ->
        parse_function_expr state;
        Syntax_kind.NK_FUNCTION_EXPR
    | TK_MATCH ->
        parse_match_expr state;
        Syntax_kind.NK_MATCH_EXPR
    | _ ->
        (* Create an error node for unexpected token *)
        if not (at_eof state) then bump state;
        Syntax_kind.NK_ERROR
  in
  ignore (finish_node state kind saved)

(** [parse_application state] parses function application. *)
and parse_application state =
  parse_primary state;
  (* Parse additional arguments *)
  while
    (not (at_eof state))
    && (at state TK_LOWERCASE_IDENT
       || at state TK_UPPERCASE_IDENT
       || at state TK_INTEGER
       || at state TK_FLOAT
       || at state TK_STRING
       || at state TK_TRUE
       || at state TK_FALSE
       || at state TK_LPAREN
       || at state TK_LBRACE)
  do
    let saved = start_node state in
    (* Move the last child (the function) into this new apply node *)
    let func =
      match List.rev state.children with
      | [] -> []
      | last :: rest ->
          state.children <- List.rev rest;
          [ last ]
    in
    state.children <- func;
    parse_primary state;
    ignore (finish_node state NK_APPLY_EXPR saved)
  done

(** [parse_infix_expr state] parses infix expressions with operators. *)
and parse_infix_expr state =
  parse_application state;
  while
    (not (at_eof state))
    && (at state TK_PLUS
       || at state TK_MINUS
       || at state TK_STAR
       || at state TK_SLASH
       || at state TK_LESS
       || at state TK_GREATER
       || at state TK_LESS_EQUAL
       || at state TK_GREATER_EQUAL
       || at state TK_EQUAL_EQUAL
       || at state TK_NOT_EQUAL)
  do
    let saved = start_node state in
    (* Move the last child (left operand) into this new infix node *)
    let left =
      match List.rev state.children with
      | [] -> []
      | last :: rest ->
          state.children <- List.rev rest;
          [ last ]
    in
    state.children <- left;
    bump state;
    (* operator *)
    parse_application state;
    ignore (finish_node state NK_INFIX_EXPR saved)
  done

(** [parse_expression state] parses a full expression. *)
and parse_expression state = parse_infix_expr state

(** [parse_record_expr state] parses a record expression or record update.
    Handles both { a = 1; b = 2 } and { base with a = 1; b = 2 }. *)
and parse_record_expr state =
  bump state;
  (* { *)
  (* Check for record update: { expr with ... }
     We look ahead: if we have an identifier followed by 'with', it's a record update *)
  if
    (at state TK_LOWERCASE_IDENT || at state TK_UPPERCASE_IDENT)
    && Syntax_kind.equal (peek_kind state) TK_WITH
  then (
    (* Record update: parse base expression and 'with' keyword *)
    bump state;
    (* identifier *)
    expect state TK_WITH (* 'with' keyword *));
  (* Parse record fields *)
  while (not (at_eof state)) && not (at state TK_RBRACE) do
    let field_saved = start_node state in
    if at state TK_LOWERCASE_IDENT then bump state;
    if at state TK_EQUAL then (
      bump state;
      parse_expression state);
    ignore (finish_node state NK_RECORD_FIELD field_saved);
    if at state TK_SEMICOLON then bump state
  done;
  expect state TK_RBRACE

(** [parse_if_expr state] parses an if expression. *)
and parse_if_expr state =
  bump state;
  (* if *)
  parse_expression state;
  expect state TK_THEN;
  parse_expression state;
  if at state TK_ELSE then (
    bump state;
    parse_expression state)

(** [parse_let_expr state] parses a let expression. *)
and parse_let_expr state =
  bump state;
  (* let *)
  if at state TK_REC then bump state;
  parse_binding state;
  while at state TK_AND do
    bump state;
    parse_binding state
  done;
  expect state TK_IN;
  parse_expression state

(** [parse_function_expr state] parses a function expression. *)
and parse_function_expr state =
  bump state;
  (* fun *)
  while
    (not (at_eof state))
    && (at state TK_LOWERCASE_IDENT || at state TK_UNDERSCORE || at state TK_LPAREN)
  do
    parse_pattern state
  done;
  expect state TK_ARROW;
  parse_expression state

(** [parse_match_expr state] parses a match expression. *)
and parse_match_expr state =
  bump state;
  (* match *)
  parse_expression state;
  expect state TK_WITH;
  if at state TK_BAR then bump state;
  parse_match_arm state;
  while at state TK_BAR do
    bump state;
    parse_match_arm state
  done

(** [parse_match_arm state] parses a single match arm. *)
and parse_match_arm state =
  let saved = start_node state in
  parse_pattern state;
  if at state TK_WHEN then (
    bump state;
    parse_expression state);
  expect state TK_ARROW;
  parse_expression state;
  ignore (finish_node state NK_MATCH_ARM saved)

(** {1 Pattern Parsing} *)

(** [parse_pattern state] parses a pattern. *)
and parse_pattern state =
  let saved = start_node state in
  let kind =
    match current_kind state with
    | TK_LOWERCASE_IDENT ->
        bump state;
        Syntax_kind.NK_VARIABLE_PATTERN
    | TK_UNDERSCORE ->
        bump state;
        Syntax_kind.NK_WILDCARD_PATTERN
    | TK_INTEGER | TK_FLOAT | TK_STRING | TK_TRUE | TK_FALSE ->
        bump state;
        Syntax_kind.NK_CONSTANT_PATTERN
    | TK_UPPERCASE_IDENT ->
        bump state;
        (* Check for constructor arguments *)
        if
          at state TK_LOWERCASE_IDENT
          || at state TK_UNDERSCORE
          || at state TK_LPAREN
        then parse_pattern state;
        Syntax_kind.NK_CONSTRUCTOR_PATTERN
    | TK_LPAREN ->
        bump state;
        parse_pattern state;
        (* Check for tuple *)
        if at state TK_COMMA then (
          while at state TK_COMMA do
            bump state;
            parse_pattern state
          done;
          expect state TK_RPAREN;
          Syntax_kind.NK_TUPLE_PATTERN)
        else (
          expect state TK_RPAREN;
          Syntax_kind.NK_PAREN_PATTERN)
    | TK_LBRACE ->
        parse_record_pattern state;
        Syntax_kind.NK_RECORD_PATTERN
    | _ ->
        if not (at_eof state) then bump state;
        Syntax_kind.NK_ERROR
  in
  ignore (finish_node state kind saved)

(** [parse_record_pattern state] parses a record pattern. *)
and parse_record_pattern state =
  bump state;
  (* { *)
  while (not (at_eof state)) && not (at state TK_RBRACE) do
    let field_saved = start_node state in
    if at state TK_LOWERCASE_IDENT then bump state;
    if at state TK_EQUAL then (
      bump state;
      parse_pattern state);
    ignore (finish_node state NK_RECORD_PATTERN_FIELD field_saved);
    if at state TK_SEMICOLON then bump state;
    if at state TK_DOTDOT then bump state
  done;
  expect state TK_RBRACE

(** {1 Binding Parsing} *)

(** [parse_binding state] parses a value binding. *)
and parse_binding state =
  let saved = start_node state in
  parse_pattern state;
  (* Parameters *)
  while
    at state TK_LOWERCASE_IDENT || at state TK_UNDERSCORE || at state TK_LPAREN
  do
    parse_pattern state
  done;
  (* Optional type annotation *)
  if at state TK_COLON then (
    bump state;
    parse_type state);
  expect state TK_EQUAL;
  parse_expression state;
  ignore (finish_node state NK_BINDING saved)

(** {1 Type Parsing} *)

(** [parse_type state] parses a type expression. *)
and parse_type state =
  parse_type_primary state;
  (* Arrow types *)
  while at state TK_ARROW do
    let saved = start_node state in
    let left =
      match List.rev state.children with
      | [] -> []
      | last :: rest ->
          state.children <- List.rev rest;
          [ last ]
    in
    state.children <- left;
    bump state;
    parse_type_primary state;
    ignore (finish_node state NK_TYPE_ARROW saved)
  done

(** [parse_type_primary state] parses a primary type (no arrows). *)
and parse_type_primary state =
  parse_type_atom state;
  (* Tuple types *)
  if at state TK_STAR then (
    let saved = start_node state in
    let first =
      match List.rev state.children with
      | [] -> []
      | last :: rest ->
          state.children <- List.rev rest;
          [ last ]
    in
    state.children <- first;
    while at state TK_STAR do
      bump state;
      parse_type_atom state
    done;
    ignore (finish_node state NK_TYPE_TUPLE saved))

(** [parse_type_atom state] parses an atomic type. *)
and parse_type_atom state =
  let saved = start_node state in
  let kind =
    match current_kind state with
    | TK_TYPE_VARIABLE ->
        bump state;
        Syntax_kind.NK_TYPE_VARIABLE
    | TK_LOWERCASE_IDENT ->
        bump state;
        (* Type constructor, possibly with arguments *)
        Syntax_kind.NK_TYPE_CONSTRUCTOR
    | TK_UPPERCASE_IDENT ->
        bump state;
        if at state TK_DOT then (
          bump state;
          parse_type_atom state);
        Syntax_kind.NK_TYPE_CONSTRUCTOR
    | TK_LPAREN ->
        bump state;
        parse_type state;
        expect state TK_RPAREN;
        Syntax_kind.NK_PAREN_TYPE
    | TK_LBRACE ->
        parse_record_type state;
        Syntax_kind.NK_TYPE_RECORD
    | _ ->
        if not (at_eof state) then bump state;
        Syntax_kind.NK_ERROR
  in
  ignore (finish_node state kind saved)

(** [parse_record_type state] parses a record type. *)
and parse_record_type state =
  bump state;
  (* { *)
  while (not (at_eof state)) && not (at state TK_RBRACE) do
    let field_saved = start_node state in
    if at state TK_LOWERCASE_IDENT then bump state;
    expect state TK_COLON;
    parse_type state;
    ignore (finish_node state NK_TYPE_RECORD_FIELD field_saved);
    if at state TK_SEMICOLON then bump state
  done;
  expect state TK_RBRACE

(** {1 Structure Item Parsing} *)

(** [parse_structure_item state] parses a single structure item. *)
and parse_structure_item state =
  let saved = start_node state in
  let kind =
    match current_kind state with
    | TK_LET ->
        bump state;
        if at state TK_REC then bump state;
        parse_binding state;
        while at state TK_AND do
          bump state;
          parse_binding state
        done;
        Syntax_kind.NK_VALUE_DEFINITION
    | TK_TYPE ->
        bump state;
        parse_type_declaration state;
        while at state TK_AND do
          bump state;
          parse_type_declaration state
        done;
        Syntax_kind.NK_TYPE_DEFINITION
    | TK_MODULE ->
        bump state;
        if at state TK_TYPE then (
          bump state;
          parse_module_type_definition state;
          Syntax_kind.NK_MODULE_TYPE_DEFINITION)
        else (
          parse_module_definition state;
          Syntax_kind.NK_MODULE_DEFINITION)
    | TK_OPEN ->
        bump state;
        parse_module_path state;
        Syntax_kind.NK_OPEN_DECLARATION
    | TK_INCLUDE ->
        bump state;
        parse_module_expr state;
        Syntax_kind.NK_INCLUDE_DECLARATION
    | TK_EXTERNAL ->
        parse_external_declaration state;
        Syntax_kind.NK_EXTERNAL_DECLARATION
    | TK_AT ->
        (* Attribute - consume it and try next item *)
        parse_attribute state;
        ignore (parse_structure_item state);
        Syntax_kind.NK_VALUE_DEFINITION (* The item is already added to children *)
    | _ ->
        (* Unexpected token, create error and advance *)
        if not (at_eof state) then bump state;
        Syntax_kind.NK_ERROR
  in
  finish_node state kind saved

(** [parse_type_declaration state] parses a type declaration. *)
and parse_type_declaration state =
  let saved = start_node state in
  (* Type parameters *)
  if at state TK_TYPE_VARIABLE then (
    let params_saved = start_node state in
    bump state;
    while at state TK_COMMA || at state TK_TYPE_VARIABLE do
      if at state TK_COMMA then bump state;
      if at state TK_TYPE_VARIABLE then bump state
    done;
    ignore (finish_node state NK_TYPE_PARAMETERS params_saved))
  else if at state TK_LPAREN then (
    let params_saved = start_node state in
    bump state;
    while (not (at_eof state)) && not (at state TK_RPAREN) do
      if at state TK_TYPE_VARIABLE then bump state;
      if at state TK_COMMA then bump state
    done;
    expect state TK_RPAREN;
    ignore (finish_node state NK_TYPE_PARAMETERS params_saved));
  (* Type name *)
  if at state TK_LOWERCASE_IDENT then bump state;
  (* Optional definition *)
  if at state TK_EQUAL then (
    bump state;
    if at state TK_BAR || at state TK_UPPERCASE_IDENT then (
      (* Variant type *)
      if at state TK_BAR then bump state;
      parse_constructor_declaration state;
      while at state TK_BAR do
        bump state;
        parse_constructor_declaration state
      done)
    else if at state TK_LBRACE then parse_record_type state
    else parse_type state);
  ignore (finish_node state NK_TYPE_DECLARATION saved)

(** [parse_constructor_declaration state] parses a variant constructor. *)
and parse_constructor_declaration state =
  let saved = start_node state in
  if at state TK_UPPERCASE_IDENT then bump state;
  if at state TK_OF then (
    bump state;
    parse_type state);
  ignore (finish_node state NK_CONSTRUCTOR_DECLARATION saved)

(** [parse_module_definition state] parses a module definition. *)
and parse_module_definition state =
  if at state TK_UPPERCASE_IDENT then bump state;
  (* Optional functor parameters *)
  while at state TK_LPAREN do
    parse_functor_parameter state
  done;
  (* Optional signature constraint *)
  if at state TK_COLON then (
    bump state;
    parse_module_type state);
  expect state TK_EQUAL;
  parse_module_expr state

(** [parse_module_type_definition state] parses a module type definition. *)
and parse_module_type_definition state =
  if at state TK_UPPERCASE_IDENT then bump state;
  expect state TK_EQUAL;
  parse_module_type state

(** [parse_module_path state] parses a module path (A.B.C). *)
and parse_module_path state =
  let saved = start_node state in
  if at state TK_UPPERCASE_IDENT then bump state;
  while at state TK_DOT do
    bump state;
    if at state TK_UPPERCASE_IDENT then bump state
  done;
  ignore (finish_node state NK_MODULE_PATH saved)

(** [parse_module_expr state] parses a module expression. *)
and parse_module_expr state =
  match current_kind state with
  | TK_STRUCT ->
      let saved = start_node state in
      bump state;
      while (not (at_eof state)) && not (at state TK_END) do
        ignore (parse_structure_item state)
      done;
      expect state TK_END;
      ignore (finish_node state NK_STRUCTURE saved)
  | TK_FUNCTOR ->
      let saved = start_node state in
      bump state;
      while at state TK_LPAREN do
        parse_functor_parameter state
      done;
      expect state TK_ARROW;
      parse_module_expr state;
      ignore (finish_node state NK_FUNCTOR_EXPR saved)
  | TK_UPPERCASE_IDENT ->
      parse_module_path state;
      (* Check for functor application *)
      while at state TK_LPAREN do
        let saved = start_node state in
        let path =
          match List.rev state.children with
          | [] -> []
          | last :: rest ->
              state.children <- List.rev rest;
              [ last ]
        in
        state.children <- path;
        bump state;
        parse_module_expr state;
        expect state TK_RPAREN;
        ignore (finish_node state NK_MODULE_APPLY saved)
      done
  | _ -> ()

(** [parse_module_type state] parses a module type (signature). *)
and parse_module_type state =
  match current_kind state with
  | TK_SIG ->
      let saved = start_node state in
      bump state;
      while (not (at_eof state)) && not (at state TK_END) do
        parse_signature_item state
      done;
      expect state TK_END;
      ignore (finish_node state NK_SIGNATURE saved)
  | TK_FUNCTOR ->
      let saved = start_node state in
      bump state;
      while at state TK_LPAREN do
        parse_functor_parameter state
      done;
      expect state TK_ARROW;
      parse_module_type state;
      ignore (finish_node state NK_FUNCTOR_TYPE saved)
  | TK_UPPERCASE_IDENT ->
      let saved = start_node state in
      parse_module_path state;
      (* Check for with constraints *)
      if at state TK_WITH then (
        bump state;
        parse_with_constraint state;
        while at state TK_AND do
          bump state;
          parse_with_constraint state
        done;
        ignore (finish_node state NK_MODULE_TYPE_WITH saved))
  | _ -> ()

(** [parse_functor_parameter state] parses a functor parameter. *)
and parse_functor_parameter state =
  let saved = start_node state in
  bump state;
  (* ( *)
  if at state TK_UPPERCASE_IDENT then bump state;
  expect state TK_COLON;
  parse_module_type state;
  expect state TK_RPAREN;
  ignore (finish_node state NK_FUNCTOR_PARAMETER saved)

(** [parse_with_constraint state] parses a with-constraint. *)
and parse_with_constraint state =
  let saved = start_node state in
  let kind =
    if at state TK_TYPE then (
      bump state;
      (* type path = type *)
      while at state TK_TYPE_VARIABLE || at state TK_LPAREN do
        if at state TK_TYPE_VARIABLE then bump state
        else if at state TK_LPAREN then (
          bump state;
          while (not (at_eof state)) && not (at state TK_RPAREN) do
            if at state TK_TYPE_VARIABLE then bump state;
            if at state TK_COMMA then bump state
          done;
          expect state TK_RPAREN)
      done;
      parse_module_path state;
      if at state TK_DOT then (
        bump state;
        if at state TK_LOWERCASE_IDENT then bump state);
      expect state TK_EQUAL;
      parse_type state;
      Syntax_kind.NK_WITH_TYPE_CONSTRAINT)
    else if at state TK_MODULE then (
      bump state;
      parse_module_path state;
      expect state TK_EQUAL;
      parse_module_path state;
      Syntax_kind.NK_WITH_MODULE_CONSTRAINT)
    else Syntax_kind.NK_ERROR
  in
  ignore (finish_node state kind saved)

(** [parse_signature_item state] parses a signature item. *)
and parse_signature_item state =
  let saved = start_node state in
  let kind =
    match current_kind state with
    | TK_VAL ->
        bump state;
        if at state TK_LOWERCASE_IDENT then bump state;
        expect state TK_COLON;
        parse_type state;
        Syntax_kind.NK_VALUE_SPECIFICATION
    | TK_TYPE ->
        bump state;
        parse_type_declaration state;
        while at state TK_AND do
          bump state;
          parse_type_declaration state
        done;
        Syntax_kind.NK_TYPE_SPECIFICATION
    | TK_MODULE ->
        bump state;
        if at state TK_TYPE then (
          bump state;
          if at state TK_UPPERCASE_IDENT then bump state;
          if at state TK_EQUAL then (
            bump state;
            parse_module_type state);
          Syntax_kind.NK_MODULE_TYPE_SPECIFICATION)
        else (
          if at state TK_UPPERCASE_IDENT then bump state;
          expect state TK_COLON;
          parse_module_type state;
          Syntax_kind.NK_MODULE_SPECIFICATION)
    | TK_INCLUDE ->
        bump state;
        parse_module_type state;
        Syntax_kind.NK_INCLUDE_SPECIFICATION
    | _ ->
        if not (at_eof state) then bump state;
        Syntax_kind.NK_ERROR
  in
  ignore (finish_node state kind saved)

(** [parse_external_declaration state] parses an external declaration. *)
and parse_external_declaration state =
  bump state;
  (* external *)
  if at state TK_LOWERCASE_IDENT then bump state;
  expect state TK_COLON;
  parse_type state;
  expect state TK_EQUAL;
  if at state TK_STRING then bump state

(** [parse_attribute state] parses an attribute (@name or @name(payload)). *)
and parse_attribute state =
  let saved = start_node state in
  bump state;
  (* @ *)
  if at state TK_LOWERCASE_IDENT then bump state;
  if at state TK_LPAREN then (
    let payload_saved = start_node state in
    bump state;
    while (not (at_eof state)) && not (at state TK_RPAREN) do
      bump state
    done;
    expect state TK_RPAREN;
    ignore (finish_node state NK_ATTRIBUTE_PAYLOAD payload_saved));
  ignore (finish_node state NK_ATTRIBUTE saved)

(** {1 Main Entry Points} *)

(** [parse_structure filename content] parses a complete source file.

    Returns the root green node of the CST. *)
let parse_structure filename content =
  let state = create filename content in
  let saved = start_node state in
  while not (at_eof state) do
    ignore (parse_structure_item state)
  done;
  (* Consume EOF token to capture any trailing trivia *)
  bump state;
  finish_node state NK_SOURCE_FILE saved

(** [parse filename content] parses source code and returns a red tree root.

    This is the main entry point for parsing with full navigation support. *)
let parse filename content =
  let green = parse_structure filename content in
  Red_tree.root green

(** [parse_expression_string filename content] parses a single expression.

    Useful for testing and REPL-like interfaces. *)
let parse_expression_string filename content =
  let state = create filename content in
  let saved = start_node state in
  parse_expression state;
  if not (at_eof state) then bump state;
  finish_node state NK_SOURCE_FILE saved
