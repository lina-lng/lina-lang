(** Interactive REPL for Lina. *)

(** {1 State} *)

type state = {
  mutable env : Typing.Environment.t;
}

let create () = {
  env = Typing.Environment.initial;
}

(** {1 Helpers} *)

(** Pretty-print a type expression. *)
let type_to_string ty =
  let open Typing.Types in
  let rec pp ty =
    match representative ty with
    | TypeVariable tv ->
      if tv.weak then Printf.sprintf "'_%d" tv.id
      else Printf.sprintf "'%d" tv.id
    | TypeConstructor (path, []) ->
      path_to_string path
    | TypeConstructor (path, args) ->
      let args_str = String.concat ", " (List.map pp args) in
      Printf.sprintf "%s<%s>" (path_to_string path) args_str
    | TypeArrow (arg, result) ->
      Printf.sprintf "(%s -> %s)" (pp arg) (pp result)
    | TypeTuple elements ->
      Printf.sprintf "(%s)" (String.concat " * " (List.map pp elements))
    | TypeRecord row ->
      Printf.sprintf "{ %s }" (pp_row row)
    | TypeRowEmpty -> ""
  and pp_row row =
    let fields = List.map (fun (label, field) ->
      match field with
      | RowFieldPresent ty -> Printf.sprintf "%s: %s" label (pp ty)
    ) row.row_fields in
    let more = match representative row.row_more with
      | TypeRowEmpty -> ""
      | TypeVariable tv -> Printf.sprintf "; ..%d" tv.id
      | _ -> ""
    in
    String.concat "; " fields ^ more
  in
  pp ty

(** Pretty-print a type scheme. *)
let scheme_to_string scheme =
  let open Typing.Types in
  if scheme.quantified_variables = [] then
    type_to_string scheme.body
  else
    let vars = List.map (fun tv -> Printf.sprintf "'%d" tv.id)
      scheme.quantified_variables in
    Printf.sprintf "forall %s. %s"
      (String.concat " " vars)
      (type_to_string scheme.body)

(** Parse an expression from a string. *)
let parse_expression source =
  try
    let state = Parsing.Lexer.create_state "<repl>" source in
    let lexer () =
      let token, loc = Parsing.Lexer.next_token state in
      let start_pos = Parsing.Parsing_utils.location_to_lexing_position loc.Common.Location.start_pos in
      let end_pos = Parsing.Parsing_utils.location_to_lexing_position loc.Common.Location.end_pos in
      (Parsing.Parsing_utils.lexer_token_to_parser_token token, start_pos, end_pos)
    in
    let parser = MenhirLib.Convert.Simplified.traditional2revised
      Parsing.Parser.expression_eof in
    Ok (parser lexer)
  with
  | Parsing.Parser.Error ->
    Error "Syntax error"
  | Common.Compiler_error.Error err ->
    Error (Common.Compiler_error.report_to_string err)

(** Parse a structure (let bindings, type definitions, etc.) from a string. *)
let parse_structure source =
  try
    Ok (Parsing.Parse.structure_from_string source)
  with
  | Common.Compiler_error.Error err ->
    Error (Common.Compiler_error.report_to_string err)

(** {1 Commands} *)

let show_help () =
  {|Lina REPL Commands:
  :type <expr>   Show the type of an expression
  :load <file>   Load and evaluate a Lina source file
  :env           Show all bindings in the current environment
  :clear         Reset the environment to initial state
  :help          Show this help message
  :quit, :q      Exit the REPL

Expressions are evaluated and their value is printed.
Let bindings persist across lines:
  let x = 42
  x + 1         -- evaluates to 43

For line editing, use: rlwrap lina repl|}

let type_of state expr_source =
  match parse_expression expr_source with
  | Error msg -> Error msg
  | Ok expr ->
    try
      let ctx = Typing.Typing_context.create state.env in
      let typed, _ctx = Typing.Inference.infer_expression ctx expr in
      Ok (type_to_string typed.expression_type)
    with
    | Common.Compiler_error.Error err ->
      Error (Common.Compiler_error.report_to_string err)

let load_file state path =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "File not found: %s" path)
  else begin
    let source = In_channel.with_open_text path In_channel.input_all in
    match parse_structure source with
    | Error msg -> Error msg
    | Ok structure ->
      try
        let ctx = Typing.Typing_context.create state.env in
        let typed, new_ctx = Typing.Inference.infer_structure ctx structure in
        state.env <- Typing.Typing_context.environment new_ctx;
        (* Count how many new bindings were added *)
        let count = List.length typed in
        Ok (Printf.sprintf "Loaded %s (%d item%s)"
          path count (if count = 1 then "" else "s"))
      with
      | Common.Compiler_error.Error err ->
        Error (Common.Compiler_error.report_to_string err)
  end

let show_env state =
  let values = Typing.Environment.fold_values
    (fun name _id scheme acc ->
      Printf.sprintf "  val %s : %s" name (scheme_to_string scheme) :: acc)
    state.env []
  in
  let types = Typing.Environment.fold_types
    (fun name decl acc ->
      let kind = match decl.Typing.Types.declaration_kind with
        | Typing.Types.DeclarationAbstract -> "abstract"
        | Typing.Types.DeclarationVariant _ -> "variant"
        | Typing.Types.DeclarationRecord _ -> "record"
      in
      Printf.sprintf "  type %s [%s]" name kind :: acc)
    state.env []
  in
  let modules = Typing.Environment.fold_modules
    (fun name _binding acc ->
      Printf.sprintf "  module %s" name :: acc)
    state.env []
  in
  let sections = [] in
  let sections = if values <> [] then
    ("Values:" :: List.rev values) :: sections else sections in
  let sections = if types <> [] then
    ("Types:" :: List.rev types) :: sections else sections in
  let sections = if modules <> [] then
    ("Modules:" :: List.rev modules) :: sections else sections in
  if sections = [] then
    "Environment is empty."
  else
    String.concat "\n\n" (List.map (String.concat "\n") (List.rev sections))

let clear state =
  state.env <- Typing.Environment.initial

(** {1 Evaluation} *)

(** Execute Lua code and capture output. *)
let execute_lua lua_code =
  (* Write to temp file *)
  let temp_file = Filename.temp_file "lina_repl_" ".lua" in
  let () =
    let oc = open_out temp_file in
    output_string oc lua_code;
    close_out oc
  in
  (* Run with lua/luajit and capture output *)
  let cmd = Printf.sprintf "luajit %s 2>&1 || lua %s 2>&1" temp_file temp_file in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  Sys.remove temp_file;
  String.trim output

(** Evaluate a line of input. Returns a message to display. *)
let eval_input state source =
  (* Try to parse as a structure item first (let binding, type def, etc.) *)
  match parse_structure source with
  | Ok structure when structure <> [] ->
    begin try
      let ctx = Typing.Typing_context.create state.env in
      let typed, new_ctx = Typing.Inference.infer_structure ctx structure in
      state.env <- Typing.Typing_context.environment new_ctx;

      (* Generate Lua and execute for side effects *)
      let lambda = Lambda.translate_structure typed in
      let lua_ast = Lua.Codegen.generate lambda in
      let lua_code = Lua.Printer.print_chunk lua_ast in
      let output = execute_lua lua_code in

      (* Report what was defined *)
      let item = List.hd typed in
      (* Helper to extract name from pattern *)
      let pattern_name pat =
        match pat.Typing.Typed_tree.pattern_desc with
        | Typing.Typed_tree.TypedPatternVariable id -> Common.Identifier.name id
        | _ -> "_"
      in
      let msg = match item.structure_item_desc with
        | Typing.Typed_tree.TypedStructureValue (_, bindings) ->
          let names = List.map (fun b ->
            let name = pattern_name b.Typing.Typed_tree.binding_pattern in
            let scheme = {
              Typing.Types.quantified_variables = [];
              body = b.Typing.Typed_tree.binding_expression.expression_type;
            } in
            Printf.sprintf "val %s : %s" name (scheme_to_string scheme))
            bindings
          in
          String.concat "\n" names
        | Typing.Typed_tree.TypedStructureType decls ->
          let names = List.map (fun d ->
            Printf.sprintf "type %s" d.Typing.Types.declaration_name)
            decls
          in
          String.concat "\n" names
        | Typing.Typed_tree.TypedStructureModule (id, _) ->
          Printf.sprintf "module %s" (Common.Identifier.name id)
        | _ -> "ok"
      in
      if output = "" then msg
      else Printf.sprintf "%s\n%s" output msg
    with
    | Common.Compiler_error.Error err ->
      Printf.sprintf "Error: %s" (Common.Compiler_error.report_to_string err)
    end
  | _ ->
    (* Try to parse as an expression *)
    match parse_expression source with
    | Error msg -> Printf.sprintf "Error: %s" msg
    | Ok expr ->
      begin try
        let ctx = Typing.Typing_context.create state.env in
        let typed, _ctx = Typing.Inference.infer_expression ctx expr in
        let type_str = type_to_string typed.expression_type in

        (* Wrap expression in print to see result *)
        let wrapped_source = Printf.sprintf "let _ = print (%s)" source in
        match parse_structure wrapped_source with
        | Error _ ->
          Printf.sprintf "- : %s = <cannot print>" type_str
        | Ok structure ->
          begin try
            let typed_wrapped, _ =
              Typing.Inference.infer_structure ctx structure in
            let lambda = Lambda.translate_structure typed_wrapped in
            let lua_ast = Lua.Codegen.generate lambda in
            let lua_code = Lua.Printer.print_chunk lua_ast in
            let output = execute_lua lua_code in
            Printf.sprintf "- : %s = %s" type_str output
          with _ ->
            Printf.sprintf "- : %s" type_str
          end
      with
      | Common.Compiler_error.Error err ->
        Printf.sprintf "Error: %s" (Common.Compiler_error.report_to_string err)
      end

(** {1 Command Parsing} *)

let parse_command line =
  let line = String.trim line in
  if String.length line = 0 then `Empty
  else if line.[0] = ':' then begin
    let parts = String.split_on_char ' ' line in
    match parts with
    | [":quit"] | [":q"] -> `Quit
    | [":help"] | [":h"] -> `Help
    | [":env"] -> `Env
    | [":clear"] -> `Clear
    | ":type" :: rest -> `Type (String.concat " " rest)
    | ":t" :: rest -> `Type (String.concat " " rest)
    | ":load" :: rest -> `Load (String.concat " " rest)
    | ":l" :: rest -> `Load (String.concat " " rest)
    | cmd :: _ -> `Unknown cmd
    | [] -> `Empty
  end
  else `Eval line

(** {1 Main Loop} *)

let eval_line state line =
  match parse_command line with
  | `Empty -> true
  | `Quit -> false
  | `Help ->
    print_endline (show_help ());
    true
  | `Env ->
    print_endline (show_env state);
    true
  | `Clear ->
    clear state;
    print_endline "Environment cleared.";
    true
  | `Type expr ->
    begin match type_of state expr with
    | Ok ty -> print_endline ty
    | Error msg -> print_endline (Printf.sprintf "Error: %s" msg)
    end;
    true
  | `Load path ->
    begin match load_file state path with
    | Ok msg -> print_endline msg
    | Error msg -> print_endline (Printf.sprintf "Error: %s" msg)
    end;
    true
  | `Unknown cmd ->
    print_endline (Printf.sprintf "Unknown command: %s (try :help)" cmd);
    true
  | `Eval source ->
    print_endline (eval_input state source);
    true

let run ?(prompt = "lina> ") state =
  print_endline "Lina REPL (type :help for commands, :quit to exit)";
  print_newline ();
  let continue = ref true in
  while !continue do
    print_string prompt;
    flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None ->
      print_newline ();
      continue := false
    | Some line ->
      continue := eval_line state line
  done;
  print_endline "Goodbye!"
