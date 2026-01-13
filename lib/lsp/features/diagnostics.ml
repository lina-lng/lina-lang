(** Diagnostic computation for the LSP server. *)

open Common

(** Convert an accumulated error to a diagnostic. *)
let diagnostic_of_accumulated_error (err : Typing.Error_accumulator.error) :
    Lsp_types.diagnostic =
  let message = Typing.Error_accumulator.error_message err in
  let code =
    match err.kind with
    | Typing.Error_accumulator.TypeError _ -> Some "type"
    | Typing.Error_accumulator.UnboundVariable _ -> Some "unbound-variable"
    | Typing.Error_accumulator.UnboundType _ -> Some "unbound-type"
    | Typing.Error_accumulator.UnboundConstructor _ -> Some "unbound-constructor"
    | Typing.Error_accumulator.UnboundModule _ -> Some "unbound-module"
    | Typing.Error_accumulator.UnificationError _ -> Some "unification"
    | Typing.Error_accumulator.PatternError _ -> Some "pattern"
    | Typing.Error_accumulator.ModuleError _ -> Some "module"
  in
  Lsp_types.make_diagnostic ~severity:Error ~message ?code ~hints:err.hints
    err.location

(** Convert a compiler warning to a diagnostic. *)
let diagnostic_of_warning (info : Compiler_error.warning_info) : Lsp_types.diagnostic
    =
  let message = Compiler_error.warning_to_string info.warning in
  let code =
    match info.warning with
    | Compiler_error.NonExhaustiveMatch _ -> Some "non-exhaustive"
    | Compiler_error.RedundantPattern -> Some "redundant-pattern"
  in
  Lsp_types.make_diagnostic ~severity:Warning ~message ?code info.warning_location

(** Convert a parse error message to a diagnostic.
    Parse errors from tolerant parsing include location info in the message. *)
let diagnostic_of_parse_error (message : string) : Lsp_types.diagnostic =
  (* Parse error messages are in format "Syntax error at line N, column M" *)
  Lsp_types.make_diagnostic ~severity:Error ~message ~code:"syntax" Location.none

(** Parse a document and return AST with any parse errors.
    Uses tolerant parsing to produce partial ASTs even with syntax errors,
    enabling IDE features like hover and completion on invalid code. *)
let parse_document store uri =
  (* Check cache first *)
  match Document_store.get_parse_cache store uri with
  | Some cache -> (cache.ast, cache.parse_errors)
  | None -> (
      match Document_store.get_document store uri with
      | None -> (None, [])
      | Some doc ->
          let filename = Document_store.filename_of_uri uri in
          let result =
            try
              (* Use tolerant parsing to get partial AST even with errors *)
              let ast, error_messages =
                Parsing.Parse.structure_from_string_tolerant ~filename doc.content
              in
              let parse_errors = List.map diagnostic_of_parse_error error_messages in
              (* Return AST even if there were errors - it may contain error nodes
                 but valid parts can still be used for IDE features *)
              if ast = [] && error_messages <> [] then
                (None, parse_errors)
              else
                (Some ast, parse_errors)
            with
            | Compiler_error.Error err ->
                (* Lexer errors still raise exceptions *)
                (None, [ Lsp_types.diagnostic_of_compiler_error err ])
            | _ ->
                let diag =
                  Lsp_types.make_diagnostic ~severity:Error
                    ~message:"Parse error" ~code:"parser" Location.none
                in
                (None, [ diag ])
          in
          let cache : Document_store.parse_cache =
            {
              parse_version = doc.version;
              ast = fst result;
              parse_errors = snd result;
            }
          in
          Document_store.set_parse_cache store ~uri cache;
          result)

(** Type check a document and return typed AST with errors. *)
let type_check_document store uri =
  (* Check cache first *)
  match Document_store.get_typing_cache store uri with
  | Some cache -> (cache.typed_ast, cache.environment, cache.type_errors @ cache.warnings)
  | None -> (
      (* Need to parse first *)
      let ast_opt, parse_errors = parse_document store uri in
      match ast_opt with
      | None ->
          (* Parse failed, return empty with parse errors *)
          (None, Typing.Environment.initial, parse_errors)
      | Some ast -> (
          match Document_store.get_document store uri with
          | None -> (None, Typing.Environment.initial, [])
          | Some doc ->
              (* Clear warnings before type checking *)
              Compiler_error.clear_warnings ();
              Typing.Types.reset_type_variable_id ();

              (* Use error-tolerant inference to preserve environment even with errors *)
              let ctx = Typing.Typing_context.create Typing.Environment.initial in
              let typed_ast, final_ctx, errors =
                Typing.Inference.infer_structure_tolerant ctx ast
              in
              let env = Typing.Typing_context.environment final_ctx in
              (* Collect warnings *)
              let warnings = Compiler_error.get_warnings () in
              let warning_diagnostics =
                List.map diagnostic_of_warning warnings
              in
              (* Convert inference errors to diagnostics *)
              let error_diagnostics =
                List.map (fun err ->
                  match err with
                  | Typing.Inference_utils.CompilerError compiler_err ->
                      Lsp_types.diagnostic_of_compiler_error compiler_err
                  | Typing.Inference_utils.UnificationError details ->
                      let full_message =
                        Printf.sprintf "%s\nExpected: %s\nActual: %s" details.message
                          (Typing.Types.type_expression_to_string details.expected)
                          (Typing.Types.type_expression_to_string details.actual)
                      in
                      Lsp_types.make_diagnostic ~severity:Error ~message:full_message
                        ~code:"unification" details.location
                ) errors
              in
              let result = (typed_ast, env, error_diagnostics, warning_diagnostics) in
              let typed_ast, env, type_errors, warnings = result in
              let cache : Document_store.typing_cache =
                {
                  typing_version = doc.version;
                  typed_ast;
                  environment = env;
                  type_errors;
                  warnings;
                }
              in
              Document_store.set_typing_cache store ~uri cache;
              (typed_ast, env, type_errors @ warnings)))

(** Compute all diagnostics for a document.
    With tolerant parsing, we continue to type checking even if there are
    parse errors, since we may have a partial AST that can be analyzed. *)
let compute_diagnostics store uri =
  let ast_opt, parse_errors = parse_document store uri in
  match ast_opt with
  | None ->
      (* No AST at all - just return parse errors *)
      parse_errors
  | Some _ ->
      (* Have an AST (possibly with error nodes) - run type checking too *)
      let _, _, type_diagnostics = type_check_document store uri in
      parse_errors @ type_diagnostics
