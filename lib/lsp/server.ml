(** Lina LSP server implementation. *)

module Lsp = Linol.Lsp
open Lsp.Types

(** Server state containing document store and analysis caches. *)
type state = {
  document_store : Document_store.t;
  mutable workspace_root : string option;
}

(** Create initial server state. *)
let create_state () =
  { document_store = Document_store.create (); workspace_root = None }

(** Convert internal diagnostic to LSP diagnostic. *)
let lsp_diagnostic_of_internal (diag : Lsp_types.diagnostic) : Diagnostic.t =
  let range =
    Range.create
      ~start:
        (Position.create ~line:diag.range.start_pos.line
           ~character:diag.range.start_pos.character)
      ~end_:
        (Position.create ~line:diag.range.end_pos.line
           ~character:diag.range.end_pos.character)
  in
  let severity =
    match diag.severity with
    | Lsp_types.Error -> Some DiagnosticSeverity.Error
    | Lsp_types.Warning -> Some DiagnosticSeverity.Warning
    | Lsp_types.Information -> Some DiagnosticSeverity.Information
    | Lsp_types.Hint -> Some DiagnosticSeverity.Hint
  in
  Diagnostic.create ~range ?severity ~message:(`String diag.message)
    ?code:(Option.map (fun c -> `String c) diag.code)
    ?source:diag.source ()

(** The Lina LSP server class. *)
class lina_server =
  object (self)
    inherit Linol_eio.Jsonrpc2.server

    val state : state = create_state ()

    method get_state = state

    (** Spawn a query handler for concurrent processing. *)
    method spawn_query_handler f = Linol_eio.spawn f

    (** Configure text document synchronization. *)
    method! config_sync_opts =
      TextDocumentSyncOptions.create ~openClose:true
        ~change:TextDocumentSyncKind.Full
        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
        ()

    (** Enable hover support. *)
    method! config_hover = Some (`Bool true)

    (** Configure completion support. *)
    method! config_completion =
      Some
        (CompletionOptions.create ~triggerCharacters:[ "."; ":" ]
           ~resolveProvider:false ())

    (** Enable go-to-definition. *)
    method! config_definition = Some (`Bool true)

    (** Publish diagnostics for a document. *)
    method private publish_diagnostics notify_back uri =
      let diagnostics =
        Diagnostics.compute_diagnostics state.document_store uri
      in
      let lsp_diagnostics = List.map lsp_diagnostic_of_internal diagnostics in
      let params =
        PublishDiagnosticsParams.create
          ~uri:(DocumentUri.of_path uri)
          ~diagnostics:lsp_diagnostics ()
      in
      notify_back#send_notification
        (Lsp.Server_notification.PublishDiagnostics params)

    (** Handle document open notification. *)
    method on_notif_doc_did_open ~notify_back doc ~content =
      let uri = DocumentUri.to_path doc.uri in
      let version = doc.version in
      Document_store.open_document state.document_store ~uri ~content ~version;
      self#publish_diagnostics notify_back uri

    (** Handle document change notification. *)
    method on_notif_doc_did_change ~notify_back doc _changes ~old_content:_
        ~new_content =
      let uri = DocumentUri.to_path doc.uri in
      let version = doc.version in
      Document_store.update_document state.document_store ~uri ~content:new_content
        ~version;
      self#publish_diagnostics notify_back uri

    (** Handle document close notification. *)
    method on_notif_doc_did_close ~notify_back:_ doc =
      let uri = DocumentUri.to_path doc.uri in
      Document_store.close_document state.document_store ~uri

    (** Handle hover request. *)
    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _doc_state
        =
      let uri_str = DocumentUri.to_path uri in
      let lsp_pos : Lsp_types.position =
        { line = pos.line; character = pos.character }
      in
      match Lsp_hover.get_hover state.document_store uri_str lsp_pos with
      | None -> None
      | Some hover_result ->
          let range =
            Option.map
              (fun (r : Lsp_types.range) ->
                Range.create
                  ~start:
                    (Position.create ~line:r.start_pos.line
                       ~character:r.start_pos.character)
                  ~end_:
                    (Position.create ~line:r.end_pos.line
                       ~character:r.end_pos.character))
              hover_result.range
          in
          let contents =
            `MarkupContent
              (MarkupContent.create ~kind:MarkupKind.Markdown
                 ~value:hover_result.contents)
          in
          Some (Lsp.Types.Hover.create ~contents ?range ())

    (** Handle completion request. *)
    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      let uri_str = DocumentUri.to_path uri in
      let lsp_pos : Lsp_types.position =
        { line = pos.line; character = pos.character }
      in
      let items =
        Lsp_completion.get_completions state.document_store uri_str lsp_pos
      in
      let lsp_items =
        List.map
          (fun (item : Lsp_completion.completion_item) ->
            let kind =
              match item.kind with
              | Lsp_completion.Function -> Some CompletionItemKind.Function
              | Lsp_completion.Variable -> Some CompletionItemKind.Variable
              | Lsp_completion.Module -> Some CompletionItemKind.Module
              | Lsp_completion.Type -> Some CompletionItemKind.TypeParameter
              | Lsp_completion.Constructor -> Some CompletionItemKind.Constructor
              | Lsp_completion.Field -> Some CompletionItemKind.Field
              | Lsp_completion.Keyword -> Some CompletionItemKind.Keyword
            in
            CompletionItem.create ~label:item.label ?kind ?detail:item.detail
              ?documentation:
                (Option.map
                   (fun d -> `MarkupContent (MarkupContent.create ~kind:PlainText ~value:d))
                   item.documentation)
              ())
          items
      in
      Some (`CompletionList (CompletionList.create ~isIncomplete:false ~items:lsp_items ()))

    (** Handle go-to-definition request. *)
    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ _doc_state =
      let uri_str = DocumentUri.to_path uri in
      let lsp_pos : Lsp_types.position =
        { line = pos.line; character = pos.character }
      in
      match
        Lsp_definition.find_definition state.document_store uri_str lsp_pos
      with
      | None -> None
      | Some def_result ->
          let location =
            Location.create
              ~uri:(DocumentUri.of_path def_result.uri)
              ~range:
                (Range.create
                   ~start:
                     (Position.create ~line:def_result.range.start_pos.line
                        ~character:def_result.range.start_pos.character)
                   ~end_:
                     (Position.create ~line:def_result.range.end_pos.line
                        ~character:def_result.range.end_pos.character))
          in
          Some (`Location [ location ])

    (** Handle document symbols request. *)
    method! on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_
        ~partialResultToken:_ _doc_state =
      let uri_str = DocumentUri.to_path uri in
      let symbols =
        Lsp_symbols.get_document_symbols state.document_store uri_str
      in
      let lsp_symbols =
        List.map
          (fun (sym : Lsp_symbols.document_symbol) ->
            let kind =
              match sym.kind with
              | Lsp_symbols.Function -> SymbolKind.Function
              | Lsp_symbols.Variable -> SymbolKind.Variable
              | Lsp_symbols.Module -> SymbolKind.Module
              | Lsp_symbols.Type -> SymbolKind.TypeParameter
              | Lsp_symbols.Constructor -> SymbolKind.Constructor
              | Lsp_symbols.Field -> SymbolKind.Field
            in
            let range =
              Range.create
                ~start:
                  (Position.create ~line:sym.range.start_pos.line
                     ~character:sym.range.start_pos.character)
                ~end_:
                  (Position.create ~line:sym.range.end_pos.line
                     ~character:sym.range.end_pos.character)
            in
            DocumentSymbol.create ~name:sym.name ~kind ~range
              ~selectionRange:range ?detail:sym.detail
              ~children:[]
              ())
          symbols
      in
      Some (`DocumentSymbol lsp_symbols)
  end
