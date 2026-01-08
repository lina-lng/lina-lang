(** Document state management for the LSP server. *)

(** Cached parse result for a document. *)
type parse_cache = {
  parse_version : int;
  ast : Parsing.Syntax_tree.structure option;
  parse_errors : Lsp_types.diagnostic list;
}

(** Cached type checking result for a document. *)
type typing_cache = {
  typing_version : int;
  typed_ast : Typing.Typed_tree.typed_structure option;
  environment : Typing.Environment.t;
  type_errors : Lsp_types.diagnostic list;
  warnings : Lsp_types.diagnostic list;
}

(** State of a single open document. *)
type document = {
  uri : string;
  version : int;
  content : string;
  line_offsets : int array;
  mutable parse_cache : parse_cache option;
  mutable typing_cache : typing_cache option;
}

(** The document store holding all open documents. *)
type t = {
  documents : (string, document) Hashtbl.t;
}

(** Compute byte offsets for each line in content. *)
let compute_line_offsets content =
  let offsets = ref [ 0 ] in
  String.iteri
    (fun i c -> if c = '\n' then offsets := (i + 1) :: !offsets)
    content;
  Array.of_list (List.rev !offsets)

(** Create an empty document store. *)
let create () = { documents = Hashtbl.create 16 }

(** Open a new document. *)
let open_document store ~uri ~content ~version =
  let line_offsets = compute_line_offsets content in
  let doc =
    {
      uri;
      version;
      content;
      line_offsets;
      parse_cache = None;
      typing_cache = None;
    }
  in
  Hashtbl.replace store.documents uri doc

(** Close and remove a document. *)
let close_document store ~uri = Hashtbl.remove store.documents uri

(** Update document content. *)
let update_document store ~uri ~content ~version =
  match Hashtbl.find_opt store.documents uri with
  | None -> open_document store ~uri ~content ~version
  | Some doc ->
      let line_offsets = compute_line_offsets content in
      let updated =
        {
          doc with
          version;
          content;
          line_offsets;
          parse_cache = None;
          typing_cache = None;
        }
      in
      Hashtbl.replace store.documents uri updated

(** Get document by URI. *)
let get_document store uri = Hashtbl.find_opt store.documents uri

(** Get all open document URIs. *)
let get_all_uris store =
  Hashtbl.fold (fun uri _ acc -> uri :: acc) store.documents []

(** Get document content. *)
let document_content store uri =
  match get_document store uri with
  | Some doc -> Some doc.content
  | None -> None

(** Convert LSP position (0-indexed) to byte offset. *)
let position_to_offset doc line character =
  if line < 0 || line >= Array.length doc.line_offsets then None
  else
    let line_start = doc.line_offsets.(line) in
    let line_end =
      if line + 1 < Array.length doc.line_offsets then
        doc.line_offsets.(line + 1) - 1
      else String.length doc.content
    in
    let offset = line_start + character in
    if offset >= line_start && offset <= line_end then Some offset else None

(** Convert byte offset to LSP position (0-indexed). *)
let offset_to_position doc offset =
  if offset < 0 || offset > String.length doc.content then None
  else
    let rec find_line line =
      if line + 1 >= Array.length doc.line_offsets then line
      else if doc.line_offsets.(line + 1) > offset then line
      else find_line (line + 1)
    in
    let line = find_line 0 in
    let character = offset - doc.line_offsets.(line) in
    Some (line, character)

(** Convert LSP position type to byte offset. *)
let lsp_position_to_offset doc (pos : Lsp_types.position) =
  position_to_offset doc pos.line pos.character

(** Convert byte offset to LSP position type. *)
let offset_to_lsp_position doc offset =
  match offset_to_position doc offset with
  | Some (line, character) -> Some Lsp_types.{ line; character }
  | None -> None

(** Invalidate all caches for a document. *)
let invalidate_caches store ~uri =
  match Hashtbl.find_opt store.documents uri with
  | None -> ()
  | Some doc ->
      doc.parse_cache <- None;
      doc.typing_cache <- None

(** Set the parse cache. *)
let set_parse_cache store ~uri cache =
  match Hashtbl.find_opt store.documents uri with
  | None -> ()
  | Some doc -> doc.parse_cache <- Some cache

(** Set the typing cache. *)
let set_typing_cache store ~uri cache =
  match Hashtbl.find_opt store.documents uri with
  | None -> ()
  | Some doc -> doc.typing_cache <- Some cache

(** Get the parse cache if valid for current version. *)
let get_parse_cache store uri =
  match Hashtbl.find_opt store.documents uri with
  | None -> None
  | Some doc -> (
      match doc.parse_cache with
      | Some cache when cache.parse_version = doc.version -> Some cache
      | _ -> None)

(** Get the typing cache if valid for current version. *)
let get_typing_cache store uri =
  match Hashtbl.find_opt store.documents uri with
  | None -> None
  | Some doc -> (
      match doc.typing_cache with
      | Some cache when cache.typing_version = doc.version -> Some cache
      | _ -> None)

(** Extract file path from file:// URI. *)
let filename_of_uri uri =
  if String.length uri > 7 && String.sub uri 0 7 = "file://" then
    String.sub uri 7 (String.length uri - 7)
  else uri

(** Create file:// URI from file path. *)
let uri_of_filename filename = "file://" ^ filename
