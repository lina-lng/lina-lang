(** Wadler-Lindig document IR and layout algorithm.

    This module provides a language-agnostic pretty printing abstraction based on
    Philip Wadler's "A Prettier Printer" and Christian Lindig's strict adaptation.

    The key insight is that documents describe {i what} layouts are possible,
    while the layout algorithm decides {i which} layout to use based on
    available width.

    @see <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
         Wadler's original paper
    @see <https://lindig.github.io/papers/strictly-pretty-2000.pdf>
         Lindig's strict adaptation *)

(** {1 Document Type} *)

(** The document intermediate representation.

    A document describes possible layouts without committing to specific
    line breaks. The layout algorithm later chooses the best layout. *)
type doc =
  | Empty
      (** The empty document. *)
  | Text of string
      (** Literal text. Must not contain newlines. *)
  | Concat of doc * doc
      (** Horizontal concatenation of two documents. *)
  | Nest of int * doc
      (** Increase indentation by [n] spaces for line breaks within [doc]. *)
  | Break of break_mode
      (** A potential line break point. *)
  | Group of doc
      (** Enable the choice between flat and broken layout.
          If the contents fit on one line, render flat (breaks become spaces).
          Otherwise, render broken (breaks become newlines). *)
  | Align of doc
      (** Set indentation to current column for nested content. *)
  | IfBreak of { broken : doc; flat : doc }
      (** Choose between two documents based on whether the enclosing group
          is rendered flat or broken. *)
  | LineSuffix of doc
      (** Content to be appended at end of line (used for trailing comments). *)
  | HardLine
      (** An unconditional line break that cannot be flattened. *)

(** Break modes for the [Break] constructor. *)
and break_mode =
  | Space
      (** Renders as a single space when flat, newline when broken. *)
  | Softline
      (** Renders as nothing when flat, newline when broken. *)
  | Literalline
      (** Always renders as a newline with no indentation.
          Used for preserving literal content like comments. *)

(** {1 Smart Constructors}

    Use these instead of the raw constructors to ensure invariants. *)

(** The empty document. *)
let empty = Empty

(** Create a text document.

    @raise Invalid_argument if the string contains a newline *)
let text s =
  if String.contains s '\n' then
    invalid_arg "Doc.text: string must not contain newlines"
  else if s = "" then Empty
  else Text s

(** A space that becomes a newline when the group is broken. *)
let space = Break Space

(** Nothing when flat, newline when broken. *)
let softline = Break Softline

(** A literal line break with no indentation.
    Used for preserving exact whitespace in comments. *)
let literalline = Break Literalline

(** An unconditional line break. *)
let hardline = HardLine

(** Concatenate two documents. *)
let concat a b =
  match a, b with
  | Empty, x | x, Empty -> x
  | a, b -> Concat (a, b)

(** Infix concatenation. *)
let ( ^^ ) = concat

(** Concatenate with a space between. *)
let ( ^/^ ) a b =
  match a, b with
  | Empty, x | x, Empty -> x
  | a, b -> a ^^ space ^^ b

(** Concatenate with a softline between. *)
let ( ^//^ ) a b =
  match a, b with
  | Empty, x | x, Empty -> x
  | a, b -> a ^^ softline ^^ b

(** Concatenate with a hardline between. *)
let ( ^|^ ) a b =
  match a, b with
  | Empty, x | x, Empty -> x
  | a, b -> a ^^ hardline ^^ b

(** Nest: increase indentation for line breaks within this document. *)
let nest n d =
  match d with
  | Empty -> Empty
  | _ when n = 0 -> d
  | _ -> Nest (n, d)

(** Group: enable flat/broken layout choice. *)
let group d =
  match d with
  | Empty -> Empty
  | Group _ -> d (* Avoid nested groups *)
  | _ -> Group d

(** Align: set indentation to current column. *)
let align d =
  match d with
  | Empty -> Empty
  | _ -> Align d

(** Choose between documents based on whether enclosing group breaks. *)
let if_break ~broken ~flat =
  IfBreak { broken; flat }

(** Content to append at end of line (for trailing comments). *)
let line_suffix d = LineSuffix d

(** {1 Derived Combinators} *)

(** The default indentation width. *)
let indent_width = 2

(** Indent a document by the default width. *)
let indent d = nest indent_width d

(** Concatenate a list of documents. *)
let concat_list docs = List.fold_left concat empty docs

(** Separate documents with a separator. *)
let separate sep docs =
  match docs with
  | [] -> empty
  | [d] -> d
  | d :: ds -> List.fold_left (fun acc x -> acc ^^ sep ^^ x) d ds

(** Join documents with a separator that breaks appropriately. *)
let join sep docs =
  match docs with
  | [] -> empty
  | [d] -> d
  | _ -> separate (sep ^^ space) docs

(** Surround content with delimiters. *)
let surround left content right =
  left ^^ content ^^ right

(** Surround with optional soft breaks. *)
let soft_surround left content right =
  group (left ^^ align (softline ^^ content ^^ softline) ^^ right)

(** A block: opening, indented body, closing.
    If it fits on one line, keep it flat. Otherwise break. *)
let block opening body closing =
  group (
    opening ^^
    nest indent_width (softline ^^ body) ^^
    softline ^^ closing
  )

(** Prefix: header on same line, body indented below if it doesn't fit. *)
let prefix header body =
  group (header ^^ nest indent_width (softline ^^ body))

(** Infix: operator between left and right operands. *)
let infix op left right =
  group (left ^/^ op ^^ nest indent_width (softline ^^ right))

(** Fill: fit as many items per line as possible. *)
let fill docs =
  match docs with
  | [] -> empty
  | [d] -> d
  | _ ->
    let rec go = function
      | [] -> empty
      | [d] -> d
      | d :: ds ->
        d ^^ if_break ~broken:hardline ~flat:space ^^ go ds
    in
    go docs

(** Common delimiters *)
let lparen = text "("
let rparen = text ")"
let lbrace = text "{"
let rbrace = text "}"
let lbracket = text "["
let rbracket = text "]"
let comma = text ","
let semi = text ";"
let colon = text ":"
let equals = text "="
let arrow = text "->"
let fat_arrow = text "=>"
let pipe = text "|"
let dot = text "."

(** Wrap in parentheses. *)
let parens d = surround lparen d rparen

(** Wrap in braces. *)
let braces d = surround lbrace d rbrace

(** Wrap in brackets. *)
let brackets d = surround lbracket d rbracket

(** {1 Layout Algorithm}

    The Wadler-Lindig layout algorithm with O(n) time complexity. *)

(** Layout mode: whether to render breaks as spaces or newlines. *)
type mode =
  | Flat   (** Breaks become spaces *)
  | Broken (** Breaks become newlines *)

(** Command for the layout algorithm. *)
type cmd = {
  indent : int;   (** Current indentation level *)
  mode : mode;    (** Current layout mode *)
  doc : doc;      (** Document to process *)
}

(** Check if a document fits in the remaining width.

    This only looks ahead until a newline or width exhaustion,
    making the overall algorithm O(n). *)
let rec fits remaining cmds =
  if remaining < 0 then false
  else
    match cmds with
    | [] -> true
    | { doc = Empty; _ } :: rest ->
        fits remaining rest
    | { indent; mode; doc = Concat (a, b) } :: rest ->
        fits remaining ({ indent; mode; doc = a } :: { indent; mode; doc = b } :: rest)
    | { indent; mode; doc = Nest (n, d) } :: rest ->
        fits remaining ({ indent = indent + n; mode; doc = d } :: rest)
    | { doc = Text s; _ } :: rest ->
        fits (remaining - String.length s) rest
    | { mode = Flat; doc = Break Space; _ } :: rest ->
        fits (remaining - 1) rest
    | { mode = Flat; doc = Break Softline; _ } :: rest ->
        fits remaining rest
    | { doc = Break Literalline; _ } :: _ ->
        true (* Literalline always breaks *)
    | { mode = Broken; doc = Break _; _ } :: _ ->
        true (* Newline always fits *)
    | { doc = HardLine; _ } :: _ ->
        true (* Hardline always fits *)
    | { indent; doc = Group d; _ } :: rest ->
        fits remaining ({ indent; mode = Flat; doc = d } :: rest)
    | { indent; mode; doc = Align d } :: rest ->
        fits remaining ({ indent; mode; doc = d } :: rest)
    | { mode = Flat; doc = IfBreak { flat; _ }; _ } :: rest ->
        fits remaining ({ indent = 0; mode = Flat; doc = flat } :: rest)
    | { indent; mode = Broken; doc = IfBreak { broken; _ } } :: rest ->
        fits remaining ({ indent; mode = Broken; doc = broken } :: rest)
    | { doc = LineSuffix _; _ } :: rest ->
        fits remaining rest

(** Render a document to a string with the given line width.

    @param width Target line width (default 80)
    @param doc The document to render
    @return The rendered string *)
let render ?(width = 80) doc =
  let buf = Buffer.create 256 in

  (* Pending line suffix documents to be output before next newline *)
  let line_suffix_pending = ref [] in

  (* Process pending line suffixes - render them inline *)
  let rec render_suffix_docs column cmds =
    match cmds with
    | [] -> column
    | cmd :: rest ->
        let new_col = process_cmd column cmd in
        render_suffix_docs new_col rest

  (* Output a newline with indentation, flushing any pending suffixes first *)
  and output_newline indent =
    (* Flush pending line suffixes *)
    if !line_suffix_pending <> [] then begin
      let suffix_cmds = List.rev_map (fun d ->
        { indent = 0; mode = Flat; doc = d }
      ) !line_suffix_pending in
      ignore (render_suffix_docs 0 suffix_cmds);
      line_suffix_pending := []
    end;
    Buffer.add_char buf '\n';
    Buffer.add_string buf (String.make indent ' ')

  (* Process a single command, returning new column position *)
  and process_cmd column cmd =
    match cmd.doc with
    | Empty -> column
    | Text s ->
        Buffer.add_string buf s;
        column + String.length s
    | Concat (a, b) ->
        let col1 = process_cmd column { cmd with doc = a } in
        process_cmd col1 { cmd with doc = b }
    | Nest (n, d) ->
        process_cmd column { cmd with indent = cmd.indent + n; doc = d }
    | Break Space when cmd.mode = Flat ->
        Buffer.add_char buf ' ';
        column + 1
    | Break Softline when cmd.mode = Flat ->
        column
    | Break Literalline ->
        (* Literalline: newline without indentation *)
        output_newline 0;
        0
    | Break _ ->
        output_newline cmd.indent;
        cmd.indent
    | HardLine ->
        output_newline cmd.indent;
        cmd.indent
    | Group d ->
        if fits (width - column) [{ cmd with mode = Flat; doc = d }] then
          process_cmd column { cmd with mode = Flat; doc = d }
        else
          process_cmd column { cmd with mode = Broken; doc = d }
    | Align d ->
        process_cmd column { indent = column; mode = cmd.mode; doc = d }
    | IfBreak { flat; _ } when cmd.mode = Flat ->
        process_cmd column { indent = 0; mode = Flat; doc = flat }
    | IfBreak { broken; _ } ->
        process_cmd column { cmd with doc = broken }
    | LineSuffix d ->
        (* Defer this document until end of line *)
        line_suffix_pending := d :: !line_suffix_pending;
        column
  in

  (* Main processing *)
  let _ = process_cmd 0 { indent = 0; mode = Flat; doc = Group doc } in

  (* Flush any remaining line suffixes *)
  if !line_suffix_pending <> [] then begin
    let suffix_cmds = List.rev_map (fun d ->
      { indent = 0; mode = Flat; doc = d }
    ) !line_suffix_pending in
    ignore (render_suffix_docs 0 suffix_cmds)
  end;

  Buffer.contents buf

(** {1 Pretty Printing} *)

(** Pretty-print a document with default settings.
    Alias for [render ~width:80 doc]. *)
let pretty doc = render ~width:80 doc

(** Pretty-print a document to a channel. *)
let output ?(width = 80) channel doc =
  output_string channel (render ~width doc)
