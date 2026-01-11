(** Wadler-Lindig document IR and layout algorithm.

    This module provides a language-agnostic pretty printing abstraction based on
    Philip Wadler's "A Prettier Printer" and Christian Lindig's strict adaptation.

    The key insight is that documents describe {i what} layouts are possible,
    while the layout algorithm decides {i which} layout to use based on
    available width.

    {2 Basic Usage}

    {[
      open Doc

      let doc =
        group (
          text "let" ^/^ text "x" ^/^ text "=" ^^
          nest 2 (softline ^^ text "1 + 2")
        )

      let () = print_endline (render ~width:40 doc)
      (* Output: let x = 1 + 2 *)

      let () = print_endline (render ~width:10 doc)
      (* Output:
         let x =
           1 + 2
      *)
    ]}

    @see <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
         Wadler's original paper
    @see <https://lindig.github.io/papers/strictly-pretty-2000.pdf>
         Lindig's strict adaptation *)

(** {1 Document Type} *)

(** The document intermediate representation.

    A document describes possible layouts without committing to specific
    line breaks. The layout algorithm later chooses the best layout. *)
type doc

(** {1 Smart Constructors} *)

(** The empty document. *)
val empty : doc

(** [text s] creates a document containing the literal string [s].

    @raise Invalid_argument if [s] contains a newline character *)
val text : string -> doc

(** A space that becomes a newline when the enclosing group is broken. *)
val space : doc

(** Nothing when flat, newline when broken. *)
val softline : doc

(** A literal line break with no indentation.
    Used for preserving exact whitespace in comments and other literal content. *)
val literalline : doc

(** An unconditional line break that cannot be flattened. *)
val hardline : doc

(** [concat a b] concatenates documents [a] and [b] horizontally. *)
val concat : doc -> doc -> doc

(** Infix version of {!concat}. *)
val ( ^^ ) : doc -> doc -> doc

(** [a ^/^ b] concatenates with a space that may become a newline. *)
val ( ^/^ ) : doc -> doc -> doc

(** [a ^//^ b] concatenates with a softline between. *)
val ( ^//^ ) : doc -> doc -> doc

(** [a ^|^ b] concatenates with a hardline between. *)
val ( ^|^ ) : doc -> doc -> doc

(** [nest n doc] increases indentation by [n] for line breaks in [doc]. *)
val nest : int -> doc -> doc

(** [group doc] enables the choice between flat and broken layout.

    If the contents fit on one line, render flat (breaks become spaces/nothing).
    Otherwise, render broken (breaks become newlines with indentation). *)
val group : doc -> doc

(** [align doc] sets the indentation to the current column for [doc]. *)
val align : doc -> doc

(** [if_break ~broken ~flat] chooses between documents based on whether
    the enclosing group is rendered flat or broken. *)
val if_break : broken:doc -> flat:doc -> doc

(** [line_suffix doc] appends [doc] at the end of the current line.
    Used for trailing comments. *)
val line_suffix : doc -> doc

(** {1 Derived Combinators} *)

(** Get the current indentation width.
    Default is 2 spaces. Use {!set_indent_width} to change. *)
val indent_width : unit -> int

(** Set the indentation width for subsequent formatting.

    @param n Number of spaces per indent level (must be >= 0)
    @raise Invalid_argument if [n] is negative *)
val set_indent_width : int -> unit

(** [indent doc] is [nest (indent_width ()) doc]. *)
val indent : doc -> doc

(** [concat_list docs] concatenates a list of documents. *)
val concat_list : doc list -> doc

(** [separate sep docs] separates documents with [sep]. *)
val separate : doc -> doc list -> doc

(** [join sep docs] joins documents with [sep] and a space. *)
val join : doc -> doc list -> doc

(** [surround left content right] wraps [content] with delimiters. *)
val surround : doc -> doc -> doc -> doc

(** [soft_surround left content right] wraps with optional breaks inside. *)
val soft_surround : doc -> doc -> doc -> doc

(** [block opening body closing] creates a block that breaks if needed.

    Example: [block (text "\{") body (text "\}")] produces
    - [\{ body \}] if it fits on one line
    - [\{\n  body\n\}] otherwise *)
val block : doc -> doc -> doc -> doc

(** [prefix header body] keeps [header] on the same line, indents [body]. *)
val prefix : doc -> doc -> doc

(** [infix op left right] formats an infix operation. *)
val infix : doc -> doc -> doc -> doc

(** [fill docs] fits as many items per line as possible. *)
val fill : doc list -> doc

(** {1 Common Delimiters} *)

val lparen : doc    (** [(] *)
val rparen : doc    (** [)] *)
val lbrace : doc    (** [\{] *)
val rbrace : doc    (** [\}] *)
val lbracket : doc  (** [\[] *)
val rbracket : doc  (** [\]] *)
val comma : doc     (** [,] *)
val semi : doc      (** [;] *)
val colon : doc     (** [:] *)
val equals : doc    (** [=] *)
val arrow : doc     (** [->] *)
val fat_arrow : doc (** [=>] *)
val pipe : doc      (** [|] *)
val dot : doc       (** [.] *)

(** [parens doc] wraps [doc] in parentheses. *)
val parens : doc -> doc

(** [braces doc] wraps [doc] in braces. *)
val braces : doc -> doc

(** [brackets doc] wraps [doc] in brackets. *)
val brackets : doc -> doc

(** {1 Layout Algorithm} *)

(** [render ~width doc] renders [doc] to a string.

    The algorithm tries to fit content on each line within [width] characters.
    When a group doesn't fit, it breaks at the designated break points.

    @param width Target line width (default 80)
    @return The rendered string *)
val render : ?width:int -> doc -> string

(** [pretty doc] is [render ~width:80 doc]. *)
val pretty : doc -> string

(** [output ~width channel doc] renders [doc] to [channel]. *)
val output : ?width:int -> out_channel -> doc -> unit
