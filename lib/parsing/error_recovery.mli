(** Error recovery for tolerant parsing.

    This module provides synchronization and recovery strategies for
    handling syntax errors during incremental parsing. The goal is to
    produce partial ASTs with error nodes rather than aborting on the
    first syntax error. *)

(** {1 Synchronization Tokens} *)

(** Check if a token marks the beginning of a new structure item.
    Used to recover from errors at the top level. *)
val is_structure_sync_token : Parser.token -> bool

(** Check if a token marks the end of an expression context.
    Used to recover from errors within expressions. *)
val is_expression_sync_token : Parser.token -> bool

(** {1 Lexer State} *)

(** State for incremental lexing during error recovery. *)
type lexer_state

(** Create a new lexer state. *)
val create_lexer_state : string -> string -> lexer_state

(** Read the next token and update state.
    @return (token, start_position, end_position) *)
val read_token : lexer_state -> Parser.token * Lexing.position * Lexing.position

(** {1 Recovery Operations} *)

(** Skip tokens until we find a structure-level synchronization point.
    @return The location spanning the skipped tokens *)
val synchronize_to_structure : lexer_state -> Common.Location.t

(** {1 Error Node Constructors} *)

(** Create an error structure item from an error span and message. *)
val make_structure_error : Common.Location.t -> string -> Syntax_tree.structure_item

(** Create an error expression from an error span and message. *)
val make_expression_error : Common.Location.t -> string -> Syntax_tree.expression

(** Create an error pattern from an error span and message. *)
val make_pattern_error : Common.Location.t -> string -> Syntax_tree.pattern
