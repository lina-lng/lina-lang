(** Shared utilities for parsing.

    This module contains functions shared between the standard parser
    and the error-tolerant parser to avoid code duplication. *)

(** Convert a lexer token to a parser token.

    This function maps tokens from the Sedlex-based lexer to the Menhir
    parser's token type. Both have identical token definitions, but they
    are separate types due to the module structure. *)
val lexer_token_to_parser_token : Lexer.token -> Parser.token

(** Convert a Lina position to a Lexing.position.

    Lina positions are used throughout the compiler, while Lexing.position
    is required by Menhir's incremental API. *)
val location_to_lexing_position : Common.Location.position -> Lexing.position

(** Convert a Lexing.position to a Lina position.

    The inverse of {!location_to_lexing_position}. *)
val position_of_lexing : Lexing.position -> Common.Location.position

(** Create a Lina location from start and end Lexing positions.

    Useful when constructing location spans from Menhir parser state. *)
val location_from_positions : Lexing.position -> Lexing.position -> Common.Location.t
