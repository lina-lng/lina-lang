(** {1 Standard Parsing}

    These functions abort on the first syntax error. *)

(** Parse an expression from a string. Raises on syntax error. *)
val expression_from_string : string -> Syntax_tree.expression

(** Parse a structure from a string. Raises on syntax error. *)
val structure_from_string : string -> Syntax_tree.structure

(** Parse a structure from a file. Raises on syntax error. *)
val structure_from_file : string -> Syntax_tree.structure

(** {1 Tolerant Parsing with Error Recovery}

    These functions use Menhir's incremental API to recover from syntax errors,
    producing partial ASTs with error nodes instead of aborting. Error nodes
    (StructureError, ExpressionError, PatternError) are included in the AST
    where parsing failed, allowing downstream processing to continue. *)

(** Parse a structure from a string with error recovery.
    @param filename Optional filename for error messages (default: "<string>")
    @return The AST (possibly containing error nodes) and a list of error messages *)
val structure_from_string_tolerant :
  ?filename:string -> string -> Syntax_tree.structure * string list

(** Parse a structure from a file with error recovery.
    @return The AST (possibly containing error nodes) and a list of error messages *)
val structure_from_file_tolerant : string -> Syntax_tree.structure * string list
