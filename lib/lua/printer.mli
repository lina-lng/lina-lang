(** Lua AST pretty printer.

    This module converts Lua AST nodes to formatted source code strings.
    The output is valid Lua 5.1+ code that can be executed by Lua or LuaJIT.

    {2 Formatting}

    The printer produces readable output with:
    - Consistent indentation (2 spaces)
    - Proper semicolon placement
    - Minimal parentheses based on operator precedence
    - Line breaks after statements *)

(** [print_chunk chunk] converts a complete Lua chunk to source code.

    A chunk is a sequence of statements, typically representing a complete
    program or module.

    @param chunk The Lua AST chunk to print
    @return A string containing valid Lua source code *)
val print_chunk : Lua_ast.chunk -> string

(** [print_expression expr] converts a single expression to source code.

    Useful for debugging or generating code fragments.

    @param expr The expression to print
    @return A string containing the Lua expression *)
val print_expression : Lua_ast.expression -> string

(** [print_statement stmt] converts a single statement to source code.

    Useful for debugging or generating code fragments.

    @param stmt The statement to print
    @return A string containing the Lua statement *)
val print_statement : Lua_ast.statement -> string
