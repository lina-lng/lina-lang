(** Lua identifier mangling for reserved keywords.

    Lina identifiers may conflict with Lua reserved keywords.
    This module handles mangling such identifiers to avoid conflicts.

    {2 Reserved Keywords}

    Lua 5.1+ reserved keywords that cannot be used as identifiers:
    [and], [break], [do], [else], [elseif], [end], [false], [for],
    [function], [goto], [if], [in], [local], [nil], [not], [or],
    [repeat], [return], [then], [true], [until], [while]

    {2 Mangling Strategy}

    - Keywords are prefixed with underscore: [if] becomes [_if]
    - Non-zero stamps are appended: [x] with stamp 3 becomes [x_3]
    - Both transformations can combine: [if] with stamp 2 becomes [_if_2] *)

(** [is_lua_keyword name] returns [true] if [name] is a Lua reserved keyword. *)
val is_lua_keyword : string -> bool

(** [mangle_identifier id] converts a Lina identifier to a valid Lua identifier.

    Handles:
    - Reserved keyword prefixing
    - Stamp disambiguation for shadowed identifiers

    @param id The Lina identifier
    @return A valid Lua identifier string *)
val mangle_identifier : Common.Identifier.t -> Lua_ast.identifier
