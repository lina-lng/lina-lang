(** Lua code generation from Lambda IR.

    This module translates Lambda intermediate representation to Lua AST.
    The translation handles:
    - Primitive values and operations
    - Pattern matching (compiled to dispatch tables or if-chains)
    - Variant constructors with singleton optimization
    - Record creation and update
    - Let-binding with IIFE avoidance (let floating)
    - Tail call preservation

    {2 Design: Context Threading}

    Code generation uses a purely functional approach with context threading
    instead of global mutable state. A {!context} record is threaded through
    all translation functions, accumulating singleton registrations.
    This design:
    - Eliminates global mutable state for better testability
    - Makes data flow explicit and predictable
    - Enables potential future parallelization

    {2 Runtime Representation}

    | Lina Type | Lua Representation |
    |-----------|-------------------|
    | [int], [float] | number |
    | [string] | string |
    | [bool] | boolean |
    | [unit] | nil |
    | [(a, b, c)] | [{a, b, c}] (1-indexed) |
    | [{x; y}] | [{x = 1, y = 2}] |
    | [None] | [{_tag = N}] (singleton) |
    | [Some x] | [{_tag = N, _0 = x}] |

    {2 Pattern Match Compilation}

    Pattern matches use different strategies based on constructor count:
    - 3 or fewer: if-chain with [_tag] comparison
    - 4 or more: dispatch table for O(1) lookup

    The threshold is configured in {!Common.Codegen_constants}.

    {2 Singleton Optimization}

    Nullary constructors (like [None]) share a single pre-allocated table
    to reduce allocations. These are emitted as a preamble:
    {[
      local _Ctor_option_0 = {_tag = 0}
    ]}

    Singletons are tracked in the context during translation and collected
    at the end to generate the preamble.

    @see lib/lua/CLAUDE.md for detailed code generation documentation *)

(** [generate lambdas] translates Lambda IR to a Lua AST chunk.

    Takes a list of top-level Lambda expressions (typically from
    {!Lambda.translate_structure}) and produces a complete Lua chunk
    ready for printing.

    The output includes:
    1. Singleton preamble (pre-allocated nullary constructors)
    2. Translated statements from each lambda

    @param lambdas The Lambda IR expressions to translate
    @return A Lua AST chunk representing the program *)
val generate : Lambda.lambda list -> Lua_ast.chunk
