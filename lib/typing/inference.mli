val infer_expression : Environment.t -> Parsing.Syntax_tree.expression -> Typed_tree.typed_expression
val infer_structure : Environment.t -> Parsing.Syntax_tree.structure -> Typed_tree.typed_structure * Environment.t
