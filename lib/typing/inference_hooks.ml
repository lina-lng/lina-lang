(** Forward references for mutual recursion across inference modules. *)

(** {1 Expression Inference Hook} *)

type expression_hook =
  Environment.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression

let infer_expression_ref : expression_hook ref =
  ref (fun _ _ -> failwith "Inference_hooks.infer_expression not initialized")

let infer_expression env expr = !infer_expression_ref env expr

let set_infer_expression hook = infer_expression_ref := hook

(** {1 Pattern Inference Hook} *)

type pattern_hook =
  Environment.t ->
  Parsing.Syntax_tree.pattern ->
  Typed_tree.typed_pattern * Types.type_expression * Environment.t

let infer_pattern_ref : pattern_hook ref =
  ref (fun _ _ -> failwith "Inference_hooks.infer_pattern not initialized")

let infer_pattern env pattern = !infer_pattern_ref env pattern

let set_infer_pattern hook = infer_pattern_ref := hook

(** {1 Structure Inference Hook} *)

type structure_hook =
  Environment.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure * Environment.t

let infer_structure_ref : structure_hook ref =
  ref (fun _ _ -> failwith "Inference_hooks.infer_structure not initialized")

let infer_structure env structure = !infer_structure_ref env structure

let set_infer_structure hook = infer_structure_ref := hook

(** {1 Module Expression Inference Hook} *)

type module_expr_hook =
  Environment.t ->
  Parsing.Syntax_tree.module_expression ->
  Typed_tree.typed_module_expression * Environment.t

let infer_module_expression_ref : module_expr_hook ref =
  ref (fun _ _ -> failwith "Inference_hooks.infer_module_expression not initialized")

let infer_module_expression env mexpr = !infer_module_expression_ref env mexpr

let set_infer_module_expression hook = infer_module_expression_ref := hook

(** {1 Module Type Checking Hook} *)

type module_type_hook =
  Environment.t ->
  Parsing.Syntax_tree.module_type ->
  Module_types.module_type

let check_module_type_ref : module_type_hook ref =
  ref (fun _ _ -> failwith "Inference_hooks.check_module_type not initialized")

let check_module_type env mty = !check_module_type_ref env mty

let set_check_module_type hook = check_module_type_ref := hook
