open Common

type primitive =
  | PrimitiveAddInt
  | PrimitiveSubInt
  | PrimitiveMulInt
  | PrimitiveDivInt
  | PrimitiveNegInt
  | PrimitiveIntEqual
  | PrimitiveIntNotEqual
  | PrimitiveIntLess
  | PrimitiveIntGreater
  | PrimitiveIntLessEqual
  | PrimitiveIntGreaterEqual
  | PrimitiveMakeBlock of int
  | PrimitiveGetField of int
  | PrimitivePrint

type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBool of bool
  | ConstantUnit

type constructor_tag = {
  tag_name : string;
  tag_index : int;
  tag_type_name : string;
  tag_is_nullary : bool;
}

type lambda =
  | LambdaVariable of Identifier.t
  | LambdaConstant of constant
  | LambdaApply of lambda * lambda list
  | LambdaFunction of Identifier.t list * lambda
  | LambdaLet of Identifier.t * lambda * lambda
  | LambdaLetRecursive of (Identifier.t * lambda) list * lambda
  | LambdaPrimitive of primitive * lambda list
  | LambdaIfThenElse of lambda * lambda * lambda
  | LambdaSequence of lambda * lambda
  | LambdaMakeBlock of int * lambda list
  | LambdaGetField of int * lambda
  | LambdaSwitch of lambda * switch_case list * lambda option
  | LambdaConstructor of constructor_tag * lambda option
  | LambdaMakeRecord of (string * lambda) list
  | LambdaGetRecordField of string * lambda
  | LambdaRecordUpdate of lambda * (string * lambda) list
  (* Module constructs *)
  | LambdaModule of module_binding list           (** Module as table of bindings *)
  | LambdaModuleAccess of lambda * string         (** M.x - module field access *)
  | LambdaFunctor of Identifier.t * lambda        (** functor (X : S) -> ME *)
  | LambdaFunctorApply of lambda * lambda         (** F(X) - functor application *)

and module_binding = {
  mb_name : string;
  mb_value : lambda;
}

and switch_case = {
  switch_tag : int;
  switch_body : lambda;
}

let primitive_of_operator = function
  | "+" -> Some PrimitiveAddInt
  | "-" -> Some PrimitiveSubInt
  | "*" -> Some PrimitiveMulInt
  | "/" -> Some PrimitiveDivInt
  | "==" -> Some PrimitiveIntEqual
  | "!=" -> Some PrimitiveIntNotEqual
  | "<" -> Some PrimitiveIntLess
  | ">" -> Some PrimitiveIntGreater
  | "<=" -> Some PrimitiveIntLessEqual
  | ">=" -> Some PrimitiveIntGreaterEqual
  | "print" -> Some PrimitivePrint
  | _ -> None

let translate_constant = function
  | Parsing.Syntax_tree.ConstantInteger n -> ConstantInt n
  | Parsing.Syntax_tree.ConstantFloat f -> ConstantFloat f
  | Parsing.Syntax_tree.ConstantString s -> ConstantString s
  | Parsing.Syntax_tree.ConstantBoolean b -> ConstantBool b
  | Parsing.Syntax_tree.ConstantUnit -> ConstantUnit

(* --- Decision tree translation to Lambda IR --- *)

(* Translate occurrence to Lambda access expression *)
let occurrence_to_lambda (scrutinee : lambda) (occ : Pattern_match.occurrence) : lambda =
  List.fold_left (fun expr step ->
    match step with
    | Pattern_match.OccTupleField idx -> LambdaGetField (idx, expr)
    | Pattern_match.OccConstructorArg -> LambdaGetRecordField ("_0", expr)
    | Pattern_match.OccRecordField name -> LambdaGetRecordField (name, expr)
  ) scrutinee occ

(* Generate let bindings for decision tree leaf *)
let generate_dt_bindings (scrutinee : lambda) (bindings : (Identifier.t * Pattern_match.occurrence) list) (body : lambda) : lambda =
  List.fold_right (fun (id, occ) acc ->
    let value = occurrence_to_lambda scrutinee occ in
    LambdaLet (id, value, acc)
  ) bindings body

(* Translate decision tree to Lambda IR *)
let rec translate_decision_tree (scrutinee : lambda) (tree : Pattern_match.decision_tree) (translate_expr : Typing.Typed_tree.typed_expression -> lambda) : lambda =
  match tree with
  | Pattern_match.DTFail ->
    LambdaApply (
      LambdaVariable (Identifier.create "error"),
      [LambdaConstant (ConstantString "Match failure")]
    )

  | Pattern_match.DTLeaf { leaf_bindings; leaf_action } ->
    let action_lambda = translate_expr leaf_action in
    generate_dt_bindings scrutinee leaf_bindings action_lambda

  | Pattern_match.DTGuard { guard_bindings; guard_condition; guard_then; guard_else } ->
    let guard_lambda = translate_expr guard_condition in
    let guard_with_bindings = generate_dt_bindings scrutinee guard_bindings guard_lambda in
    let then_branch = translate_decision_tree scrutinee guard_then translate_expr in
    let else_branch = translate_decision_tree scrutinee guard_else translate_expr in
    LambdaIfThenElse (guard_with_bindings, then_branch, else_branch)

  | Pattern_match.DTSwitch { switch_occurrence; switch_cases; switch_default } ->
    translate_dt_switch scrutinee switch_occurrence switch_cases switch_default translate_expr

and translate_dt_switch scrutinee occ cases default translate_expr =
  let target = occurrence_to_lambda scrutinee occ in

  (* Separate constructor cases from constant cases *)
  let const_cases, ctor_cases, other_cases =
    List.fold_left (fun (consts, ctors, others) (head, tree) ->
      match head with
      | Pattern_match.HCConstant c -> ((c, tree) :: consts, ctors, others)
      | Pattern_match.HCConstructor (name, tag_index) -> (consts, ((name, tag_index), tree) :: ctors, others)
      | _ -> (consts, ctors, (head, tree) :: others)
    ) ([], [], []) cases
  in

  (* Generate code based on case types *)
  if ctor_cases <> [] && const_cases = [] then
    translate_dt_constructor_switch scrutinee target ctor_cases default translate_expr
  else if const_cases <> [] then
    translate_dt_constant_switch scrutinee target const_cases default translate_expr
  else
    match other_cases, default with
    | [(_, tree)], _ -> translate_decision_tree scrutinee tree translate_expr
    | [], Some d -> translate_decision_tree scrutinee d translate_expr
    | [], None -> translate_decision_tree scrutinee Pattern_match.DTFail translate_expr
    | _ -> translate_decision_tree scrutinee Pattern_match.DTFail translate_expr

and translate_dt_constructor_switch scrutinee target cases default translate_expr =
  let num_cases = List.length cases in
  (* Use LambdaSwitch for 4+ cases (enables dispatch table in codegen) *)
  if num_cases >= 4 then
    let switch_cases = List.map (fun ((_name, tag_index), tree) ->
      { switch_tag = tag_index;
        switch_body = translate_decision_tree scrutinee tree translate_expr }
    ) cases in
    let switch_default = match default with
      | Some d -> Some (translate_decision_tree scrutinee d translate_expr)
      | None -> None
    in
    LambdaSwitch (target, switch_cases, switch_default)
  else
    (* Use if-chain for fewer cases *)
    let tag_access = LambdaGetRecordField ("_tag", target) in
    let rec build_chain = function
      | [] ->
        begin match default with
        | Some d -> translate_decision_tree scrutinee d translate_expr
        | None -> translate_decision_tree scrutinee Pattern_match.DTFail translate_expr
        end
      | ((_name, tag_index), tree) :: rest ->
        let test = LambdaPrimitive (PrimitiveIntEqual, [tag_access; LambdaConstant (ConstantInt tag_index)]) in
        let then_branch = translate_decision_tree scrutinee tree translate_expr in
        let else_branch = build_chain rest in
        LambdaIfThenElse (test, then_branch, else_branch)
    in
    build_chain cases

and translate_dt_constant_switch scrutinee target cases default translate_expr =
  let rec build_chain = function
    | [] ->
      begin match default with
      | Some d -> translate_decision_tree scrutinee d translate_expr
      | None -> translate_decision_tree scrutinee Pattern_match.DTFail translate_expr
      end
    | (const, tree) :: rest ->
      let lambda_const = translate_constant const in
      let test = LambdaPrimitive (PrimitiveIntEqual, [target; LambdaConstant lambda_const]) in
      let then_branch = translate_decision_tree scrutinee tree translate_expr in
      let else_branch = build_chain rest in
      LambdaIfThenElse (test, then_branch, else_branch)
  in
  build_chain cases

(* --- Pattern binding helpers (for non-match patterns) --- *)

let rec translate_pattern_binding pattern value body =
  let open Typing.Typed_tree in
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
    LambdaLet (id, value, body)

  | TypedPatternWildcard ->
    LambdaLet (Identifier.create "_", value, body)

  | TypedPatternConstant _ ->
    body

  | TypedPatternTuple patterns ->
    let temp_id = Identifier.create "_tuple" in
    let bindings =
      List.mapi (fun i p ->
        let field_access = LambdaGetField (i, LambdaVariable temp_id) in
        translate_pattern_binding p field_access
      ) patterns
    in
    let inner = List.fold_right (fun f acc -> f acc) bindings body in
    LambdaLet (temp_id, value, inner)

  | TypedPatternConstructor (_, arg_pattern) ->
    begin match arg_pattern with
    | None -> body
    | Some inner_pattern ->
      let temp_id = Identifier.create "_ctor" in
      let arg_access = LambdaGetRecordField ("_0", LambdaVariable temp_id) in
      let inner = translate_pattern_binding inner_pattern arg_access body in
      LambdaLet (temp_id, value, inner)
    end

  | TypedPatternRecord (field_patterns, _is_open) ->
    let temp_id = Identifier.create "_record" in
    let bindings =
      List.map (fun (field_pattern : typed_record_pattern_field) ->
        let field_access = LambdaGetRecordField (field_pattern.typed_pattern_field_name, LambdaVariable temp_id) in
        translate_pattern_binding field_pattern.typed_pattern_field_pattern field_access
      ) field_patterns
    in
    let inner = List.fold_right (fun binding_function accumulator -> binding_function accumulator) bindings body in
    LambdaLet (temp_id, value, inner)

(** Translate a module path to a lambda expression *)
let rec translate_module_path (path : Typing.Types.path) : lambda =
  match path with
  | Typing.Types.PathBuiltin _ ->
    (* Builtin types don't have runtime representation *)
    failwith "Cannot translate builtin path to lambda"
  | Typing.Types.PathLocal name ->
    (* Local type reference - shouldn't be used for module access *)
    LambdaVariable (Identifier.create name)
  | Typing.Types.PathIdent id ->
    (* Runtime module reference with tracked identity *)
    LambdaVariable id
  | Typing.Types.PathDot (parent, name) ->
    (* Nested module access: M.N *)
    let parent_expr = translate_module_path parent in
    LambdaModuleAccess (parent_expr, name)
  | Typing.Types.PathApply (func_path, arg_path) ->
    (* Functor application path: F(X) *)
    let func_expr = translate_module_path func_path in
    let arg_expr = translate_module_path arg_path in
    LambdaFunctorApply (func_expr, arg_expr)

(** Check if a structure needs complex translation (open, include, recursive bindings, or inter-function references) *)
and structure_needs_complex_translation (structure : Typing.Typed_tree.typed_structure) : bool =
  let open Typing.Typed_tree in
  (* Collect all defined names and check for functions that might reference them *)
  let defined_names = List.concat_map (fun item ->
    match item.structure_item_desc with
    | TypedStructureValue (_, bindings) ->
      List.filter_map (fun (b : typed_binding) ->
        match b.binding_pattern.pattern_desc with
        | TypedPatternVariable id -> Some (Identifier.name id)
        | _ -> None
      ) bindings
    | _ -> []
  ) structure in
  let has_multiple_functions =
    let function_count = List.fold_left (fun count item ->
      match item.structure_item_desc with
      | TypedStructureValue (_, bindings) ->
        count + List.length (List.filter (fun (b : typed_binding) ->
          match b.binding_expression.expression_desc with
          | TypedExpressionFunction _ -> true
          | _ -> false
        ) bindings)
      | _ -> count
    ) 0 structure in
    function_count > 1 && List.length defined_names > 1
  in
  has_multiple_functions ||
  List.exists (fun item ->
    match item.structure_item_desc with
    | TypedStructureOpen _ | TypedStructureInclude _ -> true
    | TypedStructureValue (Parsing.Syntax_tree.Recursive, _) -> true  (* Recursive bindings need special handling *)
    | _ -> false
  ) structure

(** Translate a structure with opens/includes by wrapping in an IIFE *)
and translate_structure_with_opens (structure : Typing.Typed_tree.typed_structure) : lambda =
  let open Typing.Typed_tree in
  (* Process items in order, collecting local bindings and final module fields *)
  let rec process items local_bindings module_fields =
    match items with
    | [] ->
      (* Build the module table from collected fields *)
      let module_table = LambdaModule (List.rev module_fields) in
      (* Wrap with local bindings - handle both single and recursive bindings *)
      List.fold_left (fun body binding ->
        match binding with
        | `Single (id, value) -> LambdaLet (id, value, body)
        | `Recursive bindings -> LambdaLetRecursive (bindings, body)
      ) module_table local_bindings
    | item :: rest ->
      match item.structure_item_desc with
      | TypedStructureValue (rec_flag, bindings) ->
        (* For values, we need to define them as locals first *)
        let value_bindings = List.filter_map (fun (binding : typed_binding) ->
          match binding.binding_pattern.pattern_desc with
          | TypedPatternVariable id ->
            let value = translate_expression binding.binding_expression in
            Some (id, value)
          | _ -> None
        ) bindings in
        let field_refs = List.filter_map (fun (binding : typed_binding) ->
          match binding.binding_pattern.pattern_desc with
          | TypedPatternVariable id ->
            Some { mb_name = Identifier.name id; mb_value = LambdaVariable id }
          | _ -> None
        ) bindings in
        (* For recursive bindings, we need to wrap them with LambdaLetRecursive *)
        let new_local_bindings = match rec_flag with
          | Parsing.Syntax_tree.Recursive ->
            (* Mark this group as recursive so we can emit LambdaLetRecursive later *)
            (`Recursive value_bindings) :: local_bindings
          | Parsing.Syntax_tree.Nonrecursive ->
            (* Non-recursive bindings are added individually *)
            List.fold_right (fun (id, v) acc -> (`Single (id, v)) :: acc) value_bindings local_bindings
        in
        process rest new_local_bindings (List.rev_append field_refs module_fields)
      | TypedStructureType _ ->
        process rest local_bindings module_fields
      | TypedStructureModuleType _ ->
        process rest local_bindings module_fields  (* Module types have no runtime representation *)
      | TypedStructureModule (name_id, inner_mexpr) ->
        let value = translate_module_expression inner_mexpr in
        let new_local = `Single (name_id, value) in
        let new_field = { mb_name = Identifier.name name_id; mb_value = LambdaVariable name_id } in
        process rest (new_local :: local_bindings) (new_field :: module_fields)
      | TypedStructureOpen (module_path, opened_bindings) ->
        let module_expr = translate_module_path module_path in
        let new_locals = List.map (fun (name, id) ->
          `Single (id, LambdaModuleAccess (module_expr, name))
        ) opened_bindings in
        let new_fields = List.map (fun (_name, id) ->
          { mb_name = Identifier.name id; mb_value = LambdaVariable id }
        ) opened_bindings in
        process rest (List.rev_append new_locals local_bindings) (List.rev_append new_fields module_fields)
      | TypedStructureInclude (mexpr, included_bindings) ->
        let translated_mexpr = translate_module_expression mexpr in
        (* Create a temp for the included module *)
        let temp_id = Identifier.create "_included" in
        let temp_binding = `Single (temp_id, translated_mexpr) in
        (* Create bindings that access the temp *)
        let new_locals = List.map (fun (name, id) ->
          `Single (id, LambdaModuleAccess (LambdaVariable temp_id, name))
        ) included_bindings in
        let new_fields = List.map (fun (_name, id) ->
          { mb_name = Identifier.name id; mb_value = LambdaVariable id }
        ) included_bindings in
        process rest (List.rev_append new_locals (temp_binding :: local_bindings)) (List.rev_append new_fields module_fields)
  in
  process structure [] []

(** Translate a typed module expression to lambda *)
and translate_module_expression (mexpr : Typing.Typed_tree.typed_module_expression) : lambda =
  let open Typing.Typed_tree in
  match mexpr.module_desc with
  | TypedModuleStructure structure ->
    if structure_needs_complex_translation structure then
      (* Use IIFE-style translation for structures with open/include *)
      translate_structure_with_opens structure
    else
      (* Simple case: just create a table *)
      let bindings = collect_module_bindings structure in
      LambdaModule bindings
  | TypedModulePath path ->
    translate_module_path path
  | TypedModuleFunctor (param, body) ->
    (* Functor becomes a function taking a module and returning a module *)
    let translated_body = translate_module_expression body in
    LambdaFunctor (param.Typing.Module_types.param_id, translated_body)
  | TypedModuleApply (func_mexpr, arg_mexpr) ->
    (* Functor application becomes function application *)
    let translated_func = translate_module_expression func_mexpr in
    let translated_arg = translate_module_expression arg_mexpr in
    LambdaFunctorApply (translated_func, translated_arg)
  | TypedModuleConstraint (inner, _) ->
    translate_module_expression inner

(** Collect bindings from a structure for module table *)
and collect_module_bindings (structure : Typing.Typed_tree.typed_structure) : module_binding list =
  let open Typing.Typed_tree in
  List.concat_map (fun item ->
    match item.structure_item_desc with
    | TypedStructureValue (_rec_flag, bindings) ->
      List.filter_map (fun (binding : typed_binding) ->
        match binding.binding_pattern.pattern_desc with
        | TypedPatternVariable id ->
          let name = Identifier.name id in
          let value = translate_expression binding.binding_expression in
          Some { mb_name = name; mb_value = value }
        | _ -> None
      ) bindings
    | TypedStructureType _ ->
      []  (* Types don't contribute runtime values *)
    | TypedStructureModuleType _ ->
      []  (* Module types don't contribute runtime values *)
    | TypedStructureModule (name_id, inner_mexpr) ->
      let value = translate_module_expression inner_mexpr in
      [{ mb_name = Identifier.name name_id; mb_value = value }]
    | TypedStructureOpen (module_path, opened_bindings) ->
      (* Generate local bindings for opened values: local add = Math.add *)
      let module_expr = translate_module_path module_path in
      List.map (fun (name, id) ->
        { mb_name = Identifier.name id; mb_value = LambdaModuleAccess (module_expr, name) }
      ) opened_bindings
    | TypedStructureInclude (mexpr, included_bindings) ->
      (* For include inside a module, we inline the module's values directly *)
      (* Each included value becomes a field that accesses the included module *)
      let translated_mexpr = translate_module_expression mexpr in
      List.map (fun (name, id) ->
        { mb_name = Identifier.name id; mb_value = LambdaModuleAccess (translated_mexpr, name) }
      ) included_bindings
  ) structure

and translate_expression (expr : Typing.Typed_tree.typed_expression) : lambda =
  let open Typing.Typed_tree in
  match expr.expression_desc with
  | TypedExpressionVariable id ->
    LambdaVariable id

  | TypedExpressionConstant const ->
    LambdaConstant (translate_constant const)

  | TypedExpressionTuple exprs ->
    let translated = List.map translate_expression exprs in
    LambdaMakeBlock (0, translated)

  | TypedExpressionConstructor (ctor_info, arg_expr) ->
    let translated_arg = Option.map translate_expression arg_expr in
    let tag = {
      tag_name = ctor_info.Typing.Types.constructor_name;
      tag_index = ctor_info.Typing.Types.constructor_tag_index;
      tag_type_name = ctor_info.Typing.Types.constructor_type_name;
      tag_is_nullary = Option.is_none ctor_info.Typing.Types.constructor_argument_type;
    } in
    LambdaConstructor (tag, translated_arg)

  | TypedExpressionApply (func_expr, arg_exprs) ->
    let func = translate_expression func_expr in
    let args = List.map translate_expression arg_exprs in
    begin match func with
    | LambdaVariable id ->
      let name = Identifier.name id in
      begin match primitive_of_operator name with
      | Some prim -> LambdaPrimitive (prim, args)
      | None -> LambdaApply (func, args)
      end
    | _ -> LambdaApply (func, args)
    end

  | TypedExpressionFunction (param_patterns, body_expr) ->
    let param_ids =
      List.map (fun (p : typed_pattern) ->
        match p.pattern_desc with
        | TypedPatternVariable id -> id
        | _ -> Identifier.create "_param"
      ) param_patterns
    in
    let translated_body = translate_expression body_expr in
    let body_with_bindings =
      List.fold_right2 (fun pattern param_id body ->
        match pattern.pattern_desc with
        | TypedPatternVariable _ -> body
        | _ -> translate_pattern_binding pattern (LambdaVariable param_id) body
      ) param_patterns param_ids translated_body
    in
    LambdaFunction (param_ids, body_with_bindings)

  | TypedExpressionLet (rec_flag, bindings, body_expr) ->
    let translated_body = translate_expression body_expr in
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      List.fold_right (fun (binding : typed_binding) body ->
        let translated_expr = translate_expression binding.binding_expression in
        translate_pattern_binding binding.binding_pattern translated_expr body
      ) bindings translated_body

    | Parsing.Syntax_tree.Recursive ->
      let rec_bindings =
        List.map (fun (binding : typed_binding) ->
          let id = match binding.binding_pattern.pattern_desc with
            | TypedPatternVariable id -> id
            | _ -> Identifier.create "_rec"
          in
          let translated_expr = translate_expression binding.binding_expression in
          (id, translated_expr)
        ) bindings
      in
      LambdaLetRecursive (rec_bindings, translated_body)
    end

  | TypedExpressionIf (cond_expr, then_expr, else_expr_opt) ->
    let translated_cond = translate_expression cond_expr in
    let translated_then = translate_expression then_expr in
    let translated_else = match else_expr_opt with
      | Some else_expr -> translate_expression else_expr
      | None -> LambdaConstant ConstantUnit
    in
    LambdaIfThenElse (translated_cond, translated_then, translated_else)

  | TypedExpressionSequence (first_expr, second_expr) ->
    let translated_first = translate_expression first_expr in
    let translated_second = translate_expression second_expr in
    LambdaSequence (translated_first, translated_second)

  | TypedExpressionRecord record_fields ->
    let translated_fields = List.map (fun (field : typed_record_field) ->
      (field.typed_field_name, translate_expression field.typed_field_value)
    ) record_fields in
    LambdaMakeRecord translated_fields

  | TypedExpressionRecordAccess (record_expression, field_name) ->
    let translated_record = translate_expression record_expression in
    LambdaGetRecordField (field_name, translated_record)

  | TypedExpressionRecordUpdate (base_expression, update_fields) ->
    let translated_base = translate_expression base_expression in
    let translated_update_fields = List.map (fun (field : typed_record_field) ->
      (field.typed_field_name, translate_expression field.typed_field_value)
    ) update_fields in
    LambdaRecordUpdate (translated_base, translated_update_fields)

  | TypedExpressionMatch (scrutinee_expression, match_arms) ->
    let translated_scrutinee = translate_expression scrutinee_expression in
    let scrutinee_id = Identifier.create "_scrutinee" in
    (* Use the pattern match compiler with Maranget's decision tree algorithm *)
    let decision_tree = Pattern_match.compile_match match_arms in
    let match_body = translate_decision_tree
      (LambdaVariable scrutinee_id)
      decision_tree
      translate_expression
    in
    LambdaLet (scrutinee_id, translated_scrutinee, match_body)

  | TypedExpressionModuleAccess (path, name) ->
    (* Translate module path to lambda, then access the field *)
    let module_expr = translate_module_path path in
    LambdaModuleAccess (module_expr, name)

let translate_structure_item (item : Typing.Typed_tree.typed_structure_item) : lambda list =
  let open Typing.Typed_tree in
  match item.structure_item_desc with
  | TypedStructureValue (rec_flag, bindings) ->
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      List.map (fun (binding : typed_binding) ->
        let id = match binding.binding_pattern.pattern_desc with
          | TypedPatternVariable id -> id
          | _ -> Identifier.create "_top"
        in
        let translated_expr = translate_expression binding.binding_expression in
        LambdaLet (id, translated_expr, LambdaConstant ConstantUnit)
      ) bindings

    | Parsing.Syntax_tree.Recursive ->
      let rec_bindings =
        List.map (fun (binding : typed_binding) ->
          let id = match binding.binding_pattern.pattern_desc with
            | TypedPatternVariable id -> id
            | _ -> Identifier.create "_rec"
          in
          let translated_expr = translate_expression binding.binding_expression in
          (id, translated_expr)
        ) bindings
      in
      [LambdaLetRecursive (rec_bindings, LambdaConstant ConstantUnit)]
    end

  | TypedStructureType _ ->
    []

  | TypedStructureModuleType _ ->
    []  (* Module types have no runtime representation *)

  | TypedStructureModule (module_id, mexpr) ->
    (* Translate module definition as a let binding *)
    let translated_module = translate_module_expression mexpr in
    [LambdaLet (module_id, translated_module, LambdaConstant ConstantUnit)]

  | TypedStructureOpen (module_path, opened_bindings) ->
    (* Generate local bindings for opened values: local add = Math.add *)
    let module_expr = translate_module_path module_path in
    List.map (fun (name, id) ->
      LambdaLet (id, LambdaModuleAccess (module_expr, name), LambdaConstant ConstantUnit)
    ) opened_bindings

  | TypedStructureInclude (mexpr, included_bindings) ->
    (* Generate local bindings for included values *)
    let module_temp = Identifier.create "_include" in
    let module_binding = LambdaLet (module_temp, translate_module_expression mexpr, LambdaConstant ConstantUnit) in
    let value_bindings = List.map (fun (name, id) ->
      LambdaLet (id, LambdaModuleAccess (LambdaVariable module_temp, name), LambdaConstant ConstantUnit)
    ) included_bindings in
    module_binding :: value_bindings

let translate_structure structure =
  List.concat_map translate_structure_item structure
