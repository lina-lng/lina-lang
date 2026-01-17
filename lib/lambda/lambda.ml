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
  | PrimitiveStringEqual
  | PrimitiveStringConcat
  | PrimitiveBoolNot
  | PrimitiveMakeBlock of int
  | PrimitiveGetField of int
  | PrimitivePrint
  | PrimError

type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantBool of bool
  | ConstantUnit

type constructor_tag_index = {
  tag_name : string;
  tag_index : int;
  tag_type_name : string;
  tag_is_nullary : bool;
  tag_is_extension : bool;  (** True for extensible variant constructors - use string tag *)
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
  | LambdaConstructor of constructor_tag_index * lambda option
  | LambdaMakeRecord of (string * lambda) list
  | LambdaGetRecordField of string * lambda
  | LambdaRecordUpdate of lambda * (string * lambda) list
  (* Module constructs *)
  | LambdaModule of module_binding list           (** Module as table of bindings *)
  | LambdaModuleAccess of lambda * string         (** M.x - module field access *)
  | LambdaFunctor of Identifier.t * lambda        (** functor (X : S) -> ME *)
  | LambdaFunctorApply of lambda * lambda         (** F(X) - functor application *)
  (* FFI constructs *)
  | LambdaExternalCall of Typing_ffi.Types.ffi_spec * lambda list  (** FFI external call with arguments *)
  (* Reference constructs *)
  | LambdaRef of lambda                           (** ref e - create mutable cell *)
  | LambdaDeref of lambda                         (** !e - read mutable cell *)
  | LambdaAssign of lambda * lambda               (** e1 := e2 - write to mutable cell *)
  (* Polymorphic variant *)
  | LambdaPolyVariant of string * lambda option   (** `Tag or `Tag e - poly variant constructor *)
  (* Loops *)
  | LambdaWhile of lambda * lambda                (** while cond do body done *)
  | LambdaFor of Identifier.t * lambda * lambda * Parsing.Syntax_tree.direction_flag * lambda
      (** for i = start to/downto end do body done *)

and module_binding = {
  mb_id : Identifier.t;  (** Original identifier for internal references *)
  mb_value : lambda;     (** Bound value *)
}

and switch_case = {
  switch_tag : int;
  switch_body : lambda;
}

(** Get the binding name for a module binding.
    This is the name used for the module field in generated code. *)
let module_binding_name (binding : module_binding) : string =
  Identifier.name binding.mb_id

let primitive_of_operator = function
  | "+" -> Some PrimitiveAddInt
  | "-" -> Some PrimitiveSubInt
  | "*" -> Some PrimitiveMulInt
  | "/" -> Some PrimitiveDivInt
  | "=" -> Some PrimitiveIntEqual
  | "==" -> Some PrimitiveIntEqual
  | "!=" -> Some PrimitiveIntNotEqual
  | "<>" -> Some PrimitiveIntNotEqual
  | "<" -> Some PrimitiveIntLess
  | ">" -> Some PrimitiveIntGreater
  | "<=" -> Some PrimitiveIntLessEqual
  | ">=" -> Some PrimitiveIntGreaterEqual
  | "^" -> Some PrimitiveStringConcat
  | "print" -> Some PrimitivePrint
  | "not" -> Some PrimitiveBoolNot
  | _ -> None

let translate_constant = function
  | Parsing.Syntax_tree.ConstantInteger n -> ConstantInt n
  | Parsing.Syntax_tree.ConstantFloat f -> ConstantFloat f
  | Parsing.Syntax_tree.ConstantString s -> ConstantString s
  | Parsing.Syntax_tree.ConstantBoolean b -> ConstantBool b
  | Parsing.Syntax_tree.ConstantUnit -> ConstantUnit

let make_ffi_wrapper (spec : Typing_ffi.Types.ffi_spec) : lambda =
  let arity = spec.ffi_arity in

  if arity = 0 then
    LambdaExternalCall (spec, [])
  else
    let arg_ids = List.init arity (fun index ->
      Identifier.create (Printf.sprintf "arg%d" index))
    in
    let arg_vars = List.map (fun id -> LambdaVariable id) arg_ids in
    LambdaFunction (arg_ids, LambdaExternalCall (spec, arg_vars))

let occurrence_to_lambda (scrutinee : lambda) (occ : Pattern_match.occurrence) : lambda =
  List.fold_left (fun expr step ->
    match step with
    | Pattern_match.OccTupleField idx -> LambdaGetField (idx, expr)
    | Pattern_match.OccConstructorArg -> LambdaGetRecordField ("_0", expr)
    | Pattern_match.OccRecordField name -> LambdaGetRecordField (name, expr)
  ) scrutinee occ

let generate_dt_bindings scrutinee bindings body =
  List.fold_right (fun (id, occ) acc ->
    let bound_expr = occurrence_to_lambda scrutinee occ in
    LambdaLet (id, bound_expr, acc)
  ) bindings body

(** Parameterized switch config to reduce duplication between constructor,
    constant, and polymorphic variant switches. *)
type 'key switch_config = {
  get_discriminant : lambda -> lambda;
  key_to_constant : 'key -> constant;
  equality_primitive : primitive;
}

let build_switch_if_chain (type key) ~(config : key switch_config) ~translate_tree target cases default =
  let discriminant = config.get_discriminant target in
  let make_test key =
    LambdaPrimitive (config.equality_primitive,
      [discriminant; LambdaConstant (config.key_to_constant key)])
  in
  let rec build = function
    | [] ->
      begin match default with
      | Some default_tree -> translate_tree default_tree
      | None -> translate_tree Pattern_match.DTFail
      end
    | (key, tree) :: rest ->
      let test = make_test key in
      let then_branch = translate_tree tree in
      let else_branch = build rest in
      LambdaIfThenElse (test, then_branch, else_branch)
  in
  build cases

let variant_tag_discriminant target =
  LambdaGetRecordField (Codegen_constants.variant_tag_field, target)

let constructor_switch_config : (string * int * bool) switch_config = {
  get_discriminant = variant_tag_discriminant;
  key_to_constant = (fun (_name, tag_index, _is_ext) -> ConstantInt tag_index);
  equality_primitive = PrimitiveIntEqual;
}

let extension_constructor_switch_config : (string * int * bool) switch_config = {
  get_discriminant = variant_tag_discriminant;
  key_to_constant = (fun (name, _tag_index, _is_ext) -> ConstantString name);
  equality_primitive = PrimitiveStringEqual;
}

let constant_switch_config : Parsing.Syntax_tree.constant switch_config = {
  get_discriminant = Fun.id;
  key_to_constant = translate_constant;
  equality_primitive = PrimitiveIntEqual;
}

let poly_variant_switch_config : string switch_config = {
  get_discriminant = variant_tag_discriminant;
  key_to_constant = (fun tag -> ConstantString tag);
  equality_primitive = PrimitiveStringEqual;
}

let rec translate_decision_tree scrutinee tree translate_expr =
  match tree with
  | Pattern_match.DTFail ->
    (* Use stamp 0 so the mangler generates just "error" without suffix *)
    LambdaApply (
      LambdaVariable (Identifier.create_with_stamp "error" 0),
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
  let translate_tree tree = translate_decision_tree scrutinee tree translate_expr in

  let simple_if_chain config typed_cases =
    build_switch_if_chain ~config ~translate_tree target typed_cases default
  in

  let constant_switch_cases, constructor_switch_cases, poly_variant_switch_cases, fallback_cases =
    List.fold_left (fun (constants, constructors, poly_variants, fallbacks) (head, tree) ->
      match head with
      | Pattern_match.HCConstant c ->
          ((c, tree) :: constants, constructors, poly_variants, fallbacks)
      | Pattern_match.HCConstructor (name, tag_index, is_extension) ->
          (constants, ((name, tag_index, is_extension), tree) :: constructors, poly_variants, fallbacks)
      | Pattern_match.HCPolyVariant tag ->
          (constants, constructors, (tag, tree) :: poly_variants, fallbacks)
      | _ ->
          (constants, constructors, poly_variants, (head, tree) :: fallbacks)
    ) ([], [], [], []) cases
  in

  let has_only_constructors =
    constructor_switch_cases <> [] &&
    constant_switch_cases = [] &&
    poly_variant_switch_cases = []
  in
  let has_only_poly_variants =
    poly_variant_switch_cases <> [] &&
    constant_switch_cases = [] &&
    constructor_switch_cases = []
  in
  let has_constants = constant_switch_cases <> [] in

  if has_only_constructors then
    translate_dt_constructor_switch target constructor_switch_cases default translate_tree

  else if has_only_poly_variants then
    simple_if_chain poly_variant_switch_config poly_variant_switch_cases

  else if has_constants then
    simple_if_chain constant_switch_config constant_switch_cases

  else
    match fallback_cases, default with
    | [(_, tree)], _ -> translate_tree tree
    | [], Some default_tree -> translate_tree default_tree
    | _, _ -> translate_tree Pattern_match.DTFail

and translate_dt_constructor_switch target cases default translate_tree =
  let num_cases = List.length cases in

  (* Check if any case is an extension constructor *)
  let has_extension = List.exists (fun ((_name, _tag_index, is_ext), _tree) -> is_ext) cases in

  if has_extension then
    (* Extension constructors use string tags - always use if-chain *)
    build_switch_if_chain ~config:extension_constructor_switch_config ~translate_tree target cases default

  else begin
    (* Regular constructors use numeric tags *)
    (* Use LambdaSwitch for many cases (enables dispatch table in codegen) *)
    if num_cases >= Codegen_constants.dispatch_table_threshold then
      let switch_cases = List.map (fun ((_name, tag_index, _is_ext), tree) ->
        { switch_tag = tag_index; switch_body = translate_tree tree }
      ) cases in
      let switch_default = Option.map translate_tree default in
      LambdaSwitch (target, switch_cases, switch_default)

    else
      build_switch_if_chain ~config:constructor_switch_config ~translate_tree target cases default
  end

(** Bind compound pattern components via a temp variable.
    Creates let-bindings for each sub-pattern using the given field accessors. *)
let translate_compound_pattern temp_prefix build_accessors value body =
  let temp_id = Identifier.create temp_prefix in
  let binding_builders = build_accessors (LambdaVariable temp_id) in
  let nested_body = List.fold_right (fun builder acc -> builder acc) binding_builders body in

  LambdaLet (temp_id, value, nested_body)

let rec translate_pattern_binding pattern value body =
  let open Typing.Typed_tree in
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
      LambdaLet (id, value, body)

  | TypedPatternWildcard ->
      LambdaLet (Identifier.create "_", value, body)

  | TypedPatternConstant _ -> body

  | TypedPatternTuple patterns ->
      translate_compound_pattern Codegen_constants.tuple_temp_prefix
        (fun temp_var ->
          List.mapi (fun index pat ->
            let field_access = LambdaGetField (index, temp_var) in
            translate_pattern_binding pat field_access
          ) patterns)
        value body

  | TypedPatternConstructor (_, arg_pattern) | TypedPatternPolyVariant (_, arg_pattern) ->
      begin match arg_pattern with
      | None -> body
      | Some inner_pattern ->
          let temp_id = Identifier.create Codegen_constants.constructor_temp_prefix in
          let arg_access = LambdaGetRecordField ("_0", LambdaVariable temp_id) in
          let nested_body = translate_pattern_binding inner_pattern arg_access body in

          LambdaLet (temp_id, value, nested_body)
      end

  | TypedPatternRecord (field_patterns, _is_open) ->
      translate_compound_pattern Codegen_constants.record_temp_prefix
        (fun temp_var ->
          List.map (fun (field_pattern : typed_record_pattern_field) ->
            let field_access = LambdaGetRecordField (field_pattern.typed_pattern_field_name, temp_var) in
            translate_pattern_binding field_pattern.typed_pattern_field_pattern field_access
          ) field_patterns)
        value body

  | TypedPatternLocallyAbstract _ ->
      body

  | TypedPatternAlias (inner, id) ->
      (* Alias pattern: bind value to alias, then handle inner pattern *)
      let inner_body = translate_pattern_binding inner value body in
      LambdaLet (id, value, inner_body)

  | TypedPatternOr (left, _right) ->
      (* Or-pattern: both branches bind same variables, translate from left *)
      translate_pattern_binding left value body

  | TypedPatternError _ ->
      body

type structure_translation_result =
  | SimpleModule of module_binding list
  | NeedsComplexTranslation

type analysis_state = {
  needs_complex : bool;
  defined_count : int;
  function_count : int;
  bindings : module_binding list;
}

let translate_opened_bindings module_lambda opened_bindings =
  let local_bindings = List.map (fun (name, id) ->
    `Single (id, LambdaModuleAccess (module_lambda, name))
  ) opened_bindings in

  let module_fields = List.map (fun (_name, id) ->
    { mb_id = id; mb_value = LambdaVariable id }
  ) opened_bindings in

  (local_bindings, module_fields)

let extract_binding_id ~fallback_prefix (binding : Typing.Typed_tree.typed_binding) =
  let open Typing.Typed_tree in
  match binding.binding_pattern.pattern_desc with
  | TypedPatternVariable id -> id
  | _ -> Identifier.create fallback_prefix

let rec translate_module_path (path : Typing.Types.path) : lambda =
  match path with
  | Typing.Types.PathBuiltin _ ->
    failwith "Cannot translate builtin path to lambda"
  | Typing.Types.PathLocal name ->
    LambdaVariable (Identifier.create name)
  | Typing.Types.PathIdent id ->
    LambdaVariable id
  | Typing.Types.PathDot (parent, name) ->
    LambdaModuleAccess (translate_module_path parent, name)
  | Typing.Types.PathApply (func_path, arg_path) ->
    LambdaFunctorApply (translate_module_path func_path, translate_module_path arg_path)

and analyze_and_collect_structure structure =
  let open Typing.Typed_tree in

  let analyze_binding state (binding : typed_binding) =
    match binding.binding_pattern.pattern_desc with
    | TypedPatternVariable id ->
        let is_function = match binding.binding_expression.expression_desc with
          | TypedExpressionFunction _ -> true
          | _ -> false
        in
        let new_state = {
          state with
          defined_count = state.defined_count + 1;
          function_count = state.function_count + (if is_function then 1 else 0);
        } in

        if state.needs_complex then new_state
        else
          let value = translate_expression binding.binding_expression in
          { new_state with bindings = { mb_id = id; mb_value = value } :: state.bindings }

    | _ -> state
  in

  let analyze_item state item =
    match item.structure_item_desc with
    | TypedStructureOpen _ | TypedStructureInclude _ ->
        { state with needs_complex = true }

    | TypedStructureValue (rec_flag, value_bindings) ->
        let state =
          if rec_flag = Parsing.Syntax_tree.Recursive then
            { state with needs_complex = true }
          else state
        in
        List.fold_left analyze_binding state value_bindings

    | TypedStructureType _ | TypedStructureModuleType _ ->
        state

    | TypedStructureModule (name_id, inner_mexpr) ->
        if state.needs_complex then state
        else
          let value = translate_module_expression inner_mexpr in
          { state with bindings = { mb_id = name_id; mb_value = value } :: state.bindings }

    | TypedStructureRecModule _ ->
        (* Recursive modules always require complex translation *)
        { state with needs_complex = true }

    | TypedStructureExternal ext ->
        if state.needs_complex then state
        else
          let func = make_ffi_wrapper ext.external_spec in
          { state with bindings = { mb_id = ext.external_id; mb_value = func } :: state.bindings }

    | TypedStructureTypeExtension _ ->
        (* Type extensions don't generate runtime code *)
        state

    | TypedStructureError _ ->
        state
  in

  let initial = { needs_complex = false; defined_count = 0; function_count = 0; bindings = [] } in
  let final = List.fold_left analyze_item initial structure in

  (* Multiple functions heuristic: mutual recursion likely *)
  let needs_complex =
    final.needs_complex || (final.function_count > 1 && final.defined_count > 1)
  in

  if needs_complex then NeedsComplexTranslation
  else SimpleModule (List.rev final.bindings)

and translate_structure_with_opens structure =
  let open Typing.Typed_tree in

  let rec process items local_bindings module_fields =
    match items with
    | [] ->
      let module_table = LambdaModule (List.rev module_fields) in
      List.fold_left (fun body binding ->
        match binding with
        | `Single (id, value) -> LambdaLet (id, value, body)
        | `Recursive bindings -> LambdaLetRecursive (bindings, body)
      ) module_table local_bindings
    | item :: rest ->
      match item.structure_item_desc with
      | TypedStructureValue (rec_flag, bindings) ->
          let translated_bindings, field_refs =
            List.fold_right (fun (binding : typed_binding) (trans_acc, refs_acc) ->
              match binding.binding_pattern.pattern_desc with
              | TypedPatternVariable id ->
                let translated = translate_expression binding.binding_expression in
                let field_ref = { mb_id = id; mb_value = LambdaVariable id } in
                ((id, translated) :: trans_acc, field_ref :: refs_acc)
              | _ -> (trans_acc, refs_acc)
            ) bindings ([], [])
          in

          let new_local_bindings = match rec_flag with
            | Parsing.Syntax_tree.Recursive ->
                (`Recursive translated_bindings) :: local_bindings
            | Parsing.Syntax_tree.Nonrecursive ->
                List.fold_right
                  (fun (id, expr) acc -> (`Single (id, expr)) :: acc)
                  translated_bindings local_bindings
          in

          process rest new_local_bindings (List.rev_append field_refs module_fields)

      | TypedStructureType _
      | TypedStructureModuleType _ ->
          process rest local_bindings module_fields

      | TypedStructureModule (name_id, inner_mexpr) ->
          let translated_module = translate_module_expression inner_mexpr in
          let new_local = `Single (name_id, translated_module) in
          let new_field = { mb_id = name_id; mb_value = LambdaVariable name_id } in
          process rest (new_local :: local_bindings) (new_field :: module_fields)

      | TypedStructureRecModule rec_bindings ->
          (* Recursive modules are translated as a recursive let group *)
          let translated_bindings = List.map (fun (binding : Typing.Typed_tree.typed_rec_module_binding) ->
            (binding.rec_module_id, translate_module_expression binding.rec_module_expr)
          ) rec_bindings in
          let new_local = `Recursive translated_bindings in
          let new_fields = List.map (fun (binding : Typing.Typed_tree.typed_rec_module_binding) ->
            { mb_id = binding.rec_module_id; mb_value = LambdaVariable binding.rec_module_id }
          ) rec_bindings in
          process rest (new_local :: local_bindings) (List.rev_append new_fields module_fields)

      | TypedStructureOpen (module_path, opened_bindings) ->
          let module_lambda = translate_module_path module_path in
          let new_locals, new_fields = translate_opened_bindings module_lambda opened_bindings in
          process rest (List.rev_append new_locals local_bindings) (List.rev_append new_fields module_fields)

      | TypedStructureInclude (mexpr, included_bindings) ->
          let translated_mexpr = translate_module_expression mexpr in
          let temp_id = Identifier.create Codegen_constants.included_module_prefix in
          let temp_binding = `Single (temp_id, translated_mexpr) in
          let new_locals, new_fields = translate_opened_bindings (LambdaVariable temp_id) included_bindings in
          process rest (List.rev_append new_locals (temp_binding :: local_bindings)) (List.rev_append new_fields module_fields)

      | TypedStructureExternal ext ->
          let ffi_wrapper = make_ffi_wrapper ext.external_spec in
          let new_local = `Single (ext.external_id, ffi_wrapper) in
          let new_field = { mb_id = ext.external_id; mb_value = LambdaVariable ext.external_id } in
          process rest (new_local :: local_bindings) (new_field :: module_fields)

      | TypedStructureTypeExtension _ ->
          (* Type extensions don't generate runtime code *)
          process rest local_bindings module_fields

      | TypedStructureError _ ->
          process rest local_bindings module_fields
  in
  process structure [] []

and translate_module_expression mexpr =
  let open Typing.Typed_tree in
  match mexpr.module_desc with
  | TypedModuleStructure structure ->
    begin match analyze_and_collect_structure structure with
    | SimpleModule bindings -> LambdaModule bindings
    | NeedsComplexTranslation -> translate_structure_with_opens structure
    end
  | TypedModulePath path ->
    translate_module_path path
  | TypedModuleFunctor (param, body) ->
      let param_id = match param with
        | Typing.Module_types.FunctorParamNamed { parameter_id; _ } -> parameter_id
        | Typing.Module_types.FunctorParamUnit ->
            (* Generative functor: use a dummy parameter *)
            Common.Identifier.create "_unit"
      in
      LambdaFunctor (param_id, translate_module_expression body)
  | TypedModuleApply (func_mexpr, arg_mexpr) ->
    LambdaFunctorApply (translate_module_expression func_mexpr, translate_module_expression arg_mexpr)
  | TypedModuleConstraint (inner, _) ->
    translate_module_expression inner

  | TypedModuleUnpack (expr, _module_type) ->
    (* First-class module unpacking: (val e : MT)
       The expression is a packed module (a table), which we can use directly
       as a module structure since modules are already tables at runtime. *)
    translate_expression expr

and translate_expression (expr : Typing.Typed_tree.typed_expression) : lambda =
  let open Typing.Typed_tree in

  match expr.expression_desc with
  | TypedExpressionVariable (id, _def_loc) ->
    LambdaVariable id

  | TypedExpressionConstant const ->
    LambdaConstant (translate_constant const)

  | TypedExpressionTuple exprs ->
    let translated = List.map translate_expression exprs in
    LambdaMakeBlock (0, translated)

  | TypedExpressionConstructor (ctor_info, arg_expr) ->
    let { Typing.Types.constructor_name; constructor_tag_index;
          constructor_type_name; constructor_argument_type; _ } = ctor_info in

    let is_extension = constructor_tag_index < 0 in
    let tag = {
      tag_name = constructor_name;
      tag_index = constructor_tag_index;
      tag_type_name = constructor_type_name;
      tag_is_nullary = Option.is_none constructor_argument_type;
      tag_is_extension = is_extension;
    } in

    begin match arg_expr with
    | Some arg ->
        LambdaConstructor (tag, Some (translate_expression arg))
    | None ->
        if Option.is_some constructor_argument_type then
          let arg_id = Identifier.create "ctor_arg" in
          LambdaFunction ([arg_id], LambdaConstructor (tag, Some (LambdaVariable arg_id)))
        else
          LambdaConstructor (tag, None)
    end

  | TypedExpressionApply (func_expr, labeled_arg_exprs) ->
    let func = translate_expression func_expr in
    (* Extract expressions from labeled pairs - Lua doesn't have labeled args *)
    let args = List.map (fun (_, e) -> translate_expression e) labeled_arg_exprs in
    begin match func with
    | LambdaVariable id ->
      let name = Identifier.name id in
      begin match primitive_of_operator name with
      | Some prim -> LambdaPrimitive (prim, args)
      | None -> LambdaApply (func, args)
      end
    | _ -> LambdaApply (func, args)
    end

  | TypedExpressionPartialApply { partial_func; partial_slots } ->
    let func = translate_expression partial_func in

    let _, needed_ids_rev, all_args_rev =
      List.fold_left (fun (counter, ids, args) (_, slot) ->
        match slot with
        | SlotFilled expr ->
            let arg = translate_expression expr in
            (counter, ids, arg :: args)
        | SlotNeeded _ ->
            let param_id = Identifier.create (Printf.sprintf "partial_arg%d" counter) in
            (counter + 1, param_id :: ids, LambdaVariable param_id :: args)
      ) (0, [], []) partial_slots
    in

    let param_ids = List.rev needed_ids_rev in
    let all_args = List.rev all_args_rev in
    LambdaFunction (param_ids, LambdaApply (func, all_args))

  | TypedExpressionFunction (labeled_param_patterns, body_expr) ->
      (* Extract patterns from labeled pairs - Lua doesn't have labeled args *)
      let runtime_patterns = List.filter_map (fun (_, p : _ * typed_pattern) ->
        match p.pattern_desc with
        | TypedPatternLocallyAbstract _ -> None
        | _ -> Some p
      ) labeled_param_patterns in

      let param_ids =
        List.map (fun (p : typed_pattern) ->
          match p.pattern_desc with
          | TypedPatternVariable id -> id
          | _ -> Identifier.create Codegen_constants.param_prefix
        ) runtime_patterns
      in

      let translated_body = translate_expression body_expr in

      let body_with_bindings =
        List.fold_right2 (fun pattern param_id body ->
          match pattern.pattern_desc with
          | TypedPatternVariable _ -> body
          | _ -> translate_pattern_binding pattern (LambdaVariable param_id) body
        ) runtime_patterns param_ids translated_body
      in

      LambdaFunction (param_ids, body_with_bindings)

  | TypedExpressionLet (rec_flag, bindings, body_expr) ->
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      let translated_body = translate_expression body_expr in
      List.fold_right (fun (binding : typed_binding) body ->
        let translated_expr = translate_expression binding.binding_expression in
        translate_pattern_binding binding.binding_pattern translated_expr body
      ) bindings translated_body

    | Parsing.Syntax_tree.Recursive ->
      let translated_body = translate_expression body_expr in
      let rec_bindings =
        List.map (fun binding ->
          let id = extract_binding_id ~fallback_prefix:Codegen_constants.rec_prefix binding in
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
    let scrutinee_id = Identifier.create Codegen_constants.scrutinee_prefix in
    let decision_tree = Pattern_match.compile_match match_arms in
    let match_body = translate_decision_tree
      (LambdaVariable scrutinee_id)
      decision_tree
      translate_expression
    in
    LambdaLet (scrutinee_id, translated_scrutinee, match_body)

  | TypedExpressionModuleAccess (path, name) ->
    LambdaModuleAccess (translate_module_path path, name)

  | TypedExpressionRef inner ->
    LambdaRef (translate_expression inner)

  | TypedExpressionDeref ref_expr ->
    LambdaDeref (translate_expression ref_expr)

  | TypedExpressionAssign (ref_expr, value_expr) ->
    LambdaAssign (translate_expression ref_expr, translate_expression value_expr)

  | TypedExpressionAssert cond_expr ->
    (* assert e => if e then () else error("Assertion failed") *)
    let cond = translate_expression cond_expr in
    let unit_value = LambdaConstant ConstantUnit in
    let error_call = LambdaPrimitive (PrimError, [LambdaConstant (ConstantString "Assertion failed")]) in
    LambdaIfThenElse (cond, unit_value, error_call)

  | TypedExpressionWhile (cond_expr, body_expr) ->
    LambdaWhile (translate_expression cond_expr, translate_expression body_expr)

  | TypedExpressionFor (var_id, start_expr, end_expr, direction, body_expr) ->
    LambdaFor (
      var_id,
      translate_expression start_expr,
      translate_expression end_expr,
      direction,
      translate_expression body_expr
    )

  | TypedExpressionPolyVariant (tag, arg) ->
    LambdaPolyVariant (tag, Option.map translate_expression arg)

  | TypedExpressionPack (module_expr, _module_type) ->
    (* First-class module packing: (module ME : MT)
       Modules are already tables at runtime, so packing just means
       translating the module expression to a lambda value. *)
    translate_module_expression module_expr

  | TypedExpressionLetModule (id, module_expr, body) ->
    (* Local module binding: let module M = ME in body
       Translate as: let M = translate(ME) in translate(body) *)
    LambdaLet (id, translate_module_expression module_expr, translate_expression body)

  | TypedExpressionError _ ->
    LambdaApply (
      LambdaVariable (Identifier.create "error"),
      [LambdaConstant (ConstantString "Error recovery: invalid expression")]
    )

let translate_structure_item (item : Typing.Typed_tree.typed_structure_item) : lambda list =
  let open Typing.Typed_tree in
  match item.structure_item_desc with
  | TypedStructureValue (rec_flag, bindings) ->
    begin match rec_flag with
    | Parsing.Syntax_tree.Nonrecursive ->
      List.map (fun binding ->
        let id = extract_binding_id ~fallback_prefix:Codegen_constants.top_prefix binding in
        let translated_expr = translate_expression binding.binding_expression in
        LambdaLet (id, translated_expr, LambdaConstant ConstantUnit)
      ) bindings

    | Parsing.Syntax_tree.Recursive ->
      let rec_bindings =
        List.map (fun binding ->
          let id = extract_binding_id ~fallback_prefix:Codegen_constants.rec_prefix binding in
          let translated_expr = translate_expression binding.binding_expression in
          (id, translated_expr)
        ) bindings
      in
      [LambdaLetRecursive (rec_bindings, LambdaConstant ConstantUnit)]
    end

  | TypedStructureType _ ->
    []

  | TypedStructureModuleType _ ->
    []

  | TypedStructureModule (module_id, mexpr) ->
    [LambdaLet (module_id, translate_module_expression mexpr, LambdaConstant ConstantUnit)]

  | TypedStructureRecModule rec_bindings ->
    (* Recursive modules: translate each binding, then create recursive let *)
    let bindings = List.map (fun (binding : Typing.Typed_tree.typed_rec_module_binding) ->
      (binding.rec_module_id, translate_module_expression binding.rec_module_expr)
    ) rec_bindings in
    [LambdaLetRecursive (bindings, LambdaConstant ConstantUnit)]

  | TypedStructureOpen (module_path, opened_bindings) ->
    let module_expr = translate_module_path module_path in
    List.map (fun (name, id) ->
      LambdaLet (id, LambdaModuleAccess (module_expr, name), LambdaConstant ConstantUnit)
    ) opened_bindings

  | TypedStructureInclude (mexpr, included_bindings) ->
    let module_temp = Identifier.create Codegen_constants.include_prefix in
    let module_binding = LambdaLet (module_temp, translate_module_expression mexpr, LambdaConstant ConstantUnit) in
    let value_bindings = List.map (fun (name, id) ->
      LambdaLet (id, LambdaModuleAccess (LambdaVariable module_temp, name), LambdaConstant ConstantUnit)
    ) included_bindings in
    module_binding :: value_bindings

  | TypedStructureExternal ext ->
    let func = make_ffi_wrapper ext.external_spec in
    [LambdaLet (ext.external_id, func, LambdaConstant ConstantUnit)]

  | TypedStructureTypeExtension _ ->
    (* Type extensions don't generate runtime code - constructors are used in expressions *)
    []

  | TypedStructureError _ ->
    []

let translate_structure structure =
  List.concat_map translate_structure_item structure
