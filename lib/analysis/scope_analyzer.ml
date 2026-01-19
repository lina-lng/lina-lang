open Common
open Typing
open Typing.Typed_tree

type state = {
  current_scope : Scope.scope;
  all_scopes : Scope.scope list ref;
  all_bindings : Scope.binding list ref;
  exported_names : (string, unit) Hashtbl.t;
}

let create_state root_scope =
  {
    current_scope = root_scope;
    all_scopes = ref [ root_scope ];
    all_bindings = ref [];
    exported_names = Hashtbl.create 32;
  }

let enter_scope state kind =
  let new_scope = Scope.create_scope kind (Some state.current_scope) in
  state.all_scopes := new_scope :: !(state.all_scopes);
  { state with current_scope = new_scope }

let add_binding state name id kind location ty =
  let binding =
    {
      Scope.bind_name = name;
      bind_id = id;
      bind_kind = kind;
      bind_location = location;
      bind_type = ty;
      bind_references = [];
      bind_exported = Hashtbl.mem state.exported_names name;
      bind_scope_id = state.current_scope.scope_id;
    }
  in
  let binding = Scope.add_binding state.current_scope binding in
  state.all_bindings := binding :: !(state.all_bindings);
  binding

let register_binding state name id kind location ty =
  ignore (add_binding state name id kind location ty)

let record_reference state name location access =
  match Scope.find_binding state.current_scope name with
  | Some binding ->
      Scope.add_reference binding { ref_location = location; ref_access = access }
  | None -> ()

let record_reference_by_id state id location access =
  let name = Identifier.name id in
  record_reference state name location access

let rec root_module_of_path (path : Types.path) =
  match path with
  | Types.PathIdent id -> Some (Identifier.name id)
  | Types.PathDot (parent, _) -> root_module_of_path parent
  | Types.PathApply (func, _) -> root_module_of_path func
  | Types.PathBuiltin _ | Types.PathLocal _ -> None

let rec record_type_references state location (ty : Types.type_expression) =
  match Types.representative ty with
  | Types.TypeVariable _ -> ()
  | Types.TypeConstructor (path, args) ->
      (match path with
       | Types.PathLocal name -> record_reference state name location Scope.Read
       | Types.PathIdent id ->
           record_reference state (Identifier.name id) location Scope.Read
       | Types.PathDot _ ->
           Option.iter
             (fun name -> record_reference state name location Scope.Read)
             (root_module_of_path path)
       | Types.PathBuiltin _ | Types.PathApply _ -> ());
      List.iter (record_type_references state location) args
  | Types.TypeTuple tys ->
      List.iter (record_type_references state location) tys
  | Types.TypeArrow (_, t1, t2) ->
      record_type_references state location t1;
      record_type_references state location t2
  | Types.TypeRecord row ->
      record_row_type_references state location row
  | Types.TypeRowEmpty -> ()
  | Types.TypePolyVariant poly_row ->
      List.iter
        (fun (_tag, field) ->
          match field with
          | Types.PVFieldPresent (Some ty) ->
              record_type_references state location ty
          | Types.PVFieldPresent None | Types.PVFieldAbsent -> ())
        poly_row.Types.pv_fields;
      record_type_references state location poly_row.Types.pv_more
  | Types.TypePackage pkg ->
      List.iter
        (fun (_name, ty) -> record_type_references state location ty)
        pkg.Types.package_signature

and record_row_type_references state location (row : Types.row) =
  List.iter
    (fun (_label, field) ->
      match field with
      | Types.RowFieldPresent ty -> record_type_references state location ty)
    row.Types.row_fields;
  record_type_references state location row.Types.row_more

let register_record_fields state location fields =
  List.iter
    (fun (field_name, field_type) ->
      register_binding state field_name (Identifier.create field_name)
        Scope.RecordField location None;
      record_type_references state location field_type)
    fields

let rec analyze_pattern state (pattern : typed_pattern) =
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
      let name = Identifier.name id in
      register_binding state name id Scope.PatternVar pattern.pattern_location
        (Some pattern.pattern_type)

  | TypedPatternWildcard -> ()
  | TypedPatternConstant _ -> ()

  | TypedPatternTuple patterns ->
      List.iter (analyze_pattern state) patterns

  | TypedPatternConstructor (ctor_info, arg_opt) ->
      record_reference state ctor_info.constructor_name pattern.pattern_location
        Scope.Pattern;
      Option.iter (analyze_pattern state) arg_opt

  | TypedPatternRecord (fields, _closed) ->
      List.iter
        (fun field ->
          record_reference state field.typed_pattern_field_name
            pattern.pattern_location Scope.Project;
          analyze_pattern state field.typed_pattern_field_pattern)
        fields

  | TypedPatternAlias (inner_pat, id) ->
      analyze_pattern state inner_pat;
      let name = Identifier.name id in
      register_binding state name id Scope.PatternVar pattern.pattern_location
        (Some pattern.pattern_type)

  | TypedPatternOr (left, right) ->
      analyze_pattern state left;
      analyze_pattern state right

  | TypedPatternLocallyAbstract (id, _decl) ->
      let name = Identifier.name id in
      register_binding state name id Scope.TypeName pattern.pattern_location None

  | TypedPatternPolyVariant (_tag, arg_opt) ->
      Option.iter (analyze_pattern state) arg_opt

  | TypedPatternError _ -> ()

let extract_record_fields ty =
  match Types.representative ty with
  | Types.TypeRecord row ->
      Some
        (List.filter_map
           (fun (name, field) ->
             match field with
             | Types.RowFieldPresent field_type -> Some (name, field_type))
           row.Types.row_fields)
  | _ -> None

let rec analyze_expression state (expr : typed_expression) =
  match expr.expression_desc with
  | TypedExpressionVariable (id, _def_loc) ->
      record_reference_by_id state id expr.expression_location Scope.Read

  | TypedExpressionConstant _ -> ()

  | TypedExpressionTuple exprs ->
      List.iter (analyze_expression state) exprs

  | TypedExpressionConstructor (ctor_info, arg_opt) ->
      record_reference state ctor_info.constructor_name expr.expression_location
        Scope.Construct;
      Option.iter (analyze_expression state) arg_opt

  | TypedExpressionApply (func, args) ->
      analyze_expression state func;
      List.iter (fun (_label, arg) -> analyze_expression state arg) args

  | TypedExpressionPartialApply partial ->
      analyze_expression state partial.partial_func;
      List.iter
        (fun (_label, slot) ->
          match slot with
          | SlotFilled e -> analyze_expression state e
          | SlotNeeded _ -> ())
        partial.partial_slots

  | TypedExpressionFunction (params, body) ->
      let func_state =
        enter_scope state (Scope.ScopeFunction { name = None; is_recursive = false })
      in

      let promote_to_parameter (binding : Scope.binding) =
        match Scope.find_binding_local func_state.current_scope binding.bind_name with
        | Some bound -> bound.bind_kind <- Scope.Parameter
        | None -> ()
      in

      List.iter
        (fun (_label, pat) ->
          analyze_pattern func_state pat;
          List.iter promote_to_parameter !(func_state.all_bindings))
        params;

      analyze_expression func_state body

  | TypedExpressionLet (rec_flag, bindings, body) ->
      let let_state = enter_scope state Scope.ScopeLet in
      let is_recursive = rec_flag = Parsing.Syntax_tree.Recursive in
      analyze_let_bindings let_state bindings is_recursive;
      analyze_expression let_state body

  | TypedExpressionIf (cond, then_expr, else_opt) ->
      analyze_expression state cond;
      analyze_expression state then_expr;
      Option.iter (analyze_expression state) else_opt

  | TypedExpressionSequence (left, right) ->
      analyze_expression state left;
      analyze_expression state right

  | TypedExpressionRecord fields ->
      List.iter
        (fun field -> analyze_expression state field.typed_field_value)
        fields

  | TypedExpressionRecordAccess (record, field_name) ->
      analyze_expression state record;
      record_reference state field_name expr.expression_location Scope.Project

  | TypedExpressionRecordUpdate (record, fields) ->
      analyze_expression state record;
      List.iter
        (fun field ->
          record_reference state field.typed_field_name expr.expression_location
            Scope.Write;
          analyze_expression state field.typed_field_value)
        fields

  | TypedExpressionMatch (scrutinee, arms) ->
      analyze_expression state scrutinee;
      List.iter (analyze_match_arm state) arms

  | TypedExpressionModuleAccess (path, name) ->
      record_reference state name expr.expression_location Scope.Read;
      Option.iter
        (fun mod_name ->
          record_reference state mod_name expr.expression_location Scope.Read)
        (root_module_of_path path)

  | TypedExpressionRef init ->
      analyze_expression state init

  | TypedExpressionDeref inner ->
      analyze_expression state inner

  | TypedExpressionAssign (lhs, rhs) ->
      analyze_expression state lhs;
      analyze_expression state rhs

  | TypedExpressionAssert inner ->
      analyze_expression state inner

  | TypedExpressionWhile (cond, body) ->
      analyze_expression state cond;
      analyze_expression state body

  | TypedExpressionFor (id, start, finish, _direction, body) ->
      let for_state = enter_scope state Scope.ScopeBlock in
      let name = Identifier.name id in
      register_binding for_state name id Scope.Variable expr.expression_location None;
      analyze_expression for_state start;
      analyze_expression for_state finish;
      analyze_expression for_state body

  | TypedExpressionPolyVariant (_tag, arg_opt) ->
      Option.iter (analyze_expression state) arg_opt

  | TypedExpressionPack (mod_expr, _mod_type) ->
      analyze_module_expression state mod_expr

  | TypedExpressionLetModule (id, mod_expr, body) ->
      let mod_state = enter_scope state Scope.ScopeLet in
      let name = Identifier.name id in
      register_binding mod_state name id Scope.ModuleAlias
        expr.expression_location None;
      analyze_module_expression mod_state mod_expr;
      analyze_expression mod_state body

  | TypedExpressionError _ -> ()

and analyze_let_bindings state bindings is_recursive =
  if is_recursive then (
    List.iter
      (fun binding -> analyze_let_binding_pattern state binding true)
      bindings;
    List.iter
      (fun binding -> analyze_expression state binding.binding_expression)
      bindings)
  else
    List.iter
      (fun binding ->
        analyze_expression state binding.binding_expression;
        analyze_let_binding_pattern state binding false)
      bindings

and analyze_let_binding_pattern state binding is_recursive =
  let rec extract_binding_info (pat : typed_pattern) =
    record_type_references state pat.pattern_location pat.pattern_type;

    match pat.pattern_desc with
    | TypedPatternVariable id ->
        let name = Identifier.name id in
        let kind =
          if is_function binding.binding_expression then
            Scope.Function { is_recursive }
          else Scope.Variable
        in
        register_binding state name id kind binding.binding_location
          (Some pat.pattern_type)

    | TypedPatternTuple pats ->
        List.iter extract_binding_info pats

    | TypedPatternRecord (fields, _) ->
        List.iter
          (fun f -> extract_binding_info f.typed_pattern_field_pattern)
          fields

    | TypedPatternAlias (inner, id) ->
        extract_binding_info inner;
        let name = Identifier.name id in
        register_binding state name id Scope.PatternVar binding.binding_location
          (Some pat.pattern_type)

    | _ -> analyze_pattern state pat
  in
  extract_binding_info binding.binding_pattern

and is_function (expr : typed_expression) =
  match expr.expression_desc with
  | TypedExpressionFunction _ -> true
  | _ -> false

and analyze_match_arm state (arm : typed_match_arm) =
  let arm_state = enter_scope state Scope.ScopeMatch in
  analyze_pattern arm_state arm.typed_arm_pattern;
  Option.iter (analyze_expression arm_state) arm.typed_arm_guard;
  analyze_expression arm_state arm.typed_arm_expression

and analyze_module_expression state (mod_expr : typed_module_expression) =
  match mod_expr.module_desc with
  | TypedModuleStructure structure ->
      let mod_state = enter_scope state (Scope.ScopeModule { name = None }) in
      analyze_structure mod_state structure

  | TypedModulePath _path -> ()

  | TypedModuleFunctor (param, body) ->
      let func_state =
        enter_scope state (Scope.ScopeFunction { name = None; is_recursive = false })
      in
      let param_name =
        match param with
        | Module_types.FunctorParamNamed { parameter_name; _ } -> parameter_name
        | Module_types.FunctorParamUnit -> "_"
      in
      let param_id = Identifier.create param_name in
      register_binding func_state param_name param_id Scope.ModuleAlias
        mod_expr.module_location None;
      analyze_module_expression func_state body

  | TypedModuleApply (func, arg) ->
      analyze_module_expression state func;
      analyze_module_expression state arg

  | TypedModuleConstraint (inner, _mod_type) ->
      analyze_module_expression state inner

  | TypedModuleUnpack (expr, _mod_type) ->
      analyze_expression state expr

and analyze_structure state structure =
  List.iter (analyze_structure_item state) structure

and analyze_structure_item state (item : typed_structure_item) =
  match item.structure_item_desc with
  | TypedStructureValue (rec_flag, bindings) ->
      let is_recursive = rec_flag = Parsing.Syntax_tree.Recursive in
      analyze_let_bindings state bindings is_recursive

  | TypedStructureType decls ->
      List.iter (analyze_type_declaration state item.structure_item_location) decls

  | TypedStructureModule (id, mod_expr) ->
      let name = Identifier.name id in
      register_binding state name id Scope.ModuleAlias
        item.structure_item_location None;
      analyze_module_expression state mod_expr

  | TypedStructureRecModule bindings ->
      List.iter
        (fun binding ->
          let name = Identifier.name binding.rec_module_id in
          register_binding state name binding.rec_module_id Scope.ModuleAlias
            binding.rec_module_location None)
        bindings;

      List.iter
        (fun binding -> analyze_module_expression state binding.rec_module_expr)
        bindings

  | TypedStructureModuleType (name, _mod_type) ->
      let id = Identifier.create name in
      register_binding state name id Scope.TypeName item.structure_item_location None

  | TypedStructureOpen (_path, opened_bindings) ->
      List.iter
        (fun (name, id) ->
          register_binding state name id Scope.OpenedBinding
            item.structure_item_location None)
        opened_bindings

  | TypedStructureInclude (_mod_expr, included_bindings) ->
      List.iter
        (fun (name, id) ->
          register_binding state name id Scope.IncludedBinding
            item.structure_item_location None)
        included_bindings

  | TypedStructureExternal ext ->
      let name = Identifier.name ext.external_id in
      register_binding state name ext.external_id Scope.External
        ext.external_location (Some ext.external_type)

  | TypedStructureTypeExtension ext ->
      List.iter
        (fun ctor ->
          register_binding state ctor.Types.constructor_name
            (Identifier.create ctor.constructor_name)
            Scope.Constructor item.structure_item_location None)
        ext.extension_constructors

  | TypedStructureExpression expr ->
      analyze_expression state expr

  | TypedStructureError _ -> ()

and analyze_type_declaration state location (decl : Types.type_declaration) =
  let id = Identifier.create decl.declaration_name in
  register_binding state decl.declaration_name id Scope.TypeName location None;

  Option.iter (record_type_references state location) decl.declaration_manifest;

  (match decl.declaration_manifest with
   | Some manifest ->
       Option.iter (register_record_fields state location)
         (extract_record_fields manifest)
   | None -> ());

  match decl.declaration_kind with
  | Types.DeclarationAbstract | Types.DeclarationExtensible -> ()

  | Types.DeclarationVariant constructors ->
      List.iter
        (fun (ctor : Types.constructor_info) ->
          register_binding state ctor.constructor_name
            (Identifier.create ctor.constructor_name)
            Scope.Constructor location None;
          Option.iter
            (record_type_references state location)
            ctor.constructor_argument_type)
        constructors

  | Types.DeclarationRecord fields -> register_record_fields state location fields

let populate_exported_names state signature =
  match signature with
  | None -> ()
  | Some sig_items ->
      let extract_names (item : Module_types.signature_item) =
        match item with
        | Module_types.SigValue (name, _) ->
            Hashtbl.replace state.exported_names name ()
        | Module_types.SigType (name, _) ->
            Hashtbl.replace state.exported_names name ()
        | Module_types.SigModule (name, _) ->
            Hashtbl.replace state.exported_names name ()
        | Module_types.SigModuleType (name, _) ->
            Hashtbl.replace state.exported_names name ()
        | Module_types.SigExtensionConstructor ctor ->
            Hashtbl.replace state.exported_names ctor.Types.constructor_name ()
      in
      List.iter extract_names sig_items

let mark_exported_bindings state =
  List.iter
    (fun binding ->
      if Hashtbl.mem state.exported_names binding.Scope.bind_name then
        Scope.mark_exported binding)
    !(state.all_bindings)

let analyze_with_signature structure signature =
  let root_scope = Scope.create_scope (Scope.ScopeModule { name = None }) None in
  let state = create_state root_scope in

  populate_exported_names state signature;
  analyze_structure state structure;
  mark_exported_bindings state;

  {
    Scope.tree_root = root_scope;
    tree_all_scopes = List.rev !(state.all_scopes);
    tree_all_bindings = List.rev !(state.all_bindings);
  }

let analyze structure = analyze_with_signature structure None
