(** Binding type inference.

    This module handles type inference for let-bindings, including:
    - Non-recursive bindings with generalization
    - Recursive bindings with mutual recursion support
    - Polymorphic recursion via TypeForall annotations

    Extracted from expression_infer.ml to reduce complexity.

    {2 Polymorphic Recursion}

    Bindings with [type a b.] annotations enable polymorphic recursion:
    - The annotated type parameters become rigid type variables
    - Recursive calls within the body instantiate the full scheme
    - This allows writing functions like [length : type a. a expr -> int]

    {2 Context Threading}

    All inference functions thread [Typing_context.t] to track:
    - Current generalization level
    - Next type variable ID
    - Typing environment *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn =
  Typing_context.t -> expression -> typed_expression * Typing_context.t

(** Expression inference with expected type for bidirectional checking. *)
type expression_infer_expected_fn =
  Typing_context.t ->
  type_expression option ->
  expression ->
  typed_expression * Typing_context.t

(** {1 Polymorphic Recursion Types} *)

(** Binding info for recursive bindings - tracks whether standard or polymorphic recursion. *)
type rec_binding_info =
  | PolyRecBinding of {
      name : string;
      id : Identifier.t;
      scheme : type_scheme;
      rigid_vars : (string * type_variable) list;
      binding : binding;
    }
  | StandardRecBinding of {
      name : string;
      id : Identifier.t;
      mono_type : type_expression;
      binding : binding;
    }

(** {1 Pattern Binding Collection} *)

(** Collect all variable bindings from a typed pattern.
    Returns a list of (name, identifier, type) tuples. *)
let rec collect_pattern_bindings pattern =
  match pattern.pattern_desc with
  | TypedPatternVariable id ->
    [(Identifier.name id, id, pattern.pattern_type)]
  | TypedPatternWildcard
  | TypedPatternConstant _
  | TypedPatternLocallyAbstract _ ->
    []
  | TypedPatternTuple patterns ->
    List.concat_map collect_pattern_bindings patterns
  | TypedPatternConstructor (_, None) ->
    []
  | TypedPatternConstructor (_, Some p) ->
    collect_pattern_bindings p
  | TypedPatternRecord (fields, _) ->
    List.concat_map (fun f -> collect_pattern_bindings f.typed_pattern_field_pattern) fields
  | TypedPatternPolyVariant (_, None) ->
    []
  | TypedPatternPolyVariant (_, Some p) ->
    collect_pattern_bindings p
  | TypedPatternError _ ->
    []

(** {1 Polymorphic Recursion Support} *)

(** Extract polymorphic recursion annotation from a binding.
    Returns [Some (name, forall_vars, body_ty, loc)] if the binding has form:
    [let name : type a b. body_ty = ...]
    Returns [None] otherwise. *)
let extract_poly_rec_annotation (binding : binding) =
  match binding.binding_pattern.Location.value with
  | PatternConstraint (inner_pat, ty_expr) ->
    begin match ty_expr.Location.value with
    | TypeForall (vars, body) ->
      begin match inner_pat.Location.value with
      | PatternVariable name -> Some (name, vars, body, ty_expr.Location.location)
      | _ -> None
      end
    | _ -> None
    end
  | _ -> None

(** Check a TypeForall annotation for polymorphic recursion.
    Creates rigid type variables for bound names and checks the body type. *)
let check_forall_annotation ctx forall_vars body_ty_expr =
  (* Create rigid type variables for each bound name.
     Note: We accumulate in reverse order during fold_left for O(1) cons,
     then reverse at the end. *)
  let rigid_vars, ctx =
    List.fold_left (fun (vars, ctx) name ->
      let tv, ctx = Typing_context.new_rigid_type_variable ctx in
      match tv with
      | TypeVariable tv_rec -> ((name, tv_rec) :: vars, ctx)
      | _ -> Compiler_error.internal_error "new_rigid_type_variable didn't return TypeVariable"
    ) ([], ctx) forall_vars
  in
  let rigid_vars = List.rev rigid_vars in
  (* Check the body type with rigid vars as parameters *)
  let param_names = List.map fst rigid_vars in
  let param_vars = List.map snd rigid_vars in
  let body_type, ctx = Type_expression_check.check_type_expression_with_params ctx param_names param_vars body_ty_expr in
  (* Build scheme: the rigid vars become quantified variables *)
  let scheme = { quantified_variables = param_vars; body = body_type } in
  (scheme, rigid_vars, ctx)

(** {1 Helper Functions} *)

(** Unify types using context's environment for alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** {1 Binding Inference} *)

(** [infer_bindings ~infer_expr ~infer_expr_expected ctx rec_flag bindings]
    infers types for a list of bindings.

    @param infer_expr The expression inference function callback
    @param infer_expr_expected The expression inference with expected type callback
    @param ctx The typing context
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_ctx)] *)
let infer_bindings
    ~(infer_expr : expression_infer_fn)
    ~(infer_expr_expected : expression_infer_expected_fn)
    ctx
    rec_flag
    bindings
  =
  match rec_flag with
  | Nonrecursive ->
    let ctx = Typing_context.enter_level ctx in
    let typed_bindings, ctx =
      List.fold_left (fun (bs, ctx) (binding : binding) ->
        (* Check for TypeForall annotation (e.g., let f : type a. a -> a = ...) *)
        match extract_poly_rec_annotation binding with
        | Some (name, forall_vars, body_ty_expr, _loc) ->
          (* Non-recursive binding with TypeForall: similar to poly-rec but simpler *)
          let scheme, rigid_vars, ctx = check_forall_annotation ctx forall_vars body_ty_expr in
          let id = Identifier.create name in
          (* Create fresh rigid type variables for the expected type so that
             the original scheme's type variables remain unlinked. *)
          let fresh_rigid_vars, ctx =
            List.fold_left (fun (acc, ctx) (var_name, tv) ->
              let fresh, ctx = Typing_context.new_rigid_type_variable ctx in
              let fresh_tv = match fresh with Types.TypeVariable ftv -> ftv | _ -> assert false in
              ((var_name, tv.Types.id, fresh_tv) :: acc, ctx)
            ) ([], ctx) rigid_vars
          in
          let expected_type =
            Type_traversal.map (fun ty ->
              match Types.representative ty with
              | Types.TypeVariable tv ->
                begin match List.find_opt (fun (_, orig_id, _) -> orig_id = tv.Types.id) fresh_rigid_vars with
                | Some (_, _, fresh_tv) -> Types.TypeVariable fresh_tv
                | None -> ty
                end
              | _ -> ty
            ) scheme.Types.body
          in
          (* Add fresh rigid vars as type aliases for body checking *)
          let body_env =
            List.fold_left (fun env (var_name, _, fresh_tv) ->
              let decl = {
                Types.declaration_name = var_name;
                declaration_parameters = [];
                declaration_variances = [];
                declaration_manifest = Some (Types.TypeVariable fresh_tv);
                declaration_kind = Types.DeclarationAbstract;
                declaration_private = false;
                declaration_constraints = [];
              } in
              Environment.add_type var_name decl env
            ) (Typing_context.environment ctx) fresh_rigid_vars
          in
          let body_ctx = Typing_context.with_environment body_env ctx in
          let typed_expr, _body_ctx =
            infer_expr_expected body_ctx (Some expected_type) binding.binding_expression
          in
          let ctx = Typing_context.leave_level ctx in
          let env = Typing_context.environment ctx in
          let env = Environment.add_value name id scheme binding.binding_location env in
          let ctx = Typing_context.with_environment env ctx in
          let ctx = Typing_context.enter_level ctx in
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = scheme.body;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: bs, ctx)

        | None ->
          (* Standard non-recursive binding *)
          let typed_expr, ctx = infer_expr ctx binding.binding_expression in
          let ctx = Typing_context.leave_level ctx in
          let level = Typing_context.current_level ctx in
          let ctx = Typing_context.enter_level ctx in
          let typed_pat, pat_ty, ctx = Pattern_infer.infer_pattern ctx binding.binding_pattern in
          unify ctx binding.binding_location pat_ty typed_expr.expression_type;
          let env = Typing_context.environment ctx in
          (* Apply value restriction: only generalize if expression is a value. *)
          let env =
            let pattern_bindings = collect_pattern_bindings typed_pat in
            List.fold_left (fun env (name, id, ty) ->
              let binding_scheme =
                Inference_utils.compute_binding_scheme_with_env ~level ~env typed_expr ty
              in
              Environment.add_value name id binding_scheme binding.binding_location env
            ) env pattern_bindings
          in
          let ctx = Typing_context.with_environment env ctx in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: bs, ctx)
      ) ([], ctx) bindings
    in
    let ctx = Typing_context.leave_level ctx in
    (List.rev typed_bindings, ctx)

  | Recursive ->
    let ctx = Typing_context.enter_level ctx in
    let env = Typing_context.environment ctx in

    (* First pass: analyze bindings and set up environment *)
    let env, binding_info_list, ctx =
      List.fold_left (fun (env, info_list, ctx) (binding : binding) ->
        match extract_poly_rec_annotation binding with
        | Some (name, forall_vars, body_ty_expr, _loc) ->
          (* Polymorphic recursion: create scheme from annotation *)
          let scheme, rigid_vars, ctx = check_forall_annotation ctx forall_vars body_ty_expr in
          let id = Identifier.create name in
          (* Add with full scheme so recursive calls instantiate fresh vars *)
          let env = Environment.add_value name id scheme binding.binding_location env in
          let info = PolyRecBinding { name; id; scheme; rigid_vars; binding } in
          (env, info :: info_list, ctx)

        | None ->
          (* Standard recursive binding *)
          begin match binding.binding_pattern.Location.value with
          | PatternVariable name ->
            let mono_type, ctx = Typing_context.new_type_variable ctx in
            let id = Identifier.create name in
            let env = Environment.add_value name id (trivial_scheme mono_type) binding.binding_location env in
            let info = StandardRecBinding { name; id; mono_type; binding } in
            (env, info :: info_list, ctx)
          | PatternConstraint (inner_pat, _ty_expr) ->
            (* Non-forall constraint - extract name and use standard approach *)
            begin match inner_pat.Location.value with
            | PatternVariable name ->
              let mono_type, ctx = Typing_context.new_type_variable ctx in
              let id = Identifier.create name in
              let env = Environment.add_value name id (trivial_scheme mono_type) binding.binding_location env in
              let info = StandardRecBinding { name; id; mono_type; binding } in
              (env, info :: info_list, ctx)
            | _ ->
              Compiler_error.type_error binding.binding_location
                "Recursive bindings must be simple variables"
            end
          | _ ->
            Compiler_error.type_error binding.binding_location
              "Recursive bindings must be simple variables"
          end
      ) (env, [], ctx) bindings
    in
    let binding_info_list = List.rev binding_info_list in
    let ctx = Typing_context.with_environment env ctx in

    (* Second pass: type-check bodies *)
    let typed_bindings, ctx =
      List.fold_left (fun (typed_bindings, ctx) info ->
        match info with
        | PolyRecBinding { name = _; id; scheme; rigid_vars; binding } ->
          (* Add rigid vars as type aliases for body checking *)
          let body_env =
            List.fold_left (fun env (var_name, tv) ->
              let decl = {
                Types.declaration_name = var_name;
                declaration_parameters = [];
                declaration_variances = [];
                declaration_manifest = Some (TypeVariable tv);
                declaration_kind = Types.DeclarationAbstract;
                declaration_private = false;
                declaration_constraints = [];
              } in
              Environment.add_type var_name decl env
            ) (Typing_context.environment ctx) rigid_vars
          in
          let body_ctx = Typing_context.with_environment body_env ctx in
          (* Type-check the body expression with expected type from annotation *)
          let typed_expr, _body_ctx =
            infer_expr_expected body_ctx (Some scheme.body) binding.binding_expression
          in
          (* Unify inferred type with annotated type *)
          unify ctx binding.binding_location scheme.body typed_expr.expression_type;
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = scheme.body;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: typed_bindings, ctx)

        | StandardRecBinding { name = _; id; mono_type; binding } ->
          (* Standard recursive binding *)
          let typed_expr, ctx = infer_expr ctx binding.binding_expression in
          unify ctx binding.binding_location mono_type typed_expr.expression_type;
          let typed_pat = {
            pattern_desc = TypedPatternVariable id;
            pattern_type = mono_type;
            pattern_location = binding.binding_pattern.Location.location;
          } in
          let typed_binding = {
            Typed_tree.binding_pattern = typed_pat;
            binding_expression = typed_expr;
            binding_location = binding.binding_location;
          } in
          (typed_binding :: typed_bindings, ctx)
      ) ([], ctx) binding_info_list
    in
    let typed_bindings = List.rev typed_bindings in

    (* Third pass: generalize and update environment *)
    let ctx = Typing_context.leave_level ctx in
    let level = Typing_context.current_level ctx in
    let env = Typing_context.environment ctx in
    let env =
      List.fold_left2 (fun env info typed_binding ->
        match info with
        | PolyRecBinding { name; id; scheme; rigid_vars = _; binding = _ } ->
          (* Poly-rec binding: keep the annotation scheme *)
          Environment.add_value name id scheme typed_binding.binding_location env

        | StandardRecBinding { name; id; mono_type; binding = _ } ->
          (* Standard binding: generalize as normal *)
          let scheme = Inference_utils.compute_binding_scheme_with_env ~level ~env typed_binding.binding_expression mono_type in
          Environment.add_value name id scheme typed_binding.binding_location env
      ) env binding_info_list typed_bindings
    in
    let ctx = Typing_context.with_environment env ctx in
    (typed_bindings, ctx)
