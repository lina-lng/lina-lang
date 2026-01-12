(** Typing context for type inference.

    The typing context is the sole source of truth for:
    - Current let-nesting level (for generalization)
    - Fresh type variable ID generation
    - Environment access

    All type inference operations should go through this context. *)

type t = {
  env : Environment.t;
  level : int;
}

(* Context Creation *)

let create env = {
  env;
  level = 1;
}

let with_environment env ctx = { ctx with env }

(* Environment Access *)

let environment ctx = ctx.env

(* Level Management *)

let current_level ctx = ctx.level

let enter_level ctx =
  { ctx with level = ctx.level + 1 }

let leave_level ctx =
  { ctx with level = ctx.level - 1 }

(* Type Variable Generation *)

let fresh_type_variable_id ctx =
  (* Use Types.fresh_type_variable_id for globally unique IDs.
     This ensures type variables have unique IDs even across modules. *)
  let id = Types.fresh_type_variable_id () in
  (id, ctx)

let new_type_variable_at_level ctx level =
  let (id, ctx') = fresh_type_variable_id ctx in
  let tv = Types.TypeVariable { Types.id; level; link = None; weak = false } in
  (tv, ctx')

let new_type_variable ctx =
  new_type_variable_at_level ctx ctx.level

(* Type Lookup *)

let type_lookup ctx path =
  Environment.find_type_by_path path ctx.env

let module_type_lookup ctx path =
  match path with
  | Types.PathLocal name ->
    begin match Environment.find_module_type name ctx.env with
    | Some (Some mty) -> Some mty
    | _ -> None
    end
  | Types.PathIdent id ->
    begin match Environment.find_module_type (Common.Identifier.name id) ctx.env with
    | Some (Some mty) -> Some mty
    | _ -> None
    end
  | Types.PathDot (parent_path, name) ->
    begin match Environment.find_module_by_path parent_path ctx.env with
    | Some binding ->
      begin match binding.Module_types.binding_type with
      | Module_types.ModTypeSig sig_ ->
        begin match Module_types.find_module_type_in_sig name sig_ with
        | Some (Some mty) -> Some mty
        | _ -> None
        end
      | _ -> None
      end
    | None -> None
    end
  | Types.PathBuiltin _ | Types.PathApply _ ->
    None

(* Type Scheme Operations *)

let generalize ctx ty =
  Type_scheme.generalize ~level:ctx.level ty

let instantiate ctx scheme =
  if scheme.Types.quantified_variables = [] then
    (scheme.Types.body, ctx)
  else begin
    (* Use a ref to track context updates from fresh_var *)
    let ctx_ref = ref ctx in
    let fresh_var () =
      let (ty, ctx') = new_type_variable !ctx_ref in
      ctx_ref := ctx';
      ty
    in
    let result = Type_scheme.instantiate ~fresh_var scheme in
    (result, !ctx_ref)
  end

