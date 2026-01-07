(** Typing context for type inference. *)

type t = {
  env : Environment.t;
  level : int;
  next_var_id : int;
}

(* Context Creation *)

let create env = {
  env;
  level = 1;
  next_var_id = 0;
}

let with_environment env ctx = { ctx with env }

(* Environment Access *)

let environment ctx = ctx.env

(* Level Management *)

let current_level ctx = ctx.level

let enter_level ctx = { ctx with level = ctx.level + 1 }

let leave_level ctx = { ctx with level = ctx.level - 1 }

(* Type Variable Generation *)

let fresh_type_variable_id ctx =
  let id = ctx.next_var_id in
  (id, { ctx with next_var_id = ctx.next_var_id + 1 })

let new_type_variable_at_level ctx level =
  let (id, ctx') = fresh_type_variable_id ctx in
  let tv = Types.TypeVariable { Types.id; level; link = None } in
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
      begin match binding.Module_types.mod_type with
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
  let current = ctx.level in
  let generalized_vars = ref [] in
  let rec collect ty =
    match Types.representative ty with
    | Types.TypeVariable tv when tv.Types.level > current ->
      if not (List.exists (fun v -> v.Types.id = tv.Types.id) !generalized_vars) then begin
        tv.Types.level <- Types.generic_level;
        generalized_vars := tv :: !generalized_vars
      end
    | Types.TypeVariable _ -> ()
    | Types.TypeConstructor (_, args) -> List.iter collect args
    | Types.TypeTuple elements -> List.iter collect elements
    | Types.TypeArrow (arg, result) ->
      collect arg;
      collect result
    | Types.TypeRecord row -> collect_row row
    | Types.TypeRowEmpty -> ()
  and collect_row row =
    List.iter (fun (_, field) ->
      match field with
      | Types.RowFieldPresent ty -> collect ty
    ) row.Types.row_fields;
    collect row.Types.row_more
  in
  collect ty;
  { Types.quantified_variables = !generalized_vars; body = ty }

let instantiate ctx scheme =
  if scheme.Types.quantified_variables = [] then
    (scheme.Types.body, ctx)
  else begin
    let ctx_ref = ref ctx in
    let substitution =
      List.map (fun tv ->
        let (fresh_ty, ctx') = new_type_variable !ctx_ref in
        ctx_ref := ctx';
        (tv.Types.id, fresh_ty)
      ) scheme.Types.quantified_variables
    in
    let rec copy ty =
      match Types.representative ty with
      | Types.TypeVariable tv ->
        begin match List.assoc_opt tv.Types.id substitution with
        | Some fresh_ty -> fresh_ty
        | None -> ty
        end
      | Types.TypeConstructor (path, args) ->
        Types.TypeConstructor (path, List.map copy args)
      | Types.TypeTuple elements ->
        Types.TypeTuple (List.map copy elements)
      | Types.TypeArrow (arg, result) ->
        Types.TypeArrow (copy arg, copy result)
      | Types.TypeRecord row ->
        Types.TypeRecord (copy_row row)
      | Types.TypeRowEmpty ->
        Types.TypeRowEmpty
    and copy_row row = {
      Types.row_fields = List.map (fun (name, field) ->
        (name, match field with
          | Types.RowFieldPresent ty -> Types.RowFieldPresent (copy ty))
      ) row.Types.row_fields;
      row_more = copy row.Types.row_more;
    }
    in
    (copy scheme.Types.body, !ctx_ref)
  end

(* Backward Compatibility *)

let install_globals ctx =
  (* Set Types global state *)
  Types.set_level ctx.level;
  Types.set_next_type_variable_id ctx.next_var_id;
  (* Set Unification type lookup *)
  Unification.set_type_lookup (fun path -> type_lookup ctx path);
  (* Set Signature_match module type lookup *)
  Signature_match.set_module_type_lookup (fun path -> module_type_lookup ctx path)
