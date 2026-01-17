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

let create env = {
  env;
  level = 1;
}

let with_environment env ctx = { ctx with env }

let environment ctx = ctx.env

let current_level ctx = ctx.level

let enter_level ctx =
  { ctx with level = ctx.level + 1 }

let leave_level ctx =
  { ctx with level = ctx.level - 1 }

let fresh_type_variable_id ctx =
  let id = Types.fresh_type_variable_id () in
  (id, ctx)

let new_type_variable_at_level ctx level =
  let (id, ctx') = fresh_type_variable_id ctx in
  let tv = Types.TypeVariable { Types.id; level; link = None; weak = false; rigid = false } in
  (tv, ctx')

let new_type_variable ctx =
  new_type_variable_at_level ctx ctx.level

(** {2 Batch Operations} *)

(** [new_type_variables ctx count] creates [count] fresh type variables.
    Returns the type_variable records (not type_expression wrappers).
    This is a convenience function for creating multiple type parameters. *)
let new_type_variables ctx count =
  let rec loop ctx acc remaining =
    if remaining = 0 then (List.rev acc, ctx)
    else
      let ty, ctx = new_type_variable ctx in
      match ty with
      | Types.TypeVariable tv -> loop ctx (tv :: acc) (remaining - 1)
      | _ -> Common.Compiler_error.internal_error "new_type_variable returned non-variable"
  in
  loop ctx [] count

(** Create a rigid type variable for locally abstract types.
    Rigid variables don't unify globally - instead, GADT pattern matching
    extracts equations on them. *)
let new_rigid_type_variable ctx =
  let (id, ctx') = fresh_type_variable_id ctx in
  let tv = Types.TypeVariable { Types.id; level = ctx.level; link = None; weak = false; rigid = true } in
  (tv, ctx')

let type_lookup ctx path =
  Environment.find_type_by_path path ctx.env

let module_type_lookup ctx path =
  Environment.find_module_type_by_path path ctx.env

let add_value name id scheme loc ctx =
  let env = Environment.add_value name id scheme loc ctx.env in
  { ctx with env }

let add_type name decl ctx =
  let env = Environment.add_type name decl ctx.env in
  { ctx with env }

let add_module name binding ctx =
  let env = Environment.add_module name binding ctx.env in
  { ctx with env }

let add_module_type name mty_opt ctx =
  let env = Environment.add_module_type name mty_opt ctx.env in
  { ctx with env }

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
    let instantiated_type = Type_scheme.instantiate ~fresh_var scheme in
    (instantiated_type, !ctx_ref)
  end

