(** Shared constructor type processing for variant declarations. *)

open Parsing.Syntax_tree

let check_constructor ctx param_names type_params result_type type_name tag_index
    (ctor : constructor_declaration) =
  let arg_type, ctor_result_type, is_gadt, ctor_type_params, existentials, ctx =
    match ctor.constructor_return_type with
    | Some ret_ty_expr ->
        let arg_ty, ret_ty, _gadt_params, ctx =
          Type_expression_check.check_gadt_constructor ctx param_names type_params
            ctor.constructor_argument ret_ty_expr
        in
        let return_type_vars = Type_traversal.free_type_variables ret_ty in
        let argument_type_vars = match arg_ty with
          | Some arg_ty_val -> Type_traversal.free_type_variables arg_ty_val
          | None -> []
        in

        let gadt_params = Gadt.compute_gadt_constructor_params
          ~return_type_vars ~argument_type_vars in
        (arg_ty, ret_ty, true,
         gadt_params.constructor_type_params,
         gadt_params.existentials, ctx)

    | None ->
        let arg_type, ctx = match ctor.constructor_argument with
          | Some ty_expr ->
              let ty, ctx = Type_expression_check.check_type_expression_with_params
                ctx param_names type_params ty_expr in
              (Some ty, ctx)
          | None -> (None, ctx)
        in
        (arg_type, result_type, false, type_params, [], ctx)
  in

  let ctor_info = Types.{
    constructor_name = ctor.constructor_name.Common.Location.value;
    constructor_tag_index = tag_index;
    constructor_type_name = type_name;
    constructor_argument_type = arg_type;
    constructor_result_type = ctor_result_type;
    constructor_type_parameters = ctor_type_params;
    constructor_is_gadt = is_gadt;
    constructor_existentials = existentials;
  } in
  (ctor_info, ctx)

let check_constructors ctx param_names type_params result_type type_name constructors =
  let indexed_constructors = List.mapi (fun index ctor -> (index, ctor)) constructors in

  let rev_constructor_infos, ctx =
    List.fold_left (fun (rev_infos, ctx) (tag_index, ctor) ->
      let ctor_info, ctx =
        check_constructor ctx param_names type_params result_type type_name tag_index ctor
      in
      (ctor_info :: rev_infos, ctx)
    ) ([], ctx) indexed_constructors
  in

  (List.rev rev_constructor_infos, ctx)
