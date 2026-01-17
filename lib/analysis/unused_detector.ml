open Common

type result = {
  diagnostics : Compiler_error.diagnostic list;
  has_errors : bool;
}

module LocationMap = Map.Make (struct
  type t = Location.t

  let compare a b =
    let c1 = compare a.Location.start_pos.offset b.Location.start_pos.offset in
    if c1 <> 0 then c1 else compare a.end_pos.offset b.end_pos.offset
end)

let warn_unless condition code = if condition then None else Some code

let detect_in_binding (binding : Scope.binding) =
  if Scope.is_intentionally_unused binding then None
  else if binding.bind_exported then None
  else
    match binding.bind_kind with
    | Scope.Variable ->
        warn_unless (Scope.has_read_references binding) Error_code.w_unused_variable

    | Scope.Function { is_recursive } ->
        let ref_count = Scope.reference_count binding in
        if ref_count = 0 then Some Error_code.w_unused_function
        else if is_recursive then
          let has_external_refs =
            List.exists
              (fun (r : Scope.reference) ->
                r.ref_location.start_pos.offset
                <> binding.bind_location.start_pos.offset)
              binding.bind_references
          in
          warn_unless has_external_refs Error_code.w_unused_rec
        else None

    | Scope.Parameter ->
        warn_unless (Scope.has_read_references binding) Error_code.w_unused_parameter

    | Scope.PatternVar ->
        warn_unless (Scope.has_read_references binding) Error_code.w_unused_variable

    | Scope.ModuleAlias ->
        warn_unless (Scope.reference_count binding > 0) Error_code.w_unused_module

    | Scope.TypeName ->
        warn_unless (Scope.reference_count binding > 0) Error_code.w_unused_type

    | Scope.Constructor ->
        let has_usage =
          Scope.has_pattern_references binding
          || Scope.has_construct_references binding
        in
        warn_unless has_usage Error_code.w_unused_constructor

    | Scope.RecordField ->
        warn_unless (Scope.has_project_references binding) Error_code.w_unused_field

    | Scope.OpenedBinding | Scope.IncludedBinding | Scope.External -> None

let binding_kind_label = function
  | Scope.Variable | Scope.PatternVar -> "variable"
  | Scope.Function _ -> "function"
  | Scope.Parameter -> "parameter"
  | Scope.ModuleAlias -> "module"
  | Scope.TypeName -> "type"
  | Scope.Constructor -> "constructor"
  | Scope.RecordField -> "field"
  | Scope.OpenedBinding -> "binding"
  | Scope.IncludedBinding -> "binding"
  | Scope.External -> "external"

let binding_origin_suffix = function
  | Scope.OpenedBinding -> " from open"
  | Scope.IncludedBinding -> " from include"
  | _ -> ""

let make_diagnostic config binding code =
  let severity = Warning_config.severity_for config code in

  let message =
    Printf.sprintf "unused %s `%s`%s"
      (binding_kind_label binding.Scope.bind_kind)
      binding.bind_name
      (binding_origin_suffix binding.bind_kind)
  in

  let suggestion =
    if String.length binding.bind_name > 0 && binding.bind_name.[0] <> '_' then
      let suggested_name = "_" ^ binding.bind_name in
      Some
        {
          Compiler_error.suggestion_message =
            Printf.sprintf "prefix with underscore if intentional: `%s`"
              suggested_name;
          replacement = suggested_name;
          suggestion_span = binding.bind_location;
          applicability = Compiler_error.MachineApplicable;
        }
    else None
  in

  let diag =
    Compiler_error.make_diagnostic ~severity ~message ~code
      ~labels:
        [
          {
            label_span = binding.bind_location;
            label_message = Some "defined here but never used";
            is_primary = true;
          };
        ]
      ()
  in

  match suggestion with
  | Some s ->
      Compiler_error.with_suggestion ~span:s.suggestion_span
        ~message:s.suggestion_message ~replacement:s.replacement
        ~applicability:s.applicability diag
  | None -> diag

let detect_unused_opens config scope_tree =
  let open_groups =
    List.fold_left
      (fun acc (binding : Scope.binding) ->
        match binding.bind_kind with
        | Scope.OpenedBinding ->
            let loc = binding.bind_location in
            let existing =
              LocationMap.find_opt loc acc |> Option.value ~default:[]
            in
            LocationMap.add loc (binding :: existing) acc
        | _ -> acc)
      LocationMap.empty scope_tree.Scope.tree_all_bindings
  in

  LocationMap.fold
    (fun loc bindings diagnostics ->
      let any_used =
        List.exists
          (fun (b : Scope.binding) -> Scope.reference_count b > 0)
          bindings
      in
      if any_used then diagnostics
      else
        let severity = Warning_config.severity_for config Error_code.w_unused_open in
        let diag =
          Compiler_error.make_diagnostic ~severity
            ~message:"unused open statement" ~code:Error_code.w_unused_open
            ~labels:
              [
                {
                  label_span = loc;
                  label_message = Some "no bindings from this open are used";
                  is_primary = true;
                };
              ]
            ()
        in
        diag :: diagnostics)
    open_groups []

let detect config scope_tree =
  let binding_diags =
    scope_tree.Scope.tree_all_bindings
    |> List.filter_map (fun binding ->
           match detect_in_binding binding with
           | Some code when Warning_config.should_report config code ->
               Some (make_diagnostic config binding code)
           | _ -> None)
  in

  let open_diags =
    if Warning_config.should_report config Error_code.w_unused_open then
      detect_unused_opens config scope_tree
    else []
  in

  let all_diagnostics = binding_diags @ open_diags in
  let has_errors =
    List.exists
      (fun d -> d.Compiler_error.severity = Compiler_error.Error)
      all_diagnostics
  in

  { diagnostics = all_diagnostics; has_errors }
