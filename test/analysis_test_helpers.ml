(** Shared test helpers for analysis module tests. *)

module Warning_config = Common.Warning_config
module Compiler_error = Common.Compiler_error
module Error_code = Common.Error_code

let diverging_prelude =
  {|
external raise : string -> 'a = "error"
external failwith : string -> 'a = "error"
external exit : int -> 'a = "os.exit"
external invalid_arg : string -> 'a = "error"
|}

let type_check source =
  Typing.Types.reset_type_variable_id ();
  let ast = Parsing.Parse.structure_from_string source in
  let ctx = Typing.Typing_context.create Typing.Environment.initial in
  Typing.Inference.infer_structure ctx ast |> fst

let type_check_with_prelude source =
  type_check (diverging_prelude ^ "\n" ^ source)

let string_starts_with ~prefix str =
  let len = String.length prefix in
  String.length str >= len && String.sub str 0 len = prefix

let string_contains ~substring str =
  let len_sub = String.length substring in
  let len_str = String.length str in
  if len_sub > len_str then false
  else
    let rec check index =
      index <= len_str - len_sub
      && (String.sub str index len_sub = substring || check (index + 1))
    in
    check 0

let find_binding_in_tree tree name =
  List.find_opt
    (fun b -> b.Analysis.Scope.bind_name = name)
    tree.Analysis.Scope.tree_all_bindings

let diagnostic_messages diags =
  List.map (fun (d : Compiler_error.diagnostic) -> d.message) diags

let has_diagnostic_code code diags =
  List.exists
    (fun (d : Compiler_error.diagnostic) ->
      match d.code with Some c -> c = code | None -> false)
    diags
