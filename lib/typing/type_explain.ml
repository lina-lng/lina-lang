type pretty_context = {
  mutable next_index : int;
  mapping : (int, int) Hashtbl.t;
}

let create_context () = {
  next_index = 0;
  mapping = Hashtbl.create 16;
}

let letter_of_int index =
  if index < 26 then
    String.make 1 (Char.chr (Char.code 'a' + index))
  else
    let base = index mod 26 in
    let suffix = index / 26 in
    String.make 1 (Char.chr (Char.code 'a' + base)) ^ string_of_int suffix

let get_var_name ctx var_id =
  match Hashtbl.find_opt ctx.mapping var_id with
  | Some index -> letter_of_int index
  | None ->
    let index = ctx.next_index in
    ctx.next_index <- ctx.next_index + 1;
    Hashtbl.add ctx.mapping var_id index;
    letter_of_int index

(** Precedence-based type printing with minimal parentheses.
    Mirrors the implementation in Types.pp_type_expression. *)

let rec pretty_type_top ctx ty =
  match Types.representative ty with
  | Types.TypeArrow (_, arg, result) ->
    Printf.sprintf "%s -> %s"
      (pretty_type_arrow_arg ctx arg)
      (pretty_type_top ctx result)
  | _ -> pretty_type_tuple ctx ty

and pretty_type_arrow_arg ctx ty =
  match Types.representative ty with
  | Types.TypeArrow _ -> Printf.sprintf "(%s)" (pretty_type_top ctx ty)
  | _ -> pretty_type_tuple ctx ty

and pretty_type_tuple ctx ty =
  match Types.representative ty with
  | Types.TypeTuple elements ->
    elements
    |> List.map (pretty_type_simple ctx)
    |> String.concat " * "
  | _ -> pretty_type_app ctx ty

and pretty_type_app ctx ty =
  match Types.representative ty with
  | Types.TypeConstructor (path, [arg]) ->
    Printf.sprintf "%s %s" (pretty_type_simple ctx arg) (Types.path_to_string path)
  | Types.TypeConstructor (path, args) when args <> [] ->
    let args_str = args |> List.map (pretty_type_top ctx) |> String.concat ", " in
    Printf.sprintf "(%s) %s" args_str (Types.path_to_string path)
  | _ -> pretty_type_simple ctx ty

and pretty_type_simple ctx ty =
  match Types.representative ty with
  | Types.TypeVariable tv ->
    let prefix = if tv.Types.weak then "'_" else "'" in
    prefix ^ get_var_name ctx tv.Types.id
  | Types.TypeConstructor (path, []) ->
    Types.path_to_string path
  | Types.TypeRecord row ->
    pretty_row_impl ctx row
  | Types.TypePolyVariant pv_row ->
    pretty_poly_variant_impl ctx pv_row
  | Types.TypePackage pkg ->
    Printf.sprintf "(module %s)" (Types.path_to_string pkg.Types.package_path)
  | Types.TypeRowEmpty ->
    "{}"
  | Types.TypeArrow _ | Types.TypeTuple _ | Types.TypeConstructor _ ->
    Printf.sprintf "(%s)" (pretty_type_top ctx ty)

and format_row_tail ~separator ~row_more ~has_fields =
  match Types.representative row_more with
  | Types.TypeRowEmpty -> ""
  | _ -> if has_fields then separator ^ ".." else ".."

and pretty_row_impl ctx row =
  let field_strs =
    row.Types.row_fields
    |> List.map (fun (name, field) ->
         match field with
         | Types.RowFieldPresent ty ->
           Printf.sprintf "%s: %s" name (pretty_type_top ctx ty))
  in
  let row_tail = format_row_tail
    ~separator:"; "
    ~row_more:row.Types.row_more
    ~has_fields:(field_strs <> [])
  in
  Printf.sprintf "{ %s%s }" (String.concat "; " field_strs) row_tail

and pretty_poly_variant_impl ctx pv_row =
  let open_marker = if pv_row.Types.pv_closed then "" else "> " in
  let field_strs =
    pv_row.Types.pv_fields
    |> List.filter_map (fun (tag, field) ->
         match field with
         | Types.PVFieldPresent None -> Some (Printf.sprintf "`%s" tag)
         | Types.PVFieldPresent (Some ty) ->
           Some (Printf.sprintf "`%s of %s" tag (pretty_type_top ctx ty))
         | Types.PVFieldAbsent -> None)
  in
  let row_tail = format_row_tail
    ~separator:" | "
    ~row_more:pv_row.Types.pv_more
    ~has_fields:(field_strs <> [])
  in
  Printf.sprintf "[%s%s%s]" open_marker (String.concat " | " field_strs) row_tail

let pretty_type ty =
  let ctx = create_context () in
  pretty_type_top ctx ty

let pretty_type_in_context = pretty_type_top

let starts_with_vowel str =
  match String.lowercase_ascii str with
  | "" -> false
  | s -> match s.[0] with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

let article_for word =
  if starts_with_vowel word then "an" else "a"

let is_constructor_named name ty =
  match Types.representative ty with
  | Types.TypeConstructor (Types.PathLocal n, _) -> String.equal n name
  | Types.TypeConstructor (Types.PathIdent id, _) ->
    String.equal (Common.Identifier.name id) name
  | _ -> false

let builtin_machine_name ty =
  match Types.representative ty with
  | Types.TypeConstructor (Types.PathBuiltin builtin, []) ->
    begin match builtin with
    | Types.BuiltinInt -> Some "int"
    | Types.BuiltinFloat -> Some "float"
    | Types.BuiltinString -> Some "string"
    | Types.BuiltinBool -> Some "bool"
    | _ -> None
    end
  | _ -> None

let split_last list =
  let rec aux acc = function
    | [] -> None
    | [x] -> Some (List.rev acc, x)
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] list

let builtin_name = function
  | Types.BuiltinInt -> "integer"
  | Types.BuiltinFloat -> "float"
  | Types.BuiltinString -> "string"
  | Types.BuiltinBool -> "boolean"
  | Types.BuiltinUnit -> "unit value"
  | Types.BuiltinRef -> "reference"
  | Types.BuiltinArray -> "array"
  | Types.BuiltinDict -> "dictionary"
  | Types.BuiltinSet -> "set"

let explain_short ty =
  match Types.representative ty with
  | Types.TypeVariable _ -> "value"

  | Types.TypeConstructor (Types.PathBuiltin builtin, []) ->
    builtin_name builtin
  | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinRef, [_]) ->
    "reference"

  | Types.TypeConstructor (_, [_]) when is_constructor_named "option" ty ->
    "optional value"
  | Types.TypeConstructor (_, [_]) when is_constructor_named "list" ty ->
    "list"

  | Types.TypeConstructor (path, _) ->
    Types.path_to_string path

  | Types.TypeTuple elements ->
    Printf.sprintf "tuple of %d elements" (List.length elements)
  | Types.TypeArrow _ ->
    "function"

  | Types.TypeRecord row ->
    let field_names = List.map fst row.Types.row_fields in
    begin match field_names with
    | [] -> "record"
    | [single] -> Printf.sprintf "record with field %s" single
    | _ ->
      match split_last field_names with
      | Some (init, last) ->
        Printf.sprintf "record with fields %s and %s"
          (String.concat ", " init) last
      | None -> "record"
    end

  | Types.TypePolyVariant _ -> "variant"
  | Types.TypePackage pkg ->
    Printf.sprintf "module of type %s" (Types.path_to_string pkg.Types.package_path)
  | Types.TypeRowEmpty -> "empty record"

let explain ty =
  let short = explain_short ty in
  Printf.sprintf "%s %s" (article_for short) short

let is_function ty =
  match Types.representative ty with
  | Types.TypeArrow _ -> true
  | _ -> false

let is_option ty =
  match Types.representative ty with
  | Types.TypeConstructor (_, [_]) -> is_constructor_named "option" ty
  | _ -> false

let option_content ty =
  match Types.representative ty with
  | Types.TypeConstructor (_, [content]) when is_constructor_named "option" ty ->
    Some content
  | _ -> None

let function_result ty =
  let rec get_result t =
    match Types.representative t with
    | Types.TypeArrow (_, _, result) ->
      begin match Types.representative result with
      | Types.TypeArrow _ -> get_result result
      | _ -> Some result
      end
    | _ -> None
  in
  get_result ty

let is_numeric ty =
  match Types.representative ty with
  | Types.TypeConstructor (Types.PathBuiltin (Types.BuiltinInt | Types.BuiltinFloat), []) ->
    true
  | _ -> false

let explain_mismatch ~expected ~actual =
  let ctx = create_context () in
  let expected_pretty = pretty_type_top ctx expected in
  let actual_pretty = pretty_type_top ctx actual in
  let expected_explain = explain expected in
  let actual_explain = explain actual in

  let hint =
    if is_function actual && not (is_function expected) then
      match function_result actual with
      | Some result_ty when Types.type_expression_to_string result_ty =
                            Types.type_expression_to_string expected ->
        Some "This is a function. Did you forget to apply it to an argument?"
      | _ ->
        Some "This is a function. Did you forget to apply it?"

    else if is_option expected && not (is_option actual) then
      match option_content expected with
      | Some content_ty when Types.type_expression_to_string content_ty =
                             Types.type_expression_to_string actual ->
        Some "The expected type is optional. Try wrapping with `Some`."
      | _ -> None

    else if not (is_option expected) && is_option actual then
      match option_content actual with
      | Some content_ty when Types.type_expression_to_string content_ty =
                             Types.type_expression_to_string expected ->
        Some "This is an optional value. Consider using pattern matching or `Option.get`."
      | _ -> None

    else if is_numeric expected && is_numeric actual then
      let expected_name = explain_short expected in
      let actual_name = explain_short actual in
      Some (Printf.sprintf "Use `%s_of_%s` to convert between numeric types."
              expected_name actual_name)

    else
      None
  in

  (Printf.sprintf "%s (%s)" expected_explain expected_pretty,
   Printf.sprintf "%s (%s)" actual_explain actual_pretty,
   hint)
