(** Semantic token support for the LSP server.

    This module provides semantic token highlighting for the Lina language,
    enabling editors to display type-aware syntax highlighting. *)

(** Semantic token types following LSP specification. *)
type token_type =
  | TokenNamespace     (** Module names *)
  | TokenType          (** Type names: int, option *)
  | TokenTypeParameter (** Type variables: 'a, 'b *)
  | TokenParameter     (** Function parameters *)
  | TokenVariable      (** Let-bound values *)
  | TokenProperty      (** Record fields *)
  | TokenEnumMember    (** Constructors: Some, None *)
  | TokenFunction      (** Functions *)
  | TokenKeyword       (** Keywords: let, match, if *)
  | TokenString        (** String literals *)
  | TokenNumber        (** Numeric literals *)
  | TokenOperator      (** Operators: +, -, * *)

(** Token modifiers for additional semantics. *)
type token_modifier =
  | ModDeclaration  (** Definition site *)
  | ModReadonly     (** Immutable binding *)
  | ModStatic       (** Module-level binding *)

(** A single semantic token with position and classification. *)
type semantic_token = {
  line : int;
  start_char : int;
  length : int;
  token_type : token_type;
  modifiers : token_modifier list;
}

(** Token type names for LSP legend (order must match token_type variants). *)
let token_type_names = [
  "namespace";
  "type";
  "typeParameter";
  "parameter";
  "variable";
  "property";
  "enumMember";
  "function";
  "keyword";
  "string";
  "number";
  "operator";
]

(** Token modifier names for LSP legend (order must match token_modifier variants). *)
let token_modifier_names = [
  "declaration";
  "readonly";
  "static";
]

(** Convert token type to index. *)
let token_type_to_index = function
  | TokenNamespace -> 0
  | TokenType -> 1
  | TokenTypeParameter -> 2
  | TokenParameter -> 3
  | TokenVariable -> 4
  | TokenProperty -> 5
  | TokenEnumMember -> 6
  | TokenFunction -> 7
  | TokenKeyword -> 8
  | TokenString -> 9
  | TokenNumber -> 10
  | TokenOperator -> 11

(** Convert token modifier to bit index. *)
let token_modifier_to_bit = function
  | ModDeclaration -> 0
  | ModReadonly -> 1
  | ModStatic -> 2

(** Convert modifiers list to bitset. *)
let modifiers_to_bitset modifiers =
  List.fold_left (fun bits modifier ->
    bits lor (1 lsl (token_modifier_to_bit modifier))
  ) 0 modifiers

(** Create a token from a location and classification. *)
let make_token loc token_type modifiers =
  (* Convert from 1-indexed Lina locations to 0-indexed LSP positions *)
  let line = loc.Common.Location.start_pos.line - 1 in
  let start_char = loc.Common.Location.start_pos.column in
  let length = loc.Common.Location.end_pos.offset - loc.Common.Location.start_pos.offset in
  if line >= 0 && length > 0 then
    Some { line; start_char; length; token_type; modifiers }
  else
    None

(** Determine if a type is a function type. *)
let is_function_type ty =
  match Typing.Types.representative ty with
  | Typing.Types.TypeArrow _ -> true
  | _ -> false

(** Collect tokens from a typed pattern. *)
let rec collect_from_pattern ~is_parameter tokens (pat : Typing.Typed_tree.typed_pattern) =
  let loc = pat.pattern_location in
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable _id ->
      (* Variable pattern - parameter or local binding *)
      let token_type = if is_parameter then TokenParameter else TokenVariable in
      let modifiers = [ModDeclaration; ModReadonly] in
      begin match make_token loc token_type modifiers with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedPatternWildcard ->
      tokens  (* No token for wildcard *)

  | Typing.Typed_tree.TypedPatternConstant const ->
      let token_type = match const with
        | Parsing.Syntax_tree.ConstantInteger _ -> TokenNumber
        | Parsing.Syntax_tree.ConstantFloat _ -> TokenNumber
        | Parsing.Syntax_tree.ConstantString _ -> TokenString
        | Parsing.Syntax_tree.ConstantBoolean _ -> TokenKeyword
        | Parsing.Syntax_tree.ConstantUnit -> TokenKeyword
      in
      begin match make_token loc token_type [] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedPatternTuple pats ->
      List.fold_left (collect_from_pattern ~is_parameter) tokens pats

  | Typing.Typed_tree.TypedPatternConstructor (ctor_info, arg_opt) ->
      (* Constructor name as enum member *)
      let ctor_name = ctor_info.Typing.Types.constructor_name in
      let ctor_loc = {
        Common.Location.start_pos = loc.start_pos;
        end_pos = {
          loc.start_pos with
          offset = loc.start_pos.offset + String.length ctor_name;
          column = loc.start_pos.column + String.length ctor_name;
        };
      } in
      let tokens = match make_token ctor_loc TokenEnumMember [] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      begin match arg_opt with
      | Some arg -> collect_from_pattern ~is_parameter tokens arg
      | None -> tokens
      end

  | Typing.Typed_tree.TypedPatternRecord (fields, _is_open) ->
      List.fold_left (fun tokens (field : Typing.Typed_tree.typed_record_pattern_field) ->
        (* Field name as property *)
        let field_tokens = match make_token pat.pattern_location TokenProperty [] with
          | Some _ -> tokens  (* TODO: precise field name location *)
          | None -> tokens
        in
        collect_from_pattern ~is_parameter field_tokens field.typed_pattern_field_pattern
      ) tokens fields

  | Typing.Typed_tree.TypedPatternLocallyAbstract (_id, _type_decl) ->
      (* Locally abstract type - type name as type token *)
      begin match make_token loc TokenType [] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedPatternPolyVariant (tag, arg_opt) ->
      (* Poly variant tag as enum member *)
      let tag_loc = {
        Common.Location.start_pos = loc.start_pos;
        end_pos = {
          loc.start_pos with
          offset = loc.start_pos.offset + String.length tag + 1;  (* +1 for backtick *)
          column = loc.start_pos.column + String.length tag + 1;
        };
      } in
      let tokens = match make_token tag_loc TokenEnumMember [] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      begin match arg_opt with
      | Some arg -> collect_from_pattern ~is_parameter tokens arg
      | None -> tokens
      end

  | Typing.Typed_tree.TypedPatternAlias (inner, _id) ->
      (* Alias pattern: collect from inner pattern, alias variable gets a token *)
      let tokens = match make_token loc TokenVariable [ModDeclaration; ModReadonly] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      collect_from_pattern ~is_parameter tokens inner

  | Typing.Typed_tree.TypedPatternOr (left, right) ->
      (* Or-pattern: collect from both branches *)
      let tokens = collect_from_pattern ~is_parameter tokens left in
      collect_from_pattern ~is_parameter tokens right

  | Typing.Typed_tree.TypedPatternError _ ->
      (* Error patterns don't produce semantic tokens *)
      tokens

(** Collect tokens from a typed expression. *)
let rec collect_from_expression tokens (expr : Typing.Typed_tree.typed_expression) =
  let loc = expr.expression_location in
  match expr.expression_desc with
  | Typing.Typed_tree.TypedExpressionVariable (_id, _def_loc) ->
      (* Variable reference - classify based on type *)
      let token_type =
        if is_function_type expr.expression_type then TokenFunction
        else TokenVariable
      in
      begin match make_token loc token_type [ModReadonly] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionConstant const ->
      let token_type = match const with
        | Parsing.Syntax_tree.ConstantInteger _ -> TokenNumber
        | Parsing.Syntax_tree.ConstantFloat _ -> TokenNumber
        | Parsing.Syntax_tree.ConstantString _ -> TokenString
        | Parsing.Syntax_tree.ConstantBoolean _ -> TokenKeyword
        | Parsing.Syntax_tree.ConstantUnit -> TokenKeyword
      in
      begin match make_token loc token_type [] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionTuple exprs ->
      List.fold_left collect_from_expression tokens exprs

  | Typing.Typed_tree.TypedExpressionConstructor (ctor_info, arg_opt) ->
      (* Constructor as enum member *)
      let ctor_name = ctor_info.Typing.Types.constructor_name in
      let ctor_loc = {
        Common.Location.start_pos = loc.start_pos;
        end_pos = {
          loc.start_pos with
          offset = loc.start_pos.offset + String.length ctor_name;
          column = loc.start_pos.column + String.length ctor_name;
        };
      } in
      let tokens = match make_token ctor_loc TokenEnumMember [] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      begin match arg_opt with
      | Some arg -> collect_from_expression tokens arg
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionApply (func, labeled_args) ->
      let tokens = collect_from_expression tokens func in
      List.fold_left (fun toks (_, e) -> collect_from_expression toks e) tokens labeled_args

  | Typing.Typed_tree.TypedExpressionPartialApply { partial_func; partial_slots } ->
      let tokens = collect_from_expression tokens partial_func in
      List.fold_left (fun toks (_, slot) ->
        match slot with
        | Typing.Typed_tree.SlotFilled expr -> collect_from_expression toks expr
        | Typing.Typed_tree.SlotNeeded _ -> toks
      ) tokens partial_slots

  | Typing.Typed_tree.TypedExpressionFunction (labeled_params, body) ->
      (* Function parameters *)
      let tokens = List.fold_left (fun toks (_, p) ->
        collect_from_pattern ~is_parameter:true toks p
      ) tokens labeled_params in
      collect_from_expression tokens body

  | Typing.Typed_tree.TypedExpressionLet (_rec_flag, bindings, body) ->
      let tokens = List.fold_left collect_from_binding tokens bindings in
      collect_from_expression tokens body

  | Typing.Typed_tree.TypedExpressionIf (cond, then_expr, else_opt) ->
      let tokens = collect_from_expression tokens cond in
      let tokens = collect_from_expression tokens then_expr in
      begin match else_opt with
      | Some else_expr -> collect_from_expression tokens else_expr
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionSequence (expr1, expr2) ->
      let tokens = collect_from_expression tokens expr1 in
      collect_from_expression tokens expr2

  | Typing.Typed_tree.TypedExpressionRecord fields ->
      List.fold_left (fun tokens (field : Typing.Typed_tree.typed_record_field) ->
        collect_from_expression tokens field.typed_field_value
      ) tokens fields

  | Typing.Typed_tree.TypedExpressionRecordAccess (record_expr, _field_name) ->
      (* TODO: highlight field name as property *)
      collect_from_expression tokens record_expr

  | Typing.Typed_tree.TypedExpressionRecordUpdate (record_expr, fields) ->
      let tokens = collect_from_expression tokens record_expr in
      List.fold_left (fun tokens (field : Typing.Typed_tree.typed_record_field) ->
        collect_from_expression tokens field.typed_field_value
      ) tokens fields

  | Typing.Typed_tree.TypedExpressionMatch (scrutinee, arms) ->
      let tokens = collect_from_expression tokens scrutinee in
      List.fold_left (fun tokens (arm : Typing.Typed_tree.typed_match_arm) ->
        let tokens = collect_from_pattern ~is_parameter:false tokens arm.typed_arm_pattern in
        let tokens = match arm.typed_arm_guard with
          | Some guard -> collect_from_expression tokens guard
          | None -> tokens
        in
        collect_from_expression tokens arm.typed_arm_expression
      ) tokens arms

  | Typing.Typed_tree.TypedExpressionModuleAccess (_path, _name) ->
      (* Module access - mark the whole thing as namespace for now *)
      begin match make_token loc TokenNamespace [] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionRef inner ->
      collect_from_expression tokens inner

  | Typing.Typed_tree.TypedExpressionDeref inner ->
      collect_from_expression tokens inner

  | Typing.Typed_tree.TypedExpressionAssign (ref_expr, value_expr) ->
      let tokens = collect_from_expression tokens ref_expr in
      collect_from_expression tokens value_expr

  | Typing.Typed_tree.TypedExpressionPolyVariant (tag, arg_opt) ->
      (* Poly variant tag as enum member *)
      let tag_loc = {
        Common.Location.start_pos = loc.start_pos;
        end_pos = {
          loc.start_pos with
          offset = loc.start_pos.offset + String.length tag + 1;  (* +1 for backtick *)
          column = loc.start_pos.column + String.length tag + 1;
        };
      } in
      let tokens = match make_token tag_loc TokenEnumMember [] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      begin match arg_opt with
      | Some arg -> collect_from_expression tokens arg
      | None -> tokens
      end

  | Typing.Typed_tree.TypedExpressionPack _ ->
      (* First-class module pack - not yet fully implemented *)
      tokens

  | Typing.Typed_tree.TypedExpressionLetModule _ ->
      tokens

  | Typing.Typed_tree.TypedExpressionAssert inner ->
      collect_from_expression tokens inner

  | Typing.Typed_tree.TypedExpressionWhile (cond, body) ->
      let tokens = collect_from_expression tokens cond in
      collect_from_expression tokens body

  | Typing.Typed_tree.TypedExpressionFor (_, start_e, end_e, _, body) ->
      let tokens = collect_from_expression tokens start_e in
      let tokens = collect_from_expression tokens end_e in
      collect_from_expression tokens body

  | Typing.Typed_tree.TypedExpressionError _ ->
      tokens

(** Collect tokens from a binding. *)
and collect_from_binding tokens (binding : Typing.Typed_tree.typed_binding) =
  (* Pattern defines the binding - mark as declaration *)
  let tokens = collect_from_pattern ~is_parameter:false tokens binding.binding_pattern in
  collect_from_expression tokens binding.binding_expression

(** Collect tokens from a structure item. *)
and collect_from_structure_item tokens (item : Typing.Typed_tree.typed_structure_item) =
  match item.structure_item_desc with
  | Typing.Typed_tree.TypedStructureValue (_rec_flag, bindings) ->
      (* For top-level bindings, check if they're functions and mark static *)
      List.fold_left (fun tokens (binding : Typing.Typed_tree.typed_binding) ->
        (* Collect from pattern with static modifier for top-level *)
        let pat_tokens = collect_from_pattern ~is_parameter:false [] binding.binding_pattern in
        let pat_tokens = List.map (fun tok ->
          { tok with modifiers = ModStatic :: tok.modifiers }
        ) pat_tokens in
        let tokens = pat_tokens @ tokens in
        collect_from_expression tokens binding.binding_expression
      ) tokens bindings

  | Typing.Typed_tree.TypedStructureType _type_decls ->
      (* TODO: collect tokens from type declarations *)
      tokens

  | Typing.Typed_tree.TypedStructureModule (id, mod_expr) ->
      (* Module name as namespace *)
      let mod_name = Common.Identifier.name id in
      let mod_loc = {
        Common.Location.start_pos = item.structure_item_location.start_pos;
        end_pos = {
          item.structure_item_location.start_pos with
          offset = item.structure_item_location.start_pos.offset + String.length mod_name;
          column = item.structure_item_location.start_pos.column + String.length mod_name;
        };
      } in
      let tokens = match make_token mod_loc TokenNamespace [ModDeclaration; ModStatic] with
        | Some tok -> tok :: tokens
        | None -> tokens
      in
      collect_from_module_expr tokens mod_expr

  | Typing.Typed_tree.TypedStructureModuleType (_name, _mod_type) ->
      (* TODO: collect tokens from module type *)
      tokens

  | Typing.Typed_tree.TypedStructureOpen (_path, _bindings) ->
      (* TODO: collect tokens from open path *)
      tokens

  | Typing.Typed_tree.TypedStructureInclude (mod_expr, _bindings) ->
      collect_from_module_expr tokens mod_expr

  | Typing.Typed_tree.TypedStructureExternal ext ->
      (* External name as function *)
      let ext_loc = ext.external_location in
      begin match make_token ext_loc TokenFunction [ModDeclaration; ModStatic] with
      | Some tok -> tok :: tokens
      | None -> tokens
      end

  | Typing.Typed_tree.TypedStructureRecModule rec_bindings ->
      (* Collect tokens from each recursive module binding *)
      List.fold_left (fun tokens (binding : Typing.Typed_tree.typed_rec_module_binding) ->
        let mod_name = Common.Identifier.name binding.rec_module_id in
        let mod_loc = {
          Common.Location.start_pos = binding.rec_module_location.start_pos;
          end_pos = {
            binding.rec_module_location.start_pos with
            offset = binding.rec_module_location.start_pos.offset + String.length mod_name;
            column = binding.rec_module_location.start_pos.column + String.length mod_name;
          };
        } in
        let tokens = match make_token mod_loc TokenNamespace [ModDeclaration; ModStatic] with
          | Some tok -> tok :: tokens
          | None -> tokens
        in
        collect_from_module_expr tokens binding.rec_module_expr
      ) tokens rec_bindings

  | Typing.Typed_tree.TypedStructureTypeExtension ext ->
      (* Type extension: highlight constructor names *)
      List.fold_left (fun tokens ctor ->
        let ctor_name = ctor.Typing.Types.constructor_name in
        let ctor_loc = {
          Common.Location.start_pos = ext.extension_location.start_pos;
          end_pos = {
            ext.extension_location.start_pos with
            offset = ext.extension_location.start_pos.offset + String.length ctor_name;
            column = ext.extension_location.start_pos.column + String.length ctor_name;
          };
        } in
        match make_token ctor_loc TokenEnumMember [ModDeclaration] with
        | Some tok -> tok :: tokens
        | None -> tokens
      ) tokens ext.extension_constructors

  | Typing.Typed_tree.TypedStructureExpression expr ->
      (* Top-level expressions can have semantic tokens in subexpressions *)
      collect_from_expression tokens expr

  | Typing.Typed_tree.TypedStructureError _ ->
      (* Error structure items don't produce semantic tokens *)
      tokens

(** Collect tokens from a module expression. *)
and collect_from_module_expr tokens (mod_expr : Typing.Typed_tree.typed_module_expression) =
  match mod_expr.module_desc with
  | Typing.Typed_tree.TypedModuleStructure structure ->
      List.fold_left collect_from_structure_item tokens structure
  | Typing.Typed_tree.TypedModulePath _path ->
      tokens
  | Typing.Typed_tree.TypedModuleFunctor (_param, body) ->
      collect_from_module_expr tokens body
  | Typing.Typed_tree.TypedModuleApply (func, arg) ->
      let tokens = collect_from_module_expr tokens func in
      collect_from_module_expr tokens arg
  | Typing.Typed_tree.TypedModuleConstraint (inner, _mty) ->
      collect_from_module_expr tokens inner
  | Typing.Typed_tree.TypedModuleUnpack _ ->
      (* First-class module unpack - not yet fully implemented *)
      tokens

(** Compare tokens by position for sorting. *)
let compare_tokens tok1 tok2 =
  let line_cmp = compare tok1.line tok2.line in
  if line_cmp <> 0 then line_cmp
  else compare tok1.start_char tok2.start_char

(** Filter overlapping tokens, keeping the shortest one at each position.
    When binary operators like + have their location assigned to the whole
    expression, this ensures we keep only the precise token. *)
let filter_overlapping_tokens tokens =
  let sorted = List.sort (fun tok1 tok2 ->
    let pos_cmp = compare_tokens tok1 tok2 in
    if pos_cmp <> 0 then pos_cmp
    else compare tok1.length tok2.length  (* Shorter first *)
  ) tokens in
  let rec dedup prev_opt = function
    | [] -> []
    | tok :: rest ->
        match prev_opt with
        | Some prev when prev.line = tok.line && prev.start_char = tok.start_char ->
            (* Skip token at same position - we already have a shorter one *)
            dedup prev_opt rest
        | _ ->
            tok :: dedup (Some tok) rest
  in
  dedup None sorted

(** Collect all semantic tokens from a typed structure. *)
let collect_tokens structure =
  let tokens = List.fold_left collect_from_structure_item [] structure in
  filter_overlapping_tokens tokens

(** Encode tokens in LSP delta format. *)
let encode_tokens tokens =
  let sorted = List.sort compare_tokens tokens in
  let prev_line = ref 0 in
  let prev_char = ref 0 in
  List.concat_map (fun tok ->
    let delta_line = tok.line - !prev_line in
    let delta_char =
      if delta_line = 0 then tok.start_char - !prev_char
      else tok.start_char
    in
    prev_line := tok.line;
    prev_char := tok.start_char;
    let type_index = token_type_to_index tok.token_type in
    let modifier_bits = modifiers_to_bitset tok.modifiers in
    [delta_line; delta_char; tok.length; type_index; modifier_bits]
  ) sorted
