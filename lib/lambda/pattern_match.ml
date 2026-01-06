open Common

(* Occurrence: path from scrutinee to sub-value *)
type occurrence_step =
  | OccTupleField of int
  | OccConstructorArg
  | OccRecordField of string

type occurrence = occurrence_step list

(* Head constructor for switching - uses Parsing constant to avoid Lambda dependency *)
type head_constructor =
  | HCWildcard
  | HCConstant of Parsing.Syntax_tree.constant
  | HCTuple of int  (* arity *)
  | HCConstructor of string * int  (* name, tag_index *)
  | HCRecord of string list  (* field names *)

(* Clause in pattern matrix *)
type clause = {
  clause_id : int; [@warning "-69"]
  clause_patterns : Typing.Typed_tree.typed_pattern list;
  clause_guard : Typing.Typed_tree.typed_expression option;
  clause_action : Typing.Typed_tree.typed_expression;
  clause_bindings : (Identifier.t * occurrence) list;
}

(* Pattern matrix *)
type pattern_matrix = {
  occurrences : occurrence list;
  clauses : clause list;
}

(* Decision tree output *)
type decision_tree =
  | DTFail
  | DTLeaf of leaf_info
  | DTSwitch of switch_info
  | DTGuard of guard_info

and leaf_info = {
  leaf_bindings : (Identifier.t * occurrence) list;
  leaf_action : Typing.Typed_tree.typed_expression;
}

and switch_info = {
  switch_occurrence : occurrence;
  switch_cases : (head_constructor * decision_tree) list;
  switch_default : decision_tree option;
}

and guard_info = {
  guard_bindings : (Identifier.t * occurrence) list;
  guard_condition : Typing.Typed_tree.typed_expression;
  guard_then : decision_tree;
  guard_else : decision_tree;
}

(* Get head constructor from a typed pattern *)
let head_of_pattern (pat : Typing.Typed_tree.typed_pattern) : head_constructor =
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable _ -> HCWildcard
  | Typing.Typed_tree.TypedPatternWildcard -> HCWildcard
  | Typing.Typed_tree.TypedPatternConstant c ->
    HCConstant c
  | Typing.Typed_tree.TypedPatternTuple pats ->
    HCTuple (List.length pats)
  | Typing.Typed_tree.TypedPatternConstructor (ctor_info, _) ->
    HCConstructor (ctor_info.Typing.Types.constructor_name, ctor_info.Typing.Types.constructor_tag_index)
  | Typing.Typed_tree.TypedPatternRecord (fields, _) ->
    HCRecord (List.map (fun f -> f.Typing.Typed_tree.typed_pattern_field_name) fields)

(* Compare constants for equality *)
let constants_equal c1 c2 =
  match c1, c2 with
  | Parsing.Syntax_tree.ConstantInteger n1, Parsing.Syntax_tree.ConstantInteger n2 -> n1 = n2
  | Parsing.Syntax_tree.ConstantFloat f1, Parsing.Syntax_tree.ConstantFloat f2 -> f1 = f2
  | Parsing.Syntax_tree.ConstantString s1, Parsing.Syntax_tree.ConstantString s2 -> s1 = s2
  | Parsing.Syntax_tree.ConstantBoolean b1, Parsing.Syntax_tree.ConstantBoolean b2 -> b1 = b2
  | Parsing.Syntax_tree.ConstantUnit, Parsing.Syntax_tree.ConstantUnit -> true
  | _ -> false

(* Check if pattern is wildcard *)
let is_wildcard (pat : Typing.Typed_tree.typed_pattern) : bool =
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard -> true
  | _ -> false

(* Extend occurrence with a step *)
let extend_occurrence (occ : occurrence) (step : occurrence_step) : occurrence =
  occ @ [step]

(* Collect variable bindings from a pattern *)
let rec collect_bindings (pat : Typing.Typed_tree.typed_pattern) (occ : occurrence) : (Identifier.t * occurrence) list =
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable id -> [(id, occ)]
  | Typing.Typed_tree.TypedPatternWildcard -> []
  | Typing.Typed_tree.TypedPatternConstant _ -> []
  | Typing.Typed_tree.TypedPatternTuple pats ->
    List.concat (List.mapi (fun i p ->
      collect_bindings p (extend_occurrence occ (OccTupleField i))
    ) pats)
  | Typing.Typed_tree.TypedPatternConstructor (_, None) -> []
  | Typing.Typed_tree.TypedPatternConstructor (_, Some arg) ->
    collect_bindings arg (extend_occurrence occ OccConstructorArg)
  | Typing.Typed_tree.TypedPatternRecord (fields, _) ->
    List.concat (List.map (fun (f : Typing.Typed_tree.typed_record_pattern_field) ->
      collect_bindings f.typed_pattern_field_pattern
        (extend_occurrence occ (OccRecordField f.typed_pattern_field_name))
    ) fields)

(* Create initial matrix from match arms *)
let initial_matrix (arms : Typing.Typed_tree.typed_match_arm list) : pattern_matrix =
  let clauses = List.mapi (fun i (arm : Typing.Typed_tree.typed_match_arm) ->
    {
      clause_id = i;
      clause_patterns = [arm.typed_arm_pattern];
      clause_guard = arm.typed_arm_guard;
      clause_action = arm.typed_arm_expression;
      clause_bindings = [];
    }
  ) arms in
  { occurrences = [[]]; clauses }  (* Root occurrence is empty path *)

(* Get arity of a head constructor based on the pattern *)
let head_arity (head : head_constructor) : int =
  match head with
  | HCWildcard -> 0
  | HCConstant _ -> 0
  | HCTuple arity -> arity
  | HCConstructor _ -> 1  (* Constructors have at most 1 argument in Lina *)
  | HCRecord fields -> List.length fields

(* Create wildcard pattern with same type *)
let make_wildcard (ty : Typing.Types.type_expression) (loc : Location.t) : Typing.Typed_tree.typed_pattern =
  { pattern_desc = Typing.Typed_tree.TypedPatternWildcard;
    pattern_type = ty;
    pattern_location = loc }

(* Get sub-patterns when specializing - arity tells us how many sub-patterns to expect *)
let sub_patterns (pat : Typing.Typed_tree.typed_pattern) (head : head_constructor) (arity : int) : Typing.Typed_tree.typed_pattern list option =
  match pat.pattern_desc, head with
  (* Wildcards expand to n wildcards based on arity *)
  | (Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard), HCTuple _ ->
    Some (List.init arity (fun _ -> make_wildcard pat.pattern_type pat.pattern_location))
  | (Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard), HCConstructor _ ->
    Some (List.init arity (fun _ -> make_wildcard pat.pattern_type pat.pattern_location))
  | (Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard), HCConstant _ ->
    Some []
  | (Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard), HCRecord fields ->
    Some (List.map (fun _ -> make_wildcard pat.pattern_type pat.pattern_location) fields)
  | (Typing.Typed_tree.TypedPatternVariable _ | Typing.Typed_tree.TypedPatternWildcard), HCWildcard ->
    Some []

  (* Matching constructors *)
  | Typing.Typed_tree.TypedPatternTuple pats, HCTuple _ ->
    Some pats
  | Typing.Typed_tree.TypedPatternConstructor (ctor_info, arg), HCConstructor (name', _) when ctor_info.Typing.Types.constructor_name = name' ->
    begin match arg with
    | Some p -> Some [p]
    | None -> Some []
    end
  | Typing.Typed_tree.TypedPatternConstructor _, HCConstructor _ ->
    None  (* Different constructor *)
  | Typing.Typed_tree.TypedPatternConstant c, HCConstant c' when constants_equal c c' ->
    Some []
  | Typing.Typed_tree.TypedPatternConstant _, HCConstant _ ->
    None  (* Different constant *)
  | Typing.Typed_tree.TypedPatternRecord (fields, _), HCRecord expected_fields ->
    let sub_pats = List.map (fun name ->
      match List.find_opt (fun (f : Typing.Typed_tree.typed_record_pattern_field) ->
        f.typed_pattern_field_name = name
      ) fields with
      | Some f -> f.typed_pattern_field_pattern
      | None -> make_wildcard pat.pattern_type pat.pattern_location
    ) expected_fields in
    Some sub_pats
  | _ -> None

(* Compute new occurrences for sub-patterns *)
let sub_occurrences (occ : occurrence) (head : head_constructor) (arity : int) : occurrence list =
  match head with
  | HCWildcard -> []
  | HCConstant _ -> []
  | HCTuple _ ->
    List.init arity (fun i -> extend_occurrence occ (OccTupleField i))
  | HCConstructor _ ->
    if arity > 0 then [extend_occurrence occ OccConstructorArg]
    else []
  | HCRecord fields ->
    List.map (fun name -> extend_occurrence occ (OccRecordField name)) fields

(* Specialize matrix for a head constructor at given column with given arity *)
let specialize (matrix : pattern_matrix) (col : int) (head : head_constructor) (arity : int) : pattern_matrix =
  let occ = List.nth matrix.occurrences col in
  let new_sub_occs = sub_occurrences occ head arity in

  (* Replace column with expanded sub-occurrences *)
  let new_occurrences =
    let before = List.filteri (fun i _ -> i < col) matrix.occurrences in
    let after = List.filteri (fun i _ -> i > col) matrix.occurrences in
    before @ new_sub_occs @ after
  in

  (* Filter and transform clauses *)
  let new_clauses = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    match sub_patterns pat head arity with
    | None -> None  (* Clause doesn't match this constructor *)
    | Some sub_pats ->
      (* Collect bindings from this pattern *)
      let new_bindings = collect_bindings pat occ @ clause.clause_bindings in
      (* Replace pattern column with sub-patterns *)
      let before_pats = List.filteri (fun i _ -> i < col) clause.clause_patterns in
      let after_pats = List.filteri (fun i _ -> i > col) clause.clause_patterns in
      Some {
        clause with
        clause_patterns = before_pats @ sub_pats @ after_pats;
        clause_bindings = new_bindings;
      }
  ) matrix.clauses in

  { occurrences = new_occurrences; clauses = new_clauses }

(* Default matrix: keep rows where column is wildcard *)
let default_matrix (matrix : pattern_matrix) (col : int) : pattern_matrix =
  let occ = List.nth matrix.occurrences col in

  (* Remove the column from occurrences *)
  let new_occurrences =
    List.filteri (fun i _ -> i <> col) matrix.occurrences
  in

  (* Keep only rows with wildcard at column *)
  let new_clauses = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    if is_wildcard pat then
      let new_bindings = collect_bindings pat occ @ clause.clause_bindings in
      let new_patterns = List.filteri (fun i _ -> i <> col) clause.clause_patterns in
      Some { clause with
        clause_patterns = new_patterns;
        clause_bindings = new_bindings
      }
    else
      None
  ) matrix.clauses in

  { occurrences = new_occurrences; clauses = new_clauses }

(* Column selection: pick leftmost column where first row is not wildcard *)
let select_column (matrix : pattern_matrix) : int =
  match matrix.clauses with
  | [] -> 0
  | first_clause :: _ ->
    let rec find_needed idx patterns =
      match patterns with
      | [] -> 0  (* All wildcards - pick first *)
      | pat :: rest ->
        if not (is_wildcard pat) then idx
        else find_needed (idx + 1) rest
    in
    find_needed 0 first_clause.clause_patterns

(* Collect head constructors from a column *)
let collect_head_constructors (matrix : pattern_matrix) (col : int) : head_constructor list =
  let heads = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    let head = head_of_pattern pat in
    match head with
    | HCWildcard -> None
    | _ -> Some head
  ) matrix.clauses in
  (* Remove duplicates *)
  let rec unique acc = function
    | [] -> List.rev acc
    | x :: xs ->
      let eq h1 h2 = match h1, h2 with
        | HCConstructor (n1, _), HCConstructor (n2, _) -> n1 = n2
        | HCConstant c1, HCConstant c2 -> constants_equal c1 c2
        | HCTuple a1, HCTuple a2 -> a1 = a2
        | HCRecord f1, HCRecord f2 -> f1 = f2
        | _ -> false
      in
      if List.exists (eq x) acc then unique acc xs
      else unique (x :: acc) xs
  in
  unique [] heads

(* Adjust arity for constructor based on whether it has an arg in any pattern *)
let adjust_constructor_arity (matrix : pattern_matrix) (col : int) (head : head_constructor) : int =
  match head with
  | HCConstructor (name, _) ->
    (* Check if any pattern with this constructor has an argument *)
    let has_arg = List.exists (fun clause ->
      let pat = List.nth clause.clause_patterns col in
      match pat.pattern_desc with
      | Typing.Typed_tree.TypedPatternConstructor (ctor_info, Some _) when ctor_info.Typing.Types.constructor_name = name -> true
      | _ -> false
    ) matrix.clauses in
    if has_arg then 1 else 0
  | _ -> head_arity head

(* Main compilation function *)
let rec compile_matrix (matrix : pattern_matrix) : decision_tree =
  (* Base case 1: Empty matrix - match failure *)
  if matrix.clauses = [] then
    DTFail

  (* Base case 2: Empty pattern row - all patterns matched *)
  else if matrix.occurrences = [] then
    let clause = List.hd matrix.clauses in
    match clause.clause_guard with
    | None ->
      DTLeaf {
        leaf_bindings = clause.clause_bindings;
        leaf_action = clause.clause_action;
      }
    | Some guard ->
      DTGuard {
        guard_bindings = clause.clause_bindings;
        guard_condition = guard;
        guard_then = DTLeaf {
          leaf_bindings = clause.clause_bindings;
          leaf_action = clause.clause_action;
        };
        guard_else = compile_matrix { matrix with clauses = List.tl matrix.clauses };
      }

  (* Recursive case: select column and compile *)
  else
    let col = select_column matrix in
    let occ = List.nth matrix.occurrences col in
    let heads = collect_head_constructors matrix col in

    (* Build switch cases for each head constructor *)
    let cases = List.map (fun head ->
      let arity = adjust_constructor_arity matrix col head in
      let specialized = specialize matrix col head arity in
      (head, compile_matrix specialized)
    ) heads in

    (* Build default case *)
    let default_mat = default_matrix matrix col in
    let default =
      if default_mat.clauses = [] then None
      else Some (compile_matrix default_mat)
    in

    DTSwitch {
      switch_occurrence = occ;
      switch_cases = cases;
      switch_default = default;
    }

(* Compile match arms to decision tree *)
let compile_match (arms : Typing.Typed_tree.typed_match_arm list) : decision_tree =
  let matrix = initial_matrix arms in
  compile_matrix matrix
