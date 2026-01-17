open Common
open Typing.Typed_tree

type segment = {
  id : int;
  mutable reachable : bool;
  location : Location.t;
  mutable successors : int list;
  mutable predecessors : int list;
  is_diverging : bool;
}

type t = {
  entry : int;
  exit : int list;
  segments : (int, segment) Hashtbl.t;
}

let add_edge from_seg to_seg =
  from_seg.successors <- to_seg.id :: from_seg.successors;
  to_seg.predecessors <- from_seg.id :: to_seg.predecessors

let build_expr fresh_id segments (expr : typed_expression) =
  let create_segment ?(diverging = false) location =
    {
      id = fresh_id ();
      reachable = false;
      location;
      successors = [];
      predecessors = [];
      is_diverging = diverging;
    }
  in

  let rec go (expr : typed_expression) : segment * segment list =
    let entry = create_segment expr.expression_location in
    Hashtbl.add segments entry.id entry;

    match expr.expression_desc with
    | TypedExpressionVariable _ | TypedExpressionConstant _
    | TypedExpressionTuple _ | TypedExpressionConstructor _
    | TypedExpressionRecord _ | TypedExpressionRecordAccess _
    | TypedExpressionRecordUpdate _ | TypedExpressionRef _
    | TypedExpressionDeref _ | TypedExpressionAssign _
    | TypedExpressionModuleAccess _ | TypedExpressionPolyVariant _
    | TypedExpressionPack _ | TypedExpressionPartialApply _
    | TypedExpressionError _ ->
        (entry, [ entry ])

    | TypedExpressionApply _ ->
        if Divergence.is_diverging_call expr then (
          let diverge = create_segment ~diverging:true expr.expression_location in
          Hashtbl.add segments diverge.id diverge;
          add_edge entry diverge;
          (entry, []))
        else (entry, [ entry ])

    | TypedExpressionFunction (_, body) ->
        let _body_entry, _body_exits = go body in
        (entry, [ entry ])

    | TypedExpressionLet (_, bindings, body) ->
        let current = ref entry in

        List.iter
          (fun binding ->
            let bind_entry, bind_exits = go binding.binding_expression in
            add_edge !current bind_entry;
            match bind_exits with
            | [ exit ] -> current := exit
            | exits ->
                let join = create_segment binding.binding_location in
                Hashtbl.add segments join.id join;
                List.iter (fun e -> add_edge e join) exits;
                current := join)
          bindings;

        let body_entry, body_exits = go body in
        add_edge !current body_entry;
        (entry, body_exits)

    | TypedExpressionIf (cond, then_expr, else_opt) ->
        let cond_entry, cond_exits = go cond in
        add_edge entry cond_entry;

        let then_entry, then_exits = go then_expr in
        List.iter (fun e -> add_edge e then_entry) cond_exits;

        let else_exits =
          match else_opt with
          | Some else_expr ->
              let else_entry, else_exits = go else_expr in
              List.iter (fun e -> add_edge e else_entry) cond_exits;
              else_exits
          | None -> cond_exits
        in

        (entry, then_exits @ else_exits)

    | TypedExpressionSequence (left, right) ->
        let left_entry, left_exits = go left in
        add_edge entry left_entry;

        let right_entry, right_exits = go right in
        List.iter (fun e -> add_edge e right_entry) left_exits;

        (entry, right_exits)

    | TypedExpressionMatch (scrutinee, arms) ->
        let scrut_entry, scrut_exits = go scrutinee in
        add_edge entry scrut_entry;

        let all_exits =
          List.concat_map
            (fun (arm : typed_match_arm) ->
              let arm_entry, arm_exits = go arm.typed_arm_expression in
              List.iter (fun e -> add_edge e arm_entry) scrut_exits;
              arm_exits)
            arms
        in

        (entry, all_exits)

    | TypedExpressionAssert inner ->
        let inner_entry, inner_exits = go inner in
        add_edge entry inner_entry;
        (entry, inner_exits)

    | TypedExpressionWhile (cond, body) ->
        let cond_entry, cond_exits = go cond in
        add_edge entry cond_entry;

        let body_entry, body_exits = go body in
        List.iter (fun e -> add_edge e body_entry) cond_exits;
        List.iter (fun e -> add_edge e cond_entry) body_exits;

        (entry, cond_exits)

    | TypedExpressionFor (_, start, finish, _, body) ->
        let start_entry, start_exits = go start in
        add_edge entry start_entry;

        let finish_entry, finish_exits = go finish in
        List.iter (fun e -> add_edge e finish_entry) start_exits;

        let body_entry, body_exits = go body in
        List.iter (fun e -> add_edge e body_entry) finish_exits;

        (entry, body_exits)

    | TypedExpressionLetModule (_, _mod_expr, body) ->
        let body_entry, body_exits = go body in
        add_edge entry body_entry;
        (entry, body_exits)
  in

  go expr

let build expr =
  let next_id = ref 0 in
  let fresh_id () =
    let id = !next_id in
    incr next_id;
    id
  in

  let segments = Hashtbl.create 32 in
  let entry, exits = build_expr fresh_id segments expr in
  { entry = entry.id; exit = List.map (fun s -> s.id) exits; segments }

let mark_reachable cfg =
  let visited = Hashtbl.create 16 in

  let rec visit seg_id =
    if not (Hashtbl.mem visited seg_id) then (
      Hashtbl.add visited seg_id ();
      match Hashtbl.find_opt cfg.segments seg_id with
      | Some seg ->
          seg.reachable <- true;
          if not seg.is_diverging then List.iter visit seg.successors
      | None -> ())
  in

  visit cfg.entry

let find_unreachable cfg =
  mark_reachable cfg;

  Hashtbl.fold
    (fun _id seg acc -> if not seg.reachable then seg :: acc else acc)
    cfg.segments []
