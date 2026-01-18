let edit_distance source target =
  let source_len = String.length source in
  let target_len = String.length target in

  if source_len = 0 then target_len
  else if target_len = 0 then source_len
  else if source = target then 0
  else begin
    let previous_row = Array.init (target_len + 1) Fun.id in
    let current_row = Array.make (target_len + 1) 0 in

    for source_idx = 1 to source_len do
      current_row.(0) <- source_idx;

      for target_idx = 1 to target_len do
        let cost =
          if source.[source_idx - 1] = target.[target_idx - 1] then 0 else 1
        in
        let deletion = previous_row.(target_idx) + 1 in
        let insertion = current_row.(target_idx - 1) + 1 in
        let substitution = previous_row.(target_idx - 1) + cost in

        current_row.(target_idx) <- min deletion (min insertion substitution)
      done;

      Array.blit current_row 0 previous_row 0 (target_len + 1)
    done;

    previous_row.(target_len)
  end

let suggestion_threshold target =
  let len = String.length target in
  if len <= 4 then 1
  else if len <= 6 then 2
  else 3

let is_prefix_match target candidate =
  let target_lower = String.lowercase_ascii target in
  let candidate_lower = String.lowercase_ascii candidate in
  let min_len = min (String.length target) (String.length candidate) in
  min_len >= 2 &&
  (String.sub target_lower 0 min_len = String.sub candidate_lower 0 min_len)

let length_diff_threshold target =
  suggestion_threshold target + 2

let within_threshold ~target ~candidate =
  let threshold = suggestion_threshold target in
  let len_diff = abs (String.length target - String.length candidate) in

  if len_diff > length_diff_threshold target then false
  else edit_distance target candidate <= threshold

let find_similar ~target ~candidates =
  let threshold = suggestion_threshold target in
  let len_threshold = length_diff_threshold target in

  candidates
  |> List.filter_map (fun candidate ->
       if candidate = target then None
       else
         let len_diff = abs (String.length target - String.length candidate) in
         if len_diff > len_threshold then None
         else
           let distance = edit_distance target candidate in
           (* Accept if within edit distance threshold OR if it's a prefix match *)
           if distance <= threshold then Some (candidate, distance)
           else if is_prefix_match target candidate then
             (* Prefix matches get a high distance so they sort after edit distance matches *)
             Some (candidate, distance)
           else None)
  |> List.sort (fun (_, distance1) (_, distance2) -> compare distance1 distance2)

let find_closest ~target ~candidates =
  match find_similar ~target ~candidates with
  | [] -> None
  | (closest, _) :: _ -> Some closest

let did_you_mean ~target ~candidates =
  match find_closest ~target ~candidates with
  | None -> None
  | Some closest -> Some (Printf.sprintf "Did you mean `%s`?" closest)

let format_suggestions ?(max_suggestions = 3) ~target ~candidates () =
  let similar = find_similar ~target ~candidates in

  match similar with
  | [] -> ""
  | [(single, _)] -> Printf.sprintf "Did you mean `%s`?" single
  | _ ->
    let limited = List.filteri (fun index _ -> index < max_suggestions) similar in
    let bullets =
      limited
      |> List.map (fun (name, _) -> Printf.sprintf "  - %s" name)
      |> String.concat "\n"
    in
    Printf.sprintf "Did you mean one of these?\n%s" bullets
