(** Context-threading fold utilities.

    See {!Context_fold} for documentation. *)

let fold_map f ctx list =
  let rec loop acc ctx = function
    | [] -> (List.rev acc, ctx)
    | x :: xs ->
      let y, ctx = f ctx x in
      loop (y :: acc) ctx xs
  in
  loop [] ctx list

let fold_map_rev f ctx list =
  List.fold_left (fun (acc, ctx) x ->
    let y, ctx = f ctx x in
    (y :: acc, ctx)
  ) ([], ctx) list

let fold_filter_map f ctx list =
  let rec loop acc ctx = function
    | [] -> (List.rev acc, ctx)
    | x :: xs ->
      let y_opt, ctx = f ctx x in
      let acc = match y_opt with
        | Some y -> y :: acc
        | None -> acc
      in
      loop acc ctx xs
  in
  loop [] ctx list

let fold_map2 f ctx list =
  let rec loop acc1 acc2 ctx = function
    | [] -> (List.rev acc1, List.rev acc2, ctx)
    | x :: xs ->
      let y, z, ctx = f ctx x in
      loop (y :: acc1) (z :: acc2) ctx xs
  in
  loop [] [] ctx list

let fold_map3 f ctx list =
  let rec loop acc1 acc2 acc3 ctx = function
    | [] -> (List.rev acc1, List.rev acc2, List.rev acc3, ctx)
    | x :: xs ->
      let y, z, w, ctx = f ctx x in
      loop (y :: acc1) (z :: acc2) (w :: acc3) ctx xs
  in
  loop [] [] [] ctx list

let fold_mapi f ctx list =
  let rec loop acc index ctx = function
    | [] -> (List.rev acc, ctx)
    | x :: xs ->
      let y, ctx = f ctx index x in
      loop (y :: acc) (index + 1) ctx xs
  in
  loop [] 0 ctx list

let fold_ctx f ctx list =
  List.fold_left f ctx list

let iter_ctx f ctx list =
  List.iter (f ctx) list
