open Common
open Types

exception Unification_error of {
  expected : type_expression;
  actual : type_expression;
  location : Location.t;
  message : string;
}

let unification_error location expected actual message =
  raise (Unification_error { expected; actual; location; message })

let rec occurs_check location tv ty =
  match representative ty with
  | TypeVariable tv' ->
    if tv.id = tv'.id then
      unification_error location
        (TypeVariable tv) ty
        "Infinite type: type variable occurs in its own definition"
    else
      if tv'.level > tv.level then tv'.level <- tv.level
  | TypeConstructor (_, args) ->
    List.iter (occurs_check location tv) args
  | TypeTuple elements ->
    List.iter (occurs_check location tv) elements
  | TypeArrow (arg, result) ->
    occurs_check location tv arg;
    occurs_check location tv result

let rec unify location ty1 ty2 =
  let ty1 = representative ty1 in
  let ty2 = representative ty2 in
  if ty1 == ty2 then ()
  else match ty1, ty2 with
  | TypeVariable tv1, TypeVariable tv2 ->
    if tv1.level < tv2.level then
      tv2.link <- Some ty1
    else
      tv1.link <- Some ty2

  | TypeVariable tv, ty | ty, TypeVariable tv ->
    occurs_check location tv ty;
    tv.link <- Some ty

  | TypeConstructor (path1, args1), TypeConstructor (path2, args2) ->
    if path1 <> path2 then
      unification_error location ty1 ty2
        (Printf.sprintf "Type mismatch: expected %s, got %s"
          (type_expression_to_string ty1)
          (type_expression_to_string ty2));
    if List.length args1 <> List.length args2 then
      unification_error location ty1 ty2
        "Type constructor arity mismatch";
    List.iter2 (unify location) args1 args2

  | TypeTuple elems1, TypeTuple elems2 ->
    if List.length elems1 <> List.length elems2 then
      unification_error location ty1 ty2
        (Printf.sprintf "Tuple size mismatch: expected %d elements, got %d"
          (List.length elems1) (List.length elems2));
    List.iter2 (unify location) elems1 elems2

  | TypeArrow (arg1, res1), TypeArrow (arg2, res2) ->
    unify location arg1 arg2;
    unify location res1 res2

  | _ ->
    unification_error location ty1 ty2
      (Printf.sprintf "Type mismatch: expected %s, got %s"
        (type_expression_to_string ty1)
        (type_expression_to_string ty2))
