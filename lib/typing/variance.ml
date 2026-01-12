(** Variance operations for type parameters.

    This module provides operations for manipulating type parameter variances.
    See {!module:Variance.mli} for detailed documentation. *)

type t = Types.variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

let flip = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant
  | Bivariant -> Bivariant

let combine variance1 variance2 =
  match variance1, variance2 with
  (* Bivariant is the identity for combination *)
  | Bivariant, other | other, Bivariant -> other
  (* Same variance stays the same *)
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  (* Invariant absorbs everything *)
  | Invariant, _ | _, Invariant -> Invariant
  (* Conflicting variances become invariant *)
  | Covariant, Contravariant | Contravariant, Covariant -> Invariant

let compose context position =
  match context with
  | Covariant -> position
  | Contravariant -> flip position
  | Invariant -> Invariant
  | Bivariant -> Bivariant

let compatible ~impl ~decl =
  match decl, impl with
  (* Bivariant declaration accepts anything (phantom type parameter) *)
  | Bivariant, _ -> true
  (* Bivariant implementation means parameter is unused, compatible with anything *)
  | _, Bivariant -> true
  (* Same variance is compatible *)
  | Covariant, Covariant -> true
  | Contravariant, Contravariant -> true
  (* Invariant implementation is compatible with any declaration
     (impl is more restrictive) *)
  | _, Invariant -> true
  (* Covariant/Contravariant impl is NOT compatible with Invariant decl
     (impl is less restrictive) *)
  | Invariant, Covariant -> false
  | Invariant, Contravariant -> false
  (* Conflicting variances are incompatible *)
  | Covariant, Contravariant -> false
  | Contravariant, Covariant -> false

let to_string = function
  | Covariant -> "+"
  | Contravariant -> "-"
  | Invariant -> ""
  | Bivariant -> "_"

let pp fmt variance =
  Format.fprintf fmt "%s" (to_string variance)

let to_annotation = function
  | Covariant -> "+"
  | Contravariant -> "-"
  | Invariant -> "" (* No annotation means invariant *)
  | Bivariant -> "_" (* Underscore for unused/phantom *)
