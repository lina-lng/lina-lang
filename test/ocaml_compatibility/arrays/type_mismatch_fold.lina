(** Test: Type mismatch in fold accumulator
    Expected: REJECT

    Fold function must return same type as accumulator. *)

let arr = Array.make 3 1
let result = Array.fold_left (fun acc x -> "wrong") 0 arr
