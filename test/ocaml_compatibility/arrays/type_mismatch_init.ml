(** Test: Type mismatch in Array.init function
    Expected: REJECT

    Init function must return consistent type. *)

let arr : int array = Array.init 3 (fun i -> "not an int")
