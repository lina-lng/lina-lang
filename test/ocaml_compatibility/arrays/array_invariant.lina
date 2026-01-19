(** Test: Array type is invariant
    Expected: REJECT

    Arrays are mutable, so 'a array is invariant in 'a.
    A string array cannot be used where an 'a array is expected
    in a mutable context. *)

let f (arr : string array) : unit =
  let r : string array ref = ref arr in
  ()

type animal = Dog | Cat
type dog = Dog

let use_animal_array (arr : animal array) = Array.length arr

let dog_arr : dog array = Array.make 1 Dog

let _ = use_animal_array dog_arr
