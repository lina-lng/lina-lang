(* Loops: nested loops *)
(* Expected: ACCEPT - multiple levels of nesting *)

let count = ref 0

let _ =
  for i = 1 to 3 do
    for j = 1 to 3 do
      for k = 1 to 3 do
        count := !count + 1
      done
    done
  done

let _ = print_int !count
