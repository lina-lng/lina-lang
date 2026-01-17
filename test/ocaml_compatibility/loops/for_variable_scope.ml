(* For loop: variable scope *)
(* Expected: REJECT - loop variable not accessible outside *)

let _ =
  for i = 1 to 5 do
    ()
  done

let x = i
