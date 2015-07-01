
open Reagent.Sugar

let() = Sched.run (fun () ->
  let c1 = Reagent.lift (fun x -> List.length x > 0) in
  let c2 = Reagent.lift (fun y -> Printf.sprintf "%B" y) in
  let c3 = Reagent.lift (fun l -> String.concat " " (List.map string_of_int l)) in
  let c4 = Reagent.lift (fun (x, y) -> "(" ^ x ^ "," ^ y ^ ")") in
  print_endline @@ Reagent.run (((Reagent.never + (c1 >> c2)) * c3) >> c4) [1;2;3] ;
                  )
