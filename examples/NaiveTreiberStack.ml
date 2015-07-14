
open CAS.Sugar
open Reagent.Sugar

type 'a t = { head : 'a list casref }

let push stack =
  Reagent.computed (fun v ->
    let s = !(stack.head) in
    Reagent.cas stack.head (s --> (v :: s))
  )

let tryPop stack =
  Reagent.computed (fun () ->
    let s = !(stack.head) in match s with
      | [] -> Reagent.constant None
      | h :: t -> Reagent.cas stack.head (s --> t)
              >>> Reagent.constant (Some h)
  )
