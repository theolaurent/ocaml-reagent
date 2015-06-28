
open CAS.Sugar
open Reagent

(* hmmm... I don't use offer there ... ; TODO: think about that ...*)


let read (r:'a casref) : (unit, 'a) t =
  let buildReact () k =
    Conthread.try_fulfill_and_wake_ignore
      k (Reaction.inert, !r)
  in { buildReact }


(* TODO: optimise when only one CAS ? cf scala code *)
(* TODO: what about CAS built up on the fly ? *)
let cas (r:'a casref) ~(expect:'a) ~(update:'a) : (unit, unit) t =
  let buildReact () k =
    let rx = Reaction.add_cas Reaction.inert r
                              ~expect ~update
    in Conthread.try_fulfill_and_wake_ignore
         k (rx, ())
  in { buildReact }
