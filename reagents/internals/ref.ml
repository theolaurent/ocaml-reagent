
open CAS.Sugar
open Reagent

(* hmmm... I don't use offer there ... ; TODO: think about that ...*)


let read (r:'a casref) : (unit, 'a) t =
  let buildReact () k =
    Conthread.resume k (Reaction.inert, !r)
  in { buildReact }


(* TODO: optimise when only one CAS ? cf scala code *)
let cas (r:'a casref) ~(expect:'a) ~(update:'a) : (unit, unit) t =
  let buildReact () k =
    let rx = Reaction.add_cas Reaction.inert r
                              ~expect ~update
    in Conthread.resume k (rx, ())
  in { buildReact }


(* TODO: Hmm but the actual cas will not be performed until the commit phase ..   *)
(* Thus, to cas piped are bound to fail? (the problem being cas >> read is false! *)
(* I think this version behave as the scala code thought  ..                     *)



(* let onthefly_cas (r:'a casref) : ('a casref_update, unit) t = *)
(*   let buildReact { expect ; update } k = *)
(*     let rx = Reaction.add_cas Reaction.inert r *)
(*                               ~expect ~update *)
(*     in Conthread.resume k (rx, ()) *)
(*   in { buildReact } *)
