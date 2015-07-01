
open Reaction.Sugar
open Reagent.Sugar

(* TODO: prevent getting twice the same message in a reaction. *)

type ('a, 'b, 'r) t_struct = { playload : 'a                 ;
                               senderRx : Reaction.t         ;
                               senderK  : ('b, 'r) Reagent.t ;
                               offer    : 'r Offer.t         }
type (_, _) t =
  M : ('a, 'b, 'r) t_struct -> ('a, 'b) t

let is_available (M m) =
  Offer.is_waiting m.offer

let send f =
  let tryReact arg rx next offer =
    f (M { playload = arg ; senderRx = rx ; senderK = next ; offer = offer })
  in { Reagent.tryReact = tryReact ; isCommit = false }

let receive (M m) =
  let merge =
    let tryReact arg rx next offer =
      next.Reagent.tryReact m.playload
                            (m.senderRx ++ rx ++ Reaction.c_offer m.offer arg)
                            Reagent.commit
                            offer
    in { Reagent.tryReact = tryReact ; isCommit = false }
  in m.senderK >> merge
