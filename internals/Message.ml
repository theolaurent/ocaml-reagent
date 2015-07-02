
open Reaction.Sugar
open Reagent.Sugar
open Reagent

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
  let withReact arg rx next offer =
    f (M { playload = arg ; senderRx = rx ; senderK = next ; offer = offer })
  in Some { withReact }

let receive (M m) =
  let merge =
    let withReact arg rx next offer =
      (commit next).withReact m.playload
                              (m.senderRx ++ rx ++ Reaction.c_offer m.offer arg)
                              None
                              offer
    in Some { withReact }
  in m.senderK >> merge
