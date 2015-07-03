
open Reaction.Sugar
open Reagent.Sugar
open Reagent

(* TODO: prevent getting twice the same message in a reaction. *)

type ('a, 'b, 'r) t_struct = { senderRx : 'a Reaction.t      ;
                               senderK  : ('b, 'r) Reagent.t ;
                               offer    : 'r Offer.t         }
type (_, _) t =
  M : ('a, 'b, 'r) t_struct -> ('a, 'b) t

let is_available (M m) =
  Offer.is_waiting m.offer

(* send is always blocking, it is to be used together with choose/atempt *)
let send f =
  let withReact rx next =
    WithOffer (fun offer -> f (M { senderRx = rx ; senderK = next ; offer = offer }))
  in Reagent { withReact }

let answer (M m) =
  let merge =
    let withReact rx next =
      (commit next).withReact ( rx >> Offer.rx_resume m.offer (Reaction.clear rx)
                                   >> m.senderRx )
                              (* The other reagent is given Reaction.inert,    *)
                              (* it is this one's role to enforce the whole    *)
                              (* reaction (i.e. both reactions and the         *)
                              (* message passing).                             *)
                              Nope
    in Reagent { withReact }
  in m.senderK |> merge
