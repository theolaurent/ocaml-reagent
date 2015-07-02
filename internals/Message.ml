
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

let send f =
  let withReact rx next offer =
    f (M { senderRx = rx ; senderK = next ; offer = offer })
  in Reagent { withReact }

let receive (M m) =
  let merge =
    let withReact rx next offer =
      (commit next).withReact ( rx >> Offer.rx_resume m.offer (Reaction.clear rx)
                                   >> m.senderRx )
                              (* The other reagent is given Reaction.inert,    *)
                              (* it is this one's role to enforce the whole    *)
                              (* reaction (i.e. both reactions and the         *)
                              (* message passing).                             *)
                              Nope
                              offer
    in Reagent { withReact }
  in m.senderK |> merge
