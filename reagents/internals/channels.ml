
open CAS.Sugar
open Reagent


(* TODO: I don't think this reaction exchange ... ; cf scala code *)

(* TODO: "non blocking bags" instead of queues ; cf scala code *)
type ('a, 'b) channel_status =
  | Incoming of ('b, 'a) Offer.t Queue.t
  | Outgoing of ('a, 'b) Offer.t Queue.t

type ('a, 'b) channel = ('a, 'b) channel_status casref

(* let rec first_offer q = *)
(*   if Queue.empty q then None *)
(*   else let o = Queue.pop q in *)
(*        match !(o.state) with *)
(*        | Waiting a when  *)
(*  *)
(*  *)
(* (\* TODO: is it possible to write swap as a composition of read/cas ? *\) *)
(* let swap (c:('a, 'b) channel) : ('a, 'b) t = *)
(*   let buildReact () k = *)
(*     let state = !c in *)
(*     match state with *)
(*     | Incoming q -> *)
(*  *)
(*     let rx = Reaction.add_cas Reaction.inert r *)
(*                               ~expect ~update *)
(*     in return_react k rx () *)
(*   in { buildReact } *)
