

type ('a, 'b) t = {
    tryReact : 'a -> Reaction.t -> 'b option ;
    suspendWithOffer : 'a -> Reaction.t -> 'b Offer.t -> unit  ;
    composeI : 'c . ('b, 'c) t -> ('a, 'c) t ;
  }

let run r a =
  match r.tryReact a Reaction.inert with
  (* for now, no retry, cf scala code *)
  | None -> perform (Sched.Suspend (fun k -> r.suspendWithOffer a Reaction.inert (Offer.make k)))
  | Some x -> x




(* from offer.ml to avoid circular dependencies *)
let comsume_and_continue o complete_with continue_with rx k enclosing_offer =
  (* forgetting the immediate CAS for now; cf scala implem *)
  let new_rx = Reaction.add_pc (Offer.rx_with_completion o rx complete_with)
                               (Offer.wake o) in
  (* for now, only blocking ones; cf scala implem *)
  k.tryReact continue_with new_rx (*enclosing_offer*) (* TODO: mmmmhhh ... How to pass the offer ? *)
