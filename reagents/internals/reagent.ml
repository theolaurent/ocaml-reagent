
type 'b result =
  | SuspendWithOffer of ('b Offer.t -> unit)
  | Immediate of 'b

type ('a, 'b) t = {
    react : 'a -> Reaction.t -> 'b result ;
    composeI : 'c . ('b, 'c) t -> ('a, 'c) t ;
  }

(* for the moment just try without and then with offer; cf scala implem (canSync...) *)
let tryReact r a rx offer = match r.react a rx with
  | Immediate x -> Some x
  | SuspendWithOffer f -> begin match offer with
                                | Some o -> f o ; None (* This can be confusing,        *)
                                                       (* this None shall never be used *)
                                | None -> None
                          end

let compose r1 r2 = r1.composeI r2

let run r a =
  match tryReact r a Reaction.inert None with
  (* for now, no retry, cf scala code *)
  | None -> perform (Sched.Suspend (fun k -> ignore (tryReact r a Reaction.inert (Some (Offer.make k)))))
  | Some x -> x




(* moved from offer.ml to avoid circular dependencies *)
let comsume_and_continue o complete_with continue_with rx k enclosing_offer =
  (* forgetting the immediate CAS for now; cf scala implem *)
  let new_rx = Reaction.add_pc (Offer.rx_with_completion o rx complete_with)
                               (Offer.wake o) in
  (* for now, only blocking ones; cf scala implem *)
  tryReact k continue_with new_rx enclosing_offer
