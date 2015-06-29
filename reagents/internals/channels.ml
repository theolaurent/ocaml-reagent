

(* TODO: store sender rx in message to prevent getting twice *)
(* the same message in a reaction.                           *)
(* TODO: what is the expected behaviour when posting on both *)
(* sides of a channel?                                       *)
(* TODO: make messages passing and resuming building a       *)
(* reaction two dinstinct things, don't use offers.          *)
type ('a, 'b) message = { playload : 'a ; offer : 'b Offer.t }


let block_and_send f a =
  Offer.suspend (fun o -> f { playload = a ; offer = o })


let rx_answer m a =
  Reaction.add_pc (Reaction.add_abstract_cas Reaction.inert
                                             (Offer.complete_cas m.offer a Reaction.inert))
                  (Offer.wake m.offer)

(* TODO: lock free bags or at least concurrent queue instead of Queue.t *)
type ('a, 'b) channel = { comming_from_a : ('a, 'b) message Queue.t ;
                          comming_from_b : ('b, 'a) message Queue.t }

let flip_channel c = { comming_from_a = c.comming_from_b ;
                       comming_from_b = c.comming_from_a }

let new_channel () = { comming_from_a = Queue.create () ;
                       comming_from_b = Queue.create () }


let rec get_first_mesage q =
  if Queue.is_empty q then None
  else let m = Queue.pop q in
       ( if Offer.is_waiting m.offer then
           Some m
         else get_first_mesage q )


let post_a c =
  let buildReact arg k =
    match get_first_mesage c.comming_from_b with
    | None -> Queue.push { playload = arg ; offer = k } c.comming_from_a
                (* TODO: Concurrent data structure (with cas ?)    *)
    | Some m -> ( assert (Queue.is_empty c.comming_from_a) ;
                (* TODO: what behaviour do I want regarding order? *)
                  ignore (Offer.try_resume k { rx = rx_answer m arg ; result = m.playload}) )
  in { Reagent.buildReact = buildReact }


let channel_post (c:('a, 'b) channel) : (('a, 'b) Reagent.t * ('b, 'a) Reagent.t) =
  (post_a c, post_a (flip_channel c))
