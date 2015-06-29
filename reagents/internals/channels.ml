
open CAS.Sugar
open Reaction.Sugar

(* TODO: store sender rx in message to prevent getting twice *)
(* the same message in a reaction.                           *)
(* TODO: what is the expected behaviour when posting on both *)
(* sides of a channel?                                       *)
(* TODO: make messages passing and resuming building a       *)
(* reaction two dinstinct things, don't use offers.          *)


type 'a message_status =
  | Waiting
  | Answered of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when   *)
(* the message has been answered.                            *)
(* Also, all answered offers should be waken at some point.  *)
type ('a, 'b) message = { playload : 'a                       ;
                          status   : 'b message_status casref ;
                          offer    : 'b Offer.t          }

let is_waiting m = match !(m.status) with
  | Waiting -> true
  | _       -> false

let block_and_send f a =
  Offer.suspend (fun k -> f { playload = a ;
                              status = ref Waiting ;
                              offer = k })


let wake m () =
  let s = !(m.status) in
  match s with
  | Answered v when (m.status <!= s --> WakedUp)
      -> assert (Offer.try_resume m.offer (return v))
  | _ -> failwith "Offer.wake: trying to wake a non-completed offer"

let rx_answer m a =
  Reaction.add_pc (Reaction.add_abstract_cas
                     Reaction.inert
                     (m.status <:= Waiting --> Answered a))
                  (wake m)

(* TODO: lock free bags ; and bootstrap with reagents *)
type ('a, 'b) channel = { comming_from_a : ('a, 'b) message BasicConcurrentQueue.t ;
                          comming_from_b : ('b, 'a) message BasicConcurrentQueue.t }

let flip_channel c = { comming_from_a = c.comming_from_b ;
                       comming_from_b = c.comming_from_a }

let new_channel () = { comming_from_a = BasicConcurrentQueue.create () ;
                       comming_from_b = BasicConcurrentQueue.create () }


let rec get_first_mesage q =
  match BasicConcurrentQueue.try_pop q with
  | None -> None (* TODO: use an option monad lib *)
  | Some m -> ( if is_waiting m then
                  Some m
                else get_first_mesage q )


let post_a c =
  let buildReact arg k =
    match get_first_mesage c.comming_from_b with
    | None -> BasicConcurrentQueue.push { playload = arg ; status = ref Waiting ; offer = k }
                         c.comming_from_a
                (* TODO: Concurrent data structure (with cas?) *)
    | Some m -> ( assert (BasicConcurrentQueue.is_empty c.comming_from_a) ;
                (* TODO: what behaviour do I want regarding order?           *)
                (* TODO: what about this reaction merge in the scala verion? *)
                  ignore (Offer.try_resume k { rx = rx_answer m arg ; result = m.playload }) )
  in { Reagent.buildReact = buildReact }


let channel_post (c:('a, 'b) channel) : (('a, 'b) Reagent.t * ('b, 'a) Reagent.t) =
  (post_a c, post_a (flip_channel c))
