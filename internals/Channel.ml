
(* TODO: what is the expected behaviour when posting on both   *)
(* sides of a channel?                                         *)

(* TODO: lock free bags ; what about bootstrap with reagents? *)
(* TODO: and then multiple message answer via choose          *)
(*       also posting an offer on the channel ??              *)
type ('a, 'b) t = { a : ('a, 'b) Message.t BasicConcurrentQueue.t ;
                    b : ('b, 'a) Message.t BasicConcurrentQueue.t }

let flip_channel c = { a = c.b ;
                       b = c.a }

let new_channel () = { a = BasicConcurrentQueue.create () ;
                       b = BasicConcurrentQueue.create () }


let rec get_first_mesage q =
  match BasicConcurrentQueue.try_pop q with
  | None -> None (* TODO: use an option monad library *)
  | Some m -> ( if Message.is_available m then
                  Some m
                else get_first_mesage q )


let post_a c = match get_first_mesage c.b with
  | None -> Message.send ( fun m -> BasicConcurrentQueue.push m c.a )
  | Some m -> ( assert (BasicConcurrentQueue.is_empty c.a) ;
                Message.receive m )
                (* TODO: what behaviour do I want regarding order?           *)

let channel_post (c:('a, 'b) t) : (('a, 'b) Reagent.t * ('b, 'a) Reagent.t) =
  (post_a c, post_a (flip_channel c))
