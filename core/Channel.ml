
(* TODO: what is the expected behaviour when posting on both   *)
(* sides of a channel?                                         *)

(* TODO: lock free bags ; what about bootstrap with reagents? *)
(* TODO: and then multiple message answer via choose          *)
(*       also posting an offer on the channel ??              *)
type ('a, 'b) endpoint = { outgoing : ('a, 'b) Message.t BasicConcurrentQueue.t ;
                           incoming : ('b, 'a) Message.t BasicConcurrentQueue.t }

let flip_endpoint e = { outgoing = e.incoming ;
                        incoming = e.outgoing }

let create () =
  let e1 = { outgoing = BasicConcurrentQueue.create () ;
             incoming = BasicConcurrentQueue.create () } in
  (e1, flip_endpoint e1)


let rec get_first_mesage q =
  match BasicConcurrentQueue.try_pop q with
  | None -> None (* TODO: use an option monad library *)
  | Some m -> ( if Message.is_available m then
                  Some m (* TODO: this is not correct ; should put the offer back *)
                else get_first_mesage q )


let swap e = match get_first_mesage e.incoming with
  | None -> Message.send ( fun m -> BasicConcurrentQueue.push m e.outgoing )
  | Some m -> (* assert ( BasicConcurrentQueue.is_empty e.outgoing ) *)
              (* TODO: what behaviour do I want regarding order?     *)
              Message.receive m
