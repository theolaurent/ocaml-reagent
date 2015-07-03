
open Reagent.Sugar

(* TODO: what is the expected behaviour when posting on both *)
(* sides of a channel?                                       *)
(* TODO: has_offer *)

type ('a, 'b) endpoint = { outgoing : ('a, 'b) Reagent.message ConcurrentQueue.t ;
                           incoming : ('b, 'a) Reagent.message ConcurrentQueue.t }

let flip_endpoint e = { outgoing = e.incoming ;
                        incoming = e.outgoing }

let create () =
  let e1 = { outgoing = ConcurrentQueue.create () ;
             incoming = ConcurrentQueue.create () } in
  (e1, flip_endpoint e1)


let swap e =
  let () = Reagent.run (ConcurrentQueue.pop_until e.incoming
                                                  Reagent.is_message_available) () in
  let push_message m = Reagent.run (ConcurrentQueue.push e.outgoing) m in
  ( ConcurrentQueue.fold (fun m res -> Reagent.answer m || res) e.incoming Reagent.never
  || Reagent.send push_message )
